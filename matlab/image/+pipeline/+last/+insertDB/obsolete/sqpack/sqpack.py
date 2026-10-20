#!/usr/bin/env python3
"""
sqpack.py - pack leaf data folders into SquashFS images, in place.

Layout handled (depth is configurable, default 4):

    ROOT/w2026/06/01/<visit>/        ~500 small files + status file(s)

After packing one leaf:

    ROOT/w2026/06/01/<visit>.sqfs    read-only image of the data files
    ROOT/w2026/06/01/<visit>  ->     /sq/w2026_06_01_<visit>   (symlink, autofs mounts on access)
    ROOT/w2026/06/01/<visit>.meta/   writable dir with the excluded files (e.g. status.txt)

Existing code keeps reading ROOT/w2026/06/01/<visit>/x.fits unchanged.
Status files are read/written at ROOT/w2026/06/01/<visit>.meta/<name>.

Commands
    sqpack.py pack    --root /data [--jobs 32] [--exclude 'status*'] [--dry-run] ...
    sqpack.py restore --root /data  PATH_TO_LEAF [PATH_TO_LEAF ...]
    sqpack.py key     --root /data  PATH_TO_LEAF      (print the autofs key)

Safety
    * Each image is written to <visit>.sqfs.tmp, verified (default: byte-for-byte
      against the originals), then renamed into place.
    * The original folder is renamed to .<visit>.packold, the symlink is swapped in
      atomically, and only then is .<visit>.packold deleted (or kept with
      --keep-originals).
    * Re-running is safe: packed leaves are symlinks and are skipped; interrupted
      leaves are redone; leftover .packold dirs are cleaned up.
    * Only pack folders the pipeline has finished writing (see --min-age-min).

Requires: squashfs-tools >= 4.5 (mksquashfs, unsquashfs), Python >= 3.8.
"""

import argparse
import concurrent.futures as cf
import filecmp
import fnmatch
import json
import os
import shutil
import subprocess
import sys
import tempfile
import threading
import time

META_SUFFIX = ".meta"
OLD_FMT = ".{}.packold"
TMP_IMG_SUFFIX = ".sqfs.tmp"
KEY_SEP = "_"

_log_lock = threading.Lock()


# --------------------------------------------------------------------------- helpers

def key_for(root, leaf, depth):
    """w2026/06/01/visit -> 'w2026_06_01_visit'. Only the LAST component may contain '_'."""
    rel = os.path.relpath(leaf, root)
    parts = rel.split(os.sep)
    if len(parts) != depth:
        raise ValueError(f"{leaf}: expected {depth} levels below root, got {len(parts)}")
    for p in parts[:-1]:
        if KEY_SEP in p:
            raise ValueError(f"{leaf}: '{p}' contains '{KEY_SEP}' (only the last level may)")
    return KEY_SEP.join(parts)


def iter_leaves(root, depth):
    """Yield (kind, path) for real directories exactly `depth` levels below root.

    kind is 'leaf' for an unpacked folder or 'old' for a leftover .packold dir.
    Uses scandir and never follows symlinks, so packed leaves do not trigger automounts.
    """
    def walk(path, level):
        try:
            entries = sorted(os.scandir(path), key=lambda e: e.name)
        except OSError as e:
            print(f"WARN cannot list {path}: {e}", file=sys.stderr)
            return
        for e in entries:
            try:
                if not e.is_dir(follow_symlinks=False):
                    continue
            except OSError:
                continue
            if level + 1 < depth:
                if not e.name.startswith("."):
                    yield from walk(e.path, level + 1)
            else:
                if e.name.startswith(".") and e.name.endswith(".packold"):
                    yield "old", e.path
                elif not e.name.startswith(".") and not e.name.endswith(META_SUFFIX):
                    yield "leaf", e.path
    yield from walk(root, 0)


def is_excluded(name, patterns):
    return any(fnmatch.fnmatchcase(name, p) for p in patterns)


def list_tree(top, exclude=()):
    """Relative paths of regular files and symlinks under top (top-level excludes honoured)."""
    out = {}
    for dp, dns, fns in os.walk(top):
        rel_dir = os.path.relpath(dp, top)
        if rel_dir == ".":
            dns[:] = [d for d in dns if not is_excluded(d, exclude)]
            fns = [f for f in fns if not is_excluded(f, exclude)]
        for f in fns:
            p = os.path.join(dp, f)
            rel = os.path.normpath(os.path.join(rel_dir, f))
            st = os.lstat(p)
            out[rel] = st.st_size
        for d in dns:
            rel = os.path.normpath(os.path.join(rel_dir, d))
            out[rel + "/"] = -1
    return out


def run(cmd):
    r = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if r.returncode != 0:
        raise RuntimeError(f"{cmd[0]} failed ({r.returncode}): {r.stderr.strip()[-500:]}")
    return r.stdout


def log(fh, rec):
    rec["t"] = time.strftime("%Y-%m-%dT%H:%M:%S")
    line = json.dumps(rec)
    with _log_lock:
        if fh:
            fh.write(line + "\n")
            fh.flush()
        if rec.get("result") not in ("ok", "skip", "dry-run", "cleaned"):
            print(line, file=sys.stderr)


# --------------------------------------------------------------------------- pack one leaf

def verify(img, leaf, exclude, mode, tmp_root):
    """Check the image against the original folder. Returns number of files checked."""
    orig = list_tree(leaf, exclude)
    if mode == "list":
        # compare names and sizes from the image listing (no extraction)
        out = run(["unsquashfs", "-lls", "-n", img])
        got = {}
        prefix = "squashfs-root/"
        for line in out.splitlines():
            if prefix not in line:
                continue
            fields = line.split(None, 5)
            if len(fields) < 6:
                continue
            path = line[line.index(prefix) + len(prefix):]
            if " -> " in path:
                path = path.split(" -> ")[0]
            if fields[0].startswith("d"):
                got[path + "/"] = -1
            elif fields[0].startswith("-"):
                got[path] = int(fields[2])
            else:
                got[path] = orig.get(path, -2)  # symlinks/devices: presence only
        if got != orig:
            missing = sorted(set(orig) - set(got))[:5]
            extra = sorted(set(got) - set(orig))[:5]
            raise RuntimeError(f"verify(list) mismatch: missing={missing} extra={extra}")
        return len(orig)

    # full: extract to tmp (RAM-backed by default) and compare byte for byte
    tdir = tempfile.mkdtemp(prefix="sqv_", dir=tmp_root)
    try:
        dest = os.path.join(tdir, "x")
        run(["unsquashfs", "-n", "-q", "-no-xattrs", "-d", dest, img])
        got = list_tree(dest)
        if set(got) != set(orig):
            missing = sorted(set(orig) - set(got))[:5]
            extra = sorted(set(got) - set(orig))[:5]
            raise RuntimeError(f"verify(full) file set mismatch: missing={missing} extra={extra}")
        for rel, size in orig.items():
            if size < 0:
                continue
            a, b = os.path.join(leaf, rel), os.path.join(dest, rel)
            if os.path.islink(a):
                if os.readlink(a) != os.readlink(b):
                    raise RuntimeError(f"verify(full) symlink differs: {rel}")
            elif got[rel] != size or not filecmp.cmp(a, b, shallow=False):
                raise RuntimeError(f"verify(full) content differs: {rel}")
        return len(orig)
    finally:
        shutil.rmtree(tdir, ignore_errors=True)


def pack_leaf(leaf, a, excl_file):
    t0 = time.time()
    parent, visit = os.path.split(leaf)
    rec = {"leaf": leaf}
    try:
        key = key_for(a.root, leaf, a.depth)
        rec["key"] = key

        if a.min_age_min > 0:
            age = (time.time() - os.stat(leaf).st_mtime) / 60
            if age < a.min_age_min:
                rec.update(result="skip", reason=f"modified {age:.0f} min ago")
                return rec

        img = os.path.join(parent, visit + ".sqfs")
        tmp = os.path.join(parent, visit + TMP_IMG_SUFFIX)
        meta = os.path.join(parent, visit + META_SUFFIX)
        old = os.path.join(parent, OLD_FMT.format(visit))
        target = os.path.join(a.mount_root, key)

        if os.path.lexists(old):
            raise RuntimeError(f"{old} exists; inspect before re-packing")

        if a.dry_run:
            rec.update(result="dry-run", image=img, symlink_to=target)
            return rec

        # 1. build image (excluded files are left out)
        cmd = ["mksquashfs", leaf, tmp, "-noappend", "-quiet", "-no-progress",
               "-b", a.block, "-processors", str(a.processors), "-mem", a.mem,
               "-exit-on-error"]
        if a.comp == "none":
            cmd += ["-no-compression"]
        else:
            cmd += ["-comp", a.comp]
            if a.comp in ("zstd", "gzip", "xz") and a.level:
                cmd += ["-Xcompression-level", str(a.level)]
        if excl_file:
            cmd += ["-wildcards", "-ef", excl_file]
        run(cmd)

        # 2. verify against originals (still hot in page cache)
        rec["files"] = verify(tmp, leaf, a.exclude, a.verify, a.verify_tmp)

        # 3. move excluded (status) files to the writable .meta dir BEFORE the
        #    image appears. Readers/writers (sqstatus_*.m) use the rule:
        #    status lives in <visit>.meta/ if that dir or <visit>.sqfs exists,
        #    otherwise in <visit>/. This order keeps the rule true at every step.
        moved = []
        for e in os.scandir(leaf):
            if is_excluded(e.name, a.exclude):
                os.makedirs(meta, exist_ok=True)
                os.rename(e.path, os.path.join(meta, e.name))
                moved.append(e.name)
        rec["meta"] = moved
        os.rename(tmp, img)

        # 4. swap folder for symlink: rename away, then atomic symlink rename
        os.rename(leaf, old)
        lnk_tmp = os.path.join(parent, f".{visit}.lnk")
        if os.path.lexists(lnk_tmp):
            os.unlink(lnk_tmp)
        os.symlink(target, lnk_tmp)
        os.rename(lnk_tmp, leaf)

        # 5. drop originals
        if not a.keep_originals:
            shutil.rmtree(old)

        st = os.stat(img)
        rec.update(result="ok", image_bytes=st.st_size, sec=round(time.time() - t0, 2))
        return rec
    except Exception as e:  # noqa: BLE001 - log and continue with other leaves
        try:
            tmp = os.path.join(parent, visit + TMP_IMG_SUFFIX)
            if os.path.exists(tmp):
                os.unlink(tmp)
        except OSError:
            pass
        rec.update(result="error", error=str(e), sec=round(time.time() - t0, 2))
        return rec


def cleanup_old(old, a):
    """Remove .<visit>.packold left by an interrupted run, if the swap completed."""
    parent, name = os.path.split(old)
    visit = name[1:-len(".packold")]
    leaf = os.path.join(parent, visit)
    img = os.path.join(parent, visit + ".sqfs")
    rec = {"leaf": leaf, "old": old}
    if os.path.islink(leaf) and os.path.isfile(img):
        if a.keep_originals or a.dry_run:
            rec.update(result="skip", reason="packold kept")
        else:
            shutil.rmtree(old)
            rec.update(result="cleaned")
    elif not os.path.lexists(leaf):
        rec.update(result="error",
                   error="interrupted between rename and symlink; run: mv OLD LEAF and re-pack")
    else:
        rec.update(result="error", error="packold exists next to a real folder; inspect manually")
    return rec


def cmd_pack(a):
    for tool in ("mksquashfs", "unsquashfs"):
        if not shutil.which(tool):
            sys.exit(f"{tool} not found: sudo apt install squashfs-tools")
    a.root = os.path.abspath(a.root)
    excl_file = None
    if a.exclude:
        fd, excl_file = tempfile.mkstemp(prefix="sqpack_excl_", text=True)
        with os.fdopen(fd, "w") as fh:
            fh.write("\n".join(a.exclude) + "\n")

    log_fh = open(a.log, "a") if a.log else None
    stats = {"ok": 0, "error": 0, "skip": 0, "dry-run": 0, "cleaned": 0}
    in_bytes = out_bytes = 0
    t0 = time.time()
    n_seen = 0

    def handle(rec):
        nonlocal out_bytes
        stats[rec["result"]] = stats.get(rec["result"], 0) + 1
        out_bytes += rec.get("image_bytes", 0)
        log(log_fh, rec)
        if a.dry_run:
            print(f"{rec['leaf']}  ->  {rec.get('image', '')}   (symlink -> {rec.get('symlink_to', '')})")
        done = sum(stats.values())
        if done % 100 == 0 and not a.dry_run:
            el = time.time() - t0
            print(f"[{done} done, {stats['ok']} ok, {stats['error']} err, "
                  f"{done / el:.1f} leaves/s]", file=sys.stderr)

    try:
        with cf.ThreadPoolExecutor(max_workers=a.jobs) as pool:
            pending = set()
            for kind, path in iter_leaves(a.root, a.depth):
                if kind == "old":
                    handle(cleanup_old(path, a))
                    continue
                n_seen += 1
                if a.limit and n_seen > a.limit:
                    break
                pending.add(pool.submit(pack_leaf, path, a, excl_file))
                if len(pending) >= a.jobs * 2:
                    done, pending = cf.wait(pending, return_when=cf.FIRST_COMPLETED)
                    for f in done:
                        handle(f.result())
            for f in cf.as_completed(pending):
                handle(f.result())
    finally:
        if excl_file:
            os.unlink(excl_file)
        if log_fh:
            log_fh.close()

    el = time.time() - t0
    print(f"\nsummary: {json.dumps(stats)}  images={out_bytes / 1e9:.2f} GB  "
          f"elapsed={el:.1f}s", file=sys.stderr)
    return 1 if stats["error"] else 0


# --------------------------------------------------------------------------- restore

def cmd_restore(a):
    """Turn packed leaves back into ordinary folders (data + meta files)."""
    a.root = os.path.abspath(a.root)
    rc = 0
    for leaf in a.leaves:
        leaf = os.path.abspath(leaf.rstrip("/"))
        parent, visit = os.path.split(leaf)
        img = os.path.join(parent, visit + ".sqfs")
        meta = os.path.join(parent, visit + META_SUFFIX)
        tmpdir = os.path.join(parent, f".{visit}.restore")
        try:
            if not os.path.islink(leaf) or not os.path.isfile(img):
                raise RuntimeError("not a packed leaf (expected symlink + .sqfs)")
            if os.path.lexists(tmpdir):
                shutil.rmtree(tmpdir)
            run(["unsquashfs", "-n", "-q", "-d", tmpdir, img])
            # data back first; status stays in .meta until the image is gone,
            # so the status-location rule stays true at every step
            os.unlink(leaf)
            os.rename(tmpdir, leaf)
            if a.keep_image:
                os.rename(img, img + ".restored")
            else:
                os.unlink(img)
            if os.path.isdir(meta):
                for e in os.scandir(meta):
                    os.rename(e.path, os.path.join(leaf, e.name))
                os.rmdir(meta)
            print(f"restored {leaf}")
        except Exception as e:  # noqa: BLE001
            rc = 1
            print(f"ERROR {leaf}: {e}", file=sys.stderr)
    return rc


def cmd_key(a):
    a.root = os.path.abspath(a.root)
    for leaf in a.leaves:
        print(key_for(a.root, os.path.abspath(leaf.rstrip("/")), a.depth))
    return 0


# --------------------------------------------------------------------------- CLI

def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)

    def common(sp):
        sp.add_argument("--root", required=True, help="top of the tree, e.g. /data (holds w2026/...)")
        sp.add_argument("--depth", type=int, default=4, help="levels below root to the leaf (default 4: wYYYY/MM/DD/visit)")

    pk = sub.add_parser("pack", help="pack unpacked leaf folders")
    common(pk)
    pk.add_argument("--mount-root", default="/sq", help="autofs mount point used in symlinks (default /sq)")
    pk.add_argument("--exclude", action="append", default=[],
                    help="glob of top-level files kept writable outside the image, e.g. 'status*' (repeatable)")
    pk.add_argument("--jobs", type=int, default=16, help="leaves packed in parallel (default 16)")
    pk.add_argument("--processors", type=int, default=2, help="threads per mksquashfs (default 2)")
    pk.add_argument("--mem", default="512M", help="cache memory per mksquashfs (default 512M)")
    pk.add_argument("--comp", default="zstd", choices=["zstd", "lz4", "gzip", "xz", "none"],
                    help="compression (default zstd; 'none' = packing only)")
    pk.add_argument("--level", type=int, default=3, help="compression level for zstd/gzip/xz (default 3)")
    pk.add_argument("--block", default="1M", help="squashfs block size (default 1M)")
    pk.add_argument("--verify", default="full", choices=["full", "list"],
                    help="full = extract + byte compare (default); list = names and sizes only")
    pk.add_argument("--verify-tmp", default="/dev/shm" if os.path.isdir("/dev/shm") else None,
                    help="scratch dir for full verification (default /dev/shm)")
    pk.add_argument("--min-age-min", type=float, default=60,
                    help="skip leaves modified less than N minutes ago (default 60; 0 = no check)")
    pk.add_argument("--keep-originals", action="store_true", help="keep .<visit>.packold dirs")
    pk.add_argument("--limit", type=int, default=0, help="stop after N leaves (pilot runs)")
    pk.add_argument("--log", default="sqpack.log.jsonl", help="JSON-lines log (default sqpack.log.jsonl)")
    pk.add_argument("--dry-run", action="store_true", help="show what would be packed, change nothing")
    pk.set_defaults(func=cmd_pack)

    rs = sub.add_parser("restore", help="unpack leaves back into ordinary folders")
    common(rs)
    rs.add_argument("leaves", nargs="+")
    rs.add_argument("--keep-image", action="store_true", help="keep the image, renamed to <visit>.sqfs.restored")
    rs.set_defaults(func=cmd_restore)

    ky = sub.add_parser("key", help="print autofs key for leaf path(s)")
    common(ky)
    ky.add_argument("leaves", nargs="+")
    ky.set_defaults(func=cmd_key)

    a = p.parse_args()
    sys.exit(a.func(a))


if __name__ == "__main__":
    main()
