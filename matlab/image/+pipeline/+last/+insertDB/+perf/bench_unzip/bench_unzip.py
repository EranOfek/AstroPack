#!/usr/bin/env python3
"""Benchmark extract + delete of zip archives into tmpfs (/dev/shm); no reads of extracted files."""

from __future__ import annotations

import argparse
import os
import shutil
import statistics
import subprocess
import sys
import time
import zipfile
from datetime import datetime
from pathlib import Path
from typing import Callable, List, Optional, Sequence, Tuple


MiB = 1024**2


def zip_metadata(path: Path) -> Tuple[float, float, int]:
    """Compressed MiB, uncompressed MiB, member count from central directory only."""
    zip_mb = path.stat().st_size / MiB
    with zipfile.ZipFile(path, "r") as zf:
        infos = zf.infolist()
        unpack = sum(i.file_size for i in infos)
        n_files = len(infos)
    return zip_mb, unpack / MiB, n_files


def tool_available(name: str) -> bool:
    return shutil.which(name) is not None


def resolve_methods(spec: str) -> List[str]:
    all_methods = ["unzip", "bsdtar", "7z", "python"]
    if spec.strip() == "all":
        return list(all_methods)
    chosen = [x.strip() for x in spec.split(",") if x.strip()]
    if not chosen:
        raise ValueError("empty --methods")
    for m in chosen:
        if m == "matlab":
            continue
        if m not in all_methods:
            raise ValueError(f"unknown method: {m}")
    return [m for m in chosen if m != "matlab"]


def extract_unzip(zip_path: Path, dest: Path) -> None:
    subprocess.run(
        ["unzip", "-q", "-o", str(zip_path), "-d", str(dest)],
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
    )


def extract_bsdtar(zip_path: Path, dest: Path) -> None:
    subprocess.run(
        ["bsdtar", "-xf", str(zip_path), "-C", str(dest)],
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
    )


def extract_7z(zip_path: Path, dest: Path) -> None:
    dest.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        ["7z", "x", "-bd", "-y", f"-o{dest}", str(zip_path)],
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
    )


def extract_python(zip_path: Path, dest: Path) -> None:
    dest.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(zip_path, "r") as zf:
        zf.extractall(dest)


def get_extractor(method: str) -> Callable[[Path, Path], None]:
    return {
        "unzip": extract_unzip,
        "bsdtar": extract_bsdtar,
        "7z": extract_7z,
        "python": extract_python,
    }[method]


def rm_dest(dest: Path) -> None:
    if dest.exists():
        subprocess.run(["rm", "-rf", str(dest)], check=True)


def timed_cycle(
    method: str,
    zip_path: Path,
    dest: Path,
    extract_fn: Callable[[Path, Path], None],
) -> Tuple[float, float, float]:
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.exists():
        rm_dest(dest)
    dest.mkdir(parents=True, exist_ok=True)

    t0 = time.perf_counter()
    extract_fn(zip_path, dest)
    unzip_sec = time.perf_counter() - t0

    t1 = time.perf_counter()
    rm_dest(dest)
    rm_sec = time.perf_counter() - t1

    return unzip_sec, rm_sec, unzip_sec + rm_sec


def log_line(fid, msg: str) -> None:
    line = f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')} {msg}"
    fid.write(line + "\n")
    fid.flush()
    print(msg, flush=True)


def mean_med(values: Sequence[float]) -> Tuple[float, float]:
    if not values:
        return float("nan"), float("nan")
    return statistics.fmean(values), statistics.median(values)


def discover_zips(zip_dir: Optional[Path], zip_file: Optional[Path]) -> List[Path]:
    if zip_file is not None:
        zf = zip_file.resolve()
        if not zf.is_file():
            raise FileNotFoundError(f"zip not found: {zf}")
        return [zf]
    if zip_dir is None:
        raise ValueError("ZIP_DIR or --zip-file required")
    d = zip_dir.resolve()
    if not d.is_dir():
        raise FileNotFoundError(f"zip dir not found: {d}")
    zips = sorted(d.glob("*.zip"))
    if not zips:
        raise FileNotFoundError(f"no *.zip under {d}")
    return zips


def method_ready(method: str) -> bool:
    if method == "python":
        return True
    if method == "unzip":
        return tool_available("unzip")
    if method == "bsdtar":
        return tool_available("bsdtar")
    if method == "7z":
        return tool_available("7z")
    return False


def run_method_on_zips(
    method: str,
    zips: List[Path],
    dest_root: Path,
    niter: int,
    warmup: int,
    fail_fast: bool,
    log_fid,
) -> List[dict]:
    if not method_ready(method):
        msg = f"SKIP method={method} reason=toolMissing"
        log_line(log_fid, msg)
        return []

    extract_fn = get_extractor(method)
    pid = os.getpid()
    records: List[dict] = []

    for zip_path in zips:
        try:
            zip_mb, unpack_mb, n_files = zip_metadata(zip_path)
        except Exception as exc:  # noqa: BLE001
            log_line(log_fid, f"ZIP method={method} file={zip_path.name} ERROR=metadata {exc}")
            if fail_fast:
                raise
            continue

        stem = zip_path.stem.replace("/", "_")
        for w in range(warmup):
            dest = dest_root / f"unzip_bench_{pid}_{method}_{stem}_w{w}"
            try:
                timed_cycle(method, zip_path, dest, extract_fn)
            except Exception as exc:  # noqa: BLE001
                rm_dest(dest)
                log_line(log_fid, f"ZIP method={method} file={zip_path.name} ERROR=warmup {exc}")
                if fail_fast:
                    raise
                break
        else:
            for it in range(1, niter + 1):
                dest = dest_root / f"unzip_bench_{pid}_{method}_{stem}_i{it}"
                try:
                    unzip_sec, rm_sec, cycle_sec = timed_cycle(method, zip_path, dest, extract_fn)
                except Exception as exc:  # noqa: BLE001
                    rm_dest(dest)
                    log_line(
                        log_fid,
                        f"ZIP method={method} file={zip_path.name} iter={it} ERROR={exc}",
                    )
                    if fail_fast:
                        raise
                    continue

                mbps = unpack_mb / unzip_sec if unzip_sec > 0 else float("nan")
                rec = {
                    "method": method,
                    "file": zip_path.name,
                    "iter": it,
                    "zipMB": zip_mb,
                    "unpackMB": unpack_mb,
                    "nFiles": n_files,
                    "unzipSec": unzip_sec,
                    "rmSec": rm_sec,
                    "cycleSec": cycle_sec,
                    "unzipMBps": mbps,
                }
                records.append(rec)
                log_line(
                    log_fid,
                    f"ZIP method={method} file={zip_path.name} iter={it} zipMB={zip_mb:.2f} "
                    f"unpackMB={unpack_mb:.2f} nFiles={n_files} unzipSec={unzip_sec:.3f} "
                    f"rmSec={rm_sec:.3f} cycleSec={cycle_sec:.3f} unzipMBps={mbps:.1f}",
                )

    if records:
        unzip_secs = [r["unzipSec"] for r in records]
        rm_secs = [r["rmSec"] for r in records]
        cycle_secs = [r["cycleSec"] for r in records]
        zip_mbs = [r["zipMB"] for r in records]
        u_mean, u_med = mean_med(unzip_secs)
        r_mean, r_med = mean_med(rm_secs)
        c_mean, c_med = mean_med(cycle_secs)
        z_mean = statistics.fmean(zip_mbs)
        total_unpack_gb = sum(r["unpackMB"] for r in records) / 1024
        n_zip = len({r["file"] for r in records})
        log_line(
            log_fid,
            f"AVG method={method} nZip={n_zip} nRec={len(records)} "
            f"unzipSec mean={u_mean:.3f} med={u_med:.3f} "
            f"rmSec mean={r_mean:.3f} med={r_med:.3f} "
            f"cycleSec mean={c_mean:.3f} med={c_med:.3f} "
            f"zipMB mean={z_mean:.2f} totalUnpackGB={total_unpack_gb:.2f}",
        )

    return records


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Unzip-to-tmpfs extract+delete benchmark")
    parser.add_argument("zip_dir", nargs="?", type=Path, default=None, help="Directory containing *.zip")
    parser.add_argument("--zip-file", type=Path, default=None, help="Single zip (optional)")
    parser.add_argument("--dest-root", type=Path, default=Path("/dev/shm"))
    parser.add_argument("--methods", default="unzip", help="Comma list or 'all' (excludes matlab; use shell for matlab)")
    parser.add_argument("--iters", type=int, default=1)
    parser.add_argument("--warmup", type=int, default=0)
    parser.add_argument("--fail-fast", action="store_true")
    parser.add_argument("--log-file", default="")
    args = parser.parse_args(argv)

    if args.iters < 1:
        print("--iters must be >= 1", file=sys.stderr)
        return 2
    if args.warmup < 0:
        print("--warmup must be >= 0", file=sys.stderr)
        return 2

    dest_root = args.dest_root.resolve()
    if not dest_root.is_dir():
        print(f"dest-root not found: {dest_root}", file=sys.stderr)
        return 2

    try:
        if "matlab" in args.methods.replace(" ", "").split(",") or args.methods.strip() == "matlab":
            print("bench_unzip.py: matlab method is run via bench_unzip.m / run_bench_unzip.sh", file=sys.stderr)
        methods = resolve_methods(args.methods)
        zips = discover_zips(args.zip_dir, args.zip_file)
    except (ValueError, FileNotFoundError) as exc:
        print(exc, file=sys.stderr)
        return 2

    log_path = (
        Path(args.log_file)
        if args.log_file
        else Path.cwd() / f"bench_unzip_{datetime.now():%Y%m%d_%H%M%S}.log"
    )

    with log_path.open("a", encoding="utf-8") as fid:
        host = os.uname().nodename if hasattr(os, "uname") else "unknown"
        log_line(
            fid,
            f"START zipDir={args.zip_dir} nZip={len(zips)} methods={','.join(methods)} "
            f"destRoot={dest_root} iters={args.iters} warmup={args.warmup}",
        )
        log_line(fid, f"HOST {host}")

        for method in methods:
            run_method_on_zips(method, zips, dest_root, args.iters, args.warmup, args.fail_fast, fid)

        log_line(fid, "END")

    print(f"log: {log_path}", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
