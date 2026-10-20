# sqpack: pack leaf folders into SquashFS images (generic prototype)

**LAST production on Euclid** uses squash-store + autofs (`astro-infra`
`servers/euclid/squash_visit_folders/`) and PostgreSQL status via
`pipeline.last.insertDB.status.StatusDB`. This folder is a generic prototype
(`wYYYY/MM/DD/<visit>/`, `/sq` autofs keys).

Layout `ROOT/wYYYY/MM/DD/<visit>/` (≈500 small files + status file) becomes:

```
ROOT/w2026/06/01/<visit>.sqfs      read-only image of the data files (one contiguous file)
ROOT/w2026/06/01/<visit>  ->  /sq/w2026_06_01_<visit>   symlink; autofs mounts the image on access
ROOT/w2026/06/01/<visit>.meta/     writable: status.txt and anything else you --exclude
```

Readers keep using `ROOT/w2026/06/01/<visit>/x.fits` unchanged.
Status is read/written at `ROOT/w2026/06/01/<visit>.meta/status.txt`.

## Install (once, Ubuntu 24.04)

```bash
sudo apt install squashfs-tools autofs
sudo install -m 755 auto.sq   /etc/auto.sq                 # edit ROOT= first
sudo install -m 644 sq.autofs /etc/auto.master.d/sq.autofs
sudo systemctl restart autofs
```

Add `/sq` to `PRUNEPATHS` in `/etc/updatedb.conf`, and point backups at the
`.sqfs` files, not at `/sq`, so tree-wide scans don't mount everything.

## Pilot, then scale

```bash
# see what would happen
python3 sqpack.py pack --root /data --exclude 'status*' --dry-run --limit 20

# pilot: 20 leaves, keep originals for comparison
python3 sqpack.py pack --root /data --exclude 'status*' --limit 20 --keep-originals

# full run, low I/O priority, resumable (re-run any time; packed leaves are skipped)
ionice -c3 nice python3 sqpack.py pack --root /data --exclude 'status*' --jobs 32
```

Options worth knowing:

| option | default | meaning |
|---|---|---|
| `--exclude GLOB` | none | top-level files kept writable in `<visit>.meta/` (repeatable) |
| `--comp` | `zstd` | `zstd`, `lz4`, `gzip`, `xz`, or `none` (packing only) |
| `--verify` | `full` | `full` = extract to /dev/shm and compare byte-for-byte; `list` = names and sizes |
| `--min-age-min` | `60` | skip folders modified in the last N minutes (still being written) |
| `--jobs` / `--processors` / `--mem` | 16 / 2 / 512M | parallel leaves, threads and cache per mksquashfs |
| `--keep-originals` | off | keep `.<visit>.packold` until a later run removes it |

Every leaf is logged as one JSON line in `sqpack.log.jsonl` (files, image size, time, errors).
The exit code is 1 if any leaf failed; failed leaves are left untouched.

## Undo

```bash
python3 sqpack.py restore --root /data /data/w2026/06/01/<visit>
```

Rebuilds the ordinary folder (data + status files) and deletes the image
(`--keep-image` keeps it as `<visit>.sqfs.restored`).

## Naming rule

The autofs key joins the path with `_` (`w2026_06_01_<visit>`), so only the last
level may contain `_`. `auto.sq` also checks the `wYYYY/MM/DD` shape; edit its
regexes if your names differ.

## Folder status (`matlab/`, `sql/`)

Per-folder status lives in PostgreSQL (`sql/schema.sql`), used from MATLAB through
the `StatusDB` class in `matlab/` (see `matlab/README.md`). Until the existing
`*.status` files are imported with `st.importStatusFiles(...)`, pack with
`--exclude '*.status'` so they stay writable in `<visit>.meta/`; the import finds
them there.
