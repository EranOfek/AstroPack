# bench_unzip — extract + delete on tmpfs

Times **unzip → `/dev/shm` → `rm -rf`** for each `*.zip` in a proc `_zip` folder. Does not read extracted FITS.

## Euclid (Jan 06 night example)

```bash
cd ~/matlab/AstroPack/matlab/image/+pipeline/+last/+insertDB/bench_unzip
chmod +x run_bench_unzip.sh run_bench_unzip_euclid.sh   # once

./run_bench_unzip_euclid.sh \
  /bigdata2/projects/last/data/LAST.01.01.01/2026/01/06/proc/_zip \
  --dest-root /dev/shm --methods unzip --iters 1 --warmup 0
```

Pre-flight:

```bash
ls /bigdata2/projects/last/data/LAST.01.01.01/2026/01/06/proc/_zip/*.zip | wc -l
```

Optional log tee:

```bash
./run_bench_unzip_euclid.sh ... 2>&1 | tee bench_unzip_$(date +%Y%m%d_%H%M%S).log
```

Direct Python (no MATLAB env):

```bash
python3 bench_unzip.py /bigdata2/projects/last/data/LAST.01.01.01/2026/01/06/proc/_zip \
  --dest-root /dev/shm --methods unzip --iters 1 --warmup 0
```

## Console lines

**Per zip** (printed and appended to log):

```text
ZIP method=unzip file=010736v0.zip iter=1 zipMB=42.10 unpackMB=91.50 nFiles=480 unzipSec=1.234 rmSec=0.089 cycleSec=1.323 unzipMBps=74.2
```

**After all zips** (one line per method):

```text
AVG method=unzip nZip=59 nRec=59 unzipSec mean=1.150 med=1.080 rmSec mean=0.090 med=0.080 cycleSec mean=1.240 med=1.160 zipMB mean=41.20 totalUnpackGB=5.40
```

| Field | Meaning |
|-------|---------|
| `unzipSec` | Extract only (decompress + create files on tmpfs) |
| `rmSec` | `rm -rf` of extract directory |
| `cycleSec` | `unzipSec + rmSec` (full visit unpack cost on tmpfs) |
| `unzipMBps` | `unpackMB / unzipSec` (does not include rm) |

## Methods

| Method | Backend |
|--------|---------|
| `unzip` | Info-ZIP `unzip -q -o` (default for night sweep) |
| `bsdtar` | `bsdtar -xf` (skipped if missing) |
| `7z` | `7z x` (skipped if missing) |
| `python` | `zipfile.extractall` |
| `matlab` | `bench_unzip.m` → `unzip()` via `run_bench_unzip.sh` |

Compare tools on one archive:

```bash
./run_bench_unzip.sh /path/to/proc/_zip --zip-file /path/to/one.zip \
  --methods unzip,bsdtar,7z,python --iters 5 --warmup 1
```

## Files

- `bench_unzip.py` — CLI tools + zipfile
- `bench_unzip.m` — MATLAB `unzip()` only
- `run_bench_unzip.sh` — host-agnostic driver
- `run_bench_unzip_euclid.sh` — sources `scripts/euclid/insertdb_env_euclid.sh`

Not part of production `insertArchive*` ingest.
