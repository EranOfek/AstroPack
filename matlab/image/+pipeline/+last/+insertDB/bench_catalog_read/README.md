# Proc catalog read benchmarks

Thread-sweep benchmarks for ~480 `LAST*proc_Cat_1.fits*` files per visit. Use on **euclid** (local `/bigdata2`) before changing `ImageIO` or `insertArchiveCatalogs2DB_perf`.

See also [astro_catalog_header_read_path.md](../docs/astro_catalog_header_read_path.md) for the production read path.

## Layout

| File | Purpose |
|------|---------|
| `bench_catalog_read.m` | MATLAB: raw / astro / header modes, `parfor` when `Threads>1` |
| `bench_catalog_read.py` | Python: raw (threads), astropy table/header (process pool) |
| `run_bench_catalog_read.sh` | Core driver |
| `run_bench_catalog_read_euclid.sh` | Euclid env + MATLAB startup |

**Input:** `PARENT` = a **`proc/`** directory (children are visit folders `*v0`, e.g. `/bigdata2/projects/last/data/LAST.01.01.01/2026/02/15/proc`).

## Quick start (euclid)

```bash
cd ~/matlab/AstroPack/matlab/image/+pipeline/+last/+insertDB/bench_catalog_read
chmod +x run_bench_catalog_read.sh run_bench_catalog_read_euclid.sh   # once if needed

./run_bench_catalog_read_euclid.sh \
  /bigdata2/projects/last/data/LAST.01.01.01/2026/02/15/proc \
  --threads 1,2,4,8,16 --mode all --max-visits 3 --lang both
```

Environment defaults come from [insertdb_env_euclid.sh](../scripts_euclid/insertdb_env_euclid.sh): `MATLAB_BIN`, `ASTROPACK_ROOT`, `INSERTDB_ARCHIVE_ROOT`.

## Options

| Flag | Default | Meaning |
|------|---------|---------|
| `--threads` | `1` | Comma list of worker counts |
| `--mode` | `all` | `raw`, `astro`, `header`, or `all` |
| `--max-visits` | `1` | How many `*v0` visits under `PARENT` |
| `--template` | `LAST*proc_Cat_1.fits*` | Glob inside each visit |
| `--lang` | `both` | `matlab`, `python`, or `both` |
| `--log-file` | auto timestamp | Append log path |

## Modes

| Mode | MATLAB | Python |
|------|--------|--------|
| **raw** | `fopen` + `fread` all bytes (like `readCatalogFilesRaw`) | Same via `ThreadPoolExecutor` |
| **astro** | `threads=1`: `AstroCatalog(...,'UseMex',true)`; `>1`: `parfor` + `ImageIO.read1` → `AstroCatalog` cell | First table HDU via **astropy** (reference; slower than MEX) |
| **header** | `threads=1`: `AstroHeader(...,3,'UseMex',true)`; `>1`: `parfor` + `read_header(...,3)` | HDU 3 header via astropy |

MATLAB **`Threads>1`** needs Parallel Computing Toolbox (`parpool('local', N)`).

## Reading results

Each line reports `sizeMB`, wall `sec`, and **MBps** (= sizeMB / sec).

- **raw** at high threads ≈ disk/NFS concurrency ceiling.
- **astro** `threads=1` ≈ production `readAstroCatalog`; parallel astro ≈ upper bound if `ImageIO` is parallelized later.
- **header** ≈ second pass in ingest (`readAstroHeader`).

Compare **MBps vs threads**; if raw stops scaling, parallel MEX is unlikely to beat that by much.

## Git executable bit

When committing on Linux:

```bash
git update-index --chmod=+x run_bench_catalog_read.sh run_bench_catalog_read_euclid.sh bench_catalog_read.py
```
