# Proc catalog read benchmarks

Goal: load the ~480 `LAST*proc_Cat_1.fits` tables of one visit into memory as fast as possible
(production path: `AstroCatalog(List,'UseMex',true)`, see
[astro_catalog_header_read_path.md](../docs/astro_catalog_header_read_path.md)).

## Result (euclid, 2026-10-13, `/bigdata2/.../LAST.01.10.04`, 480 files, 113 MB, 16 cores)

**Cold** = first touch of a visit nobody read before (what ingest really sees; 5 different visits per row, median):

| Method | cold median | warm median |
|---|---:|---:|
| MATLAB `AstroCatalog(...,'UseMex',true)` (production) | **15.7 s** | 1.6 s |
| MATLAB `io.fits.mex.read_catalog` loop | 18.7 s | 1.2 s |
| MATLAB pure `readCatalogMatlab` (no MEX), serial | 3.8 s | 0.55 s |
| MATLAB `readCatalogMatlab` in `parfor`, 16 process workers | 0.65 s | 0.12 s |
| **MATLAB `read_catalog_batch` (new MEX), 16 threads + prefetch** | **0.17 s** | **0.012 s** |
| Python astropy, serial | 9.3 s | 3.6 s |
| Python fitsio (cfitsio), serial | 8.1 s | 0.47 s |
| Python hand parser (`catread.read_numpy`), serial | 5.4 s | 0.028 s |
| Python hand parser, 16 threads + prefetch | 0.14 s | 0.057 s |
| *I/O ceiling*: raw read, prefetch + 16 threads | 0.19 s | 0.035 s |

Take-aways

1. **Cold, the disk is the whole story** (spinning RAID, ~6 ms of seek per file). CFITSIO-style readers issue
   many small sequential reads per file and run one file at a time: 8-30 s. Reading each file with a single `read()`
   is 1.5-5x faster; the big win (another 20-40x) is a **deep I/O queue**: all 480 files outstanding at once so the kernel/RAID
   can sort and merge the seeks. Either `posix_fadvise(WILLNEED)` on every file first, or 64 reader threads. The batch MEX
   and the Python prefetch reader both land on the raw-read ceiling, i.e. nothing is left to gain on I/O.
2. With prefetch, thread count barely matters (MEX: 1 thread 0.31 s, 4 -> 0.24, 16 -> 0.17, 64 -> 0.16). Without prefetch you
   need 64 threads to get there (16 threads = 0.45 s). Default: 16 threads + prefetch.
3. Warm (data in page cache), the parse is the cost: production MATLAB spends 1.6 s on what the hand parser does in
   12 ms (struct/table building, cfitsio per-column loops, `AstroCatalog` property copies).
4. Python threads do not help `astropy`/`fitsio` (GIL; 2-3x *slower* warm, no gain cold), process pools help a little warm but lose to
   pickling. In Python use the hand parser (`numpy`) or `fitsio` + prefetch.
5. The batch MEX output is **bit-identical** to `table2array(struct2table(read_catalog(...)))`, including column
   names and units (verified on all 480 files; 3 schema groups: 473 files x 43 cols, 5 and 2 files with 40 cols).

Evict state (page cache dropped, RAID cache still hot, python only) is in `results/` for reference; it is
*not* a real cold test.

## What the new MEX does (`read_catalog_batch.cpp`)

```matlab
[C, ColNames, ColUnits, Status, SchemaId] = read_catalog_batch(Files, NThreads, Prefetch);
```
One call, `N` files: parallel `open+posix_fadvise(WILLNEED)` for all, a thread pool reads only header + table bytes
(never the trailing image HDU, ~15 % of each file), the main thread allocates the outputs, workers byte-swap and
transpose into MATLAB column-major. `Status(i) ~= 0` marks files it can't handle (non all-double table...) which
the caller must re-read with `io.fits.mex.read_catalog`. No cfitsio dependency. Build:

```bash
mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -std=c++17 -pthread' LDFLAGS='$LDFLAGS -pthread' read_catalog_batch.cpp
```

**Not done (deliberately, benchmark only):** nothing in `ImageIO`/`AstroTable`/`AstroCatalog` was changed. To get the win in
production, `ImageIO`'s per-file loop has to be replaced by one `read_catalog_batch` call per visit; `AstroTable` then builds
the objects from the returned matrices + `ColNames`/`ColUnits` (the per-file `Header` is not returned yet: `AstroHeader`
HDU 3 is a separate pass and was **not benchmarked here**).

## Files

| File | Purpose |
|------|---------|
| `bench_catalog_read.m` / `.py` | MATLAB / Python benchmark harnesses (same states, same csv columns) |
| `catread.py` | Python readers: `raw`, `numpy` (hand parser), `numpy_mmap`, `fitsio`, `astropy` + drivers |
| `readCatalogMatlab.m` | pure-MATLAB reader (no MEX; works in `parfor`/thread pools) |
| `read_catalog_batch.cpp` | the batch MEX (built `.mexa64` is not committed) |
| `evict_cache.py` | drop a visit from the page cache (`posix_fadvise DONTNEED`, no root needed) |
| `summarize_bench.py` | median/min/max table from the csv files |
| `run_bench_catalog_read.sh` | driver for both languages; `run_bench_catalog_read_euclid.sh` = euclid env wrapper |
| `results/` | csv files behind the table above |

## Method (why the numbers can be trusted)

* **States.** `warm` = same visit, page cache hot (CPU only). `evict` = page cache dropped per run via `posix_fadvise(DONTNEED)`
  (works unprivileged, verified with `/proc/meminfo` Cached -113 MB), but the RAID controller cache stays hot, so it flatters readers.
  `cold` = every trial reads a **fresh visit** (from `--cold-pool` proc dirs, shuffled, config order randomised per round,
  used visits recorded in `cold_used_visits.txt`, shared by MATLAB and Python so none is reused). Cold trials vary 2-3x between visits;
  compare medians, which is what `summarize_bench.py` does.
* **Equal payload.** Every method returns one `[514 x 43]` double matrix per file; each trial is checked against an independent
  reference (`io.fits.mex.read_catalog` + `table2array` in MATLAB, astropy/hand-parser in Python) outside the timed region.
* Pools/MEX are created before the clock starts; `warm` has an untimed warm-up run.
* No `drop_caches` (needs root). For a still harsher test run as root: `sync; echo 3 > /proc/sys/vm/drop_caches` and reuse a visit.

## Usage

```bash
./run_bench_catalog_read.sh --visit /bigdata2/projects/last/data/LAST.01.10.04/2026/06/05/proc/214135v0 --states warm
./run_bench_catalog_read.sh --visit <v0 dir> --states cold --reps 5 \
    --cold-pool /bigdata2/projects/last/data/LAST.01.10.04/2026/01/23/proc  --cold-pool .../2026/01/24/proc
python3 summarize_bench.py bench_*_ml.csv bench_*_py.csv
```
`--lang matlab|python|both`, `--ml-configs raw,astrocatalog,batch:16,...`, `--py-methods raw,numpy,fitsio,astropy`.
Python needs `numpy`; `fitsio`/`astropy` are optional (skipped when missing). MATLAB needs the AstroPack startup
(`ASTROPACK_PATH` = repo root; the driver sets it) and, for `matlab_proc`, Parallel Computing Toolbox.

Findings from reviewing the old scripts: Python `header` mode read `hdul[0]` (these files have only 3 HDUs, so `len>3` was false), visit
discovery matched `"re"` anywhere in the *path*, and neither harness controlled the page cache, so repeated runs measured RAM, not disk.
