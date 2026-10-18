# Change log -- LAST insertArchive performance measurement

Paths in this folder are relative to the repo root and use `/`.

## Why these changes exist

Production cron on WRX80 runs four archive-to-ClickHouse ingest scripts daily. Before changing algorithms or MATLAB version, we need timings on WRX80 against euclid-mounted data and euclid ClickHouse. Original scripts stay unchanged; measurement lives in new files only.

## Changes

### `*_perf.m` clones (four files)

**Why:** Add stage timings without touching `insertArchiveCatalogs2DB.m`, `insertArchiveCoaddCatalogs2DB.m`, `insertArchiveAsteroids2DB.m`, or `insertArchiveImages2DB.m`. Cron can switch to clones when ready.

### Local `perfLog` at the bottom of each clone

**Why:** The main body calls short `perfLog(...)` only. The package writer `pipeline.last.insertDB.perfLog` stays in one place.

### `perfVisitStats.m` + `AVG` summary lines

**Why:** After large runs, mean/median/p90 per visit folder (total, insert, non-DB) without parsing every `visitEnd` line. See [logging.md](logging.md).

### `perfLog.m` (shared writer)

**Why:** Minimal append logging (fopen / fprintf / fclose per line) so each line is on disk before the next step; no MATLAB `fflush` on file IDs. The `LogFile` class path is commented out in `perfLog.m` for a future switch.

### `perfLogRunBanner` / `perfLogBlank`

**Why:** Blank line + `============== Started: ...` at each run start, and a blank line after each `visitEnd`, so long `insertDB_perf.log` files are easier to scan.

### Console `visit done` vs legacy `Injecting ... done in 0 s`

**Why:** Legacy bare `tic`/`toc` after catalog read only timed prep/skip-insert (~0 s). Console now prints `toc(TicVisit)` in `perfVisitEnd_`, matching `visitEnd` in the log. Proc/coadd `visitEnd` includes optional `CompressProcessed` before `perfVisitEnd_` (aligned).

**Default log directory:** `~/matlab/log/last/insertArchive/insertDB_perf.log`

### Dummy-run flags on clones

| Flag | Default | Why |
|------|---------|-----|
| `SkipInsert` | false | Measure FITS/MAT read and table prep without calling `imProc.db.insertCatalog` / `insertImages`. |
| `UpdateStatus` | true | Do not append `.status` on euclid when false (safe replays). |
| `IgnoreStatus` | false | Process every visit even if already injected (repeat timing on same dirs). |

### Host wrappers under `scripts_wrx/` and `scripts_euclid/`

**Why:** WRX80 uses `$HOME/Soft/M2025b/bin/matlab` and NFS `/bigdata2`; euclid uses `/usr/local/MATLAB/R2025b/bin/matlab` and local disk. Shared cores live in `scripts_wrx/lib/`. See [hosts_and_matlab.md](hosts_and_matlab.md).

### `run_insert_proc_last_perf_{wrx80,euclid}.sh`

**Why:** Same phase1/phase2 scope as `run_measure_proc_overhead_*`, but `SkipInsert=false` into hard-coded **`last_perf.proc_src_perf`** (with startup checks so production `last` is never targeted). `DbPassKey=last` is only the password key.

### `scripts_wrx/run_smoke_proc_one_visit_wrx80.sh` (and euclid twin)

**Why:** One proc visit (~480 FITS), measure-only, for a short WRX80 vs euclid comparison before long benchmarks.

### `scripts_wrx/run_insert_archive_wrx80.sh` (cron)

**Why:** One cron entry runs all four jobs in order (no overlap with four staggered crons). Optional second pass after `DELAY_HOURS` catches archive files that arrive hours after the remote pipeline.

### `clone_perf_tables.py`

**Why:** Empty-structure clones in `last_perf.*_perf` on euclid for future insert tests without touching production `last.*`. Credentials in `.env`, not in git.

### Catalog read perf split (`readAstroCatalog` / `readAstroHeader`)

**Why:** Proc/coadd `*_perf` clones log `AstroCatalog` and `AstroHeader` separately (replaces combined `readCatalog`). See [astro_catalog_header_read_path.md](astro_catalog_header_read_path.md).

### Proc catalog I/O baseline flags (`CatalogFileInventory` / `CatalogRawFileRead`)

**Why:** Optional per-visit `catalogFileInventory` and `readCatalogFilesRaw` in `insertArchiveCatalogs2DB_perf.m` only. Measure-overhead and **`_run_insert_proc_last_perf.sh`** pass both via env (default on). See [logging.md](logging.md).

### Insert benchmark shells (`run_insert_proc_last_perf_*`)

**Why:** Same `measure_proc_overhead.env` + catalog perf flags for euclid vs WRX inserts into `last_perf.proc_src_perf` only. Protocol: [hosts_and_matlab.md](hosts_and_matlab.md#insert-benchmark-last_perfproc_src_perf-only).

### Proc `insertCatalog` density and `secPer100k` (proc perf)

**Why:** Per-visit `fieldId`, `nCat`, `nRows`, `srcPerCat`, and client-side `secPer100k` when insert runs; run-level `AVG insertCatalog` only for real inserts (not `SkipInsert`). See [logging.md](logging.md#proc-insertcatalog-density-and-row-rates-proc-perf-clone-only).

### `catalogReadRates` / MiB/s + `AVG catalogReadRates` (proc perf)

**Why:** Per-visit **`rawMBps` / `catMBps` / `hdrMBps`** when inventory/raw enabled; run-level **`AVG catalogReadRates`**.

### `CatalogAstroPerf` / `AVG catalogAstroPerf` (proc perf)

**Why:** Default measure-overhead mode: **`rowsPerSec`** from `readAstroCatalog` wall time and catalog row count (no inventory/raw). Shell defaults `MEASURE_CATALOG_*` 0/0/1. See [logging.md](logging.md).

### `parse_insertdb_perf_log.py`

**Why:** One aligned stdout line per visit (`folder`, `nCat`, `nRows`, `visitEnd` seconds) from `catalogAstroPerf` + `visitEnd` in `insertDB_perf.log`. See [logging.md](logging.md).

### `SkipDbInsert` + `insertCatalog_perf`

**Why:** Measure non-CH work inside catalog insert (table build, BJD, healpix) vs ClickHouse client time. Production `imProc.db.insertCatalog` gains `SkipDbInsert`; perf clone uses `insertCatalog_perf` with stage lines and `MEASURE_SKIP_DB_INSERT` on insert shell. See [insertCatalog_call_tree.md](insertCatalog_call_tree.md).

### `tableBuildDetail` in `insertCatalog_perf`

**Why:** One visit-level line splits **tableBuild** into hdr/id/sel/concat/nan without per-crop log I/O. See [insertCatalog_tableBuild.md](insertCatalog_tableBuild.md).

### Zip-backed catalog read (`UseZipCatalogs` on `insertArchiveCatalogs2DB_perf`)

**Why:** On euclid, proc visit folders may be empty or squashed while catalog FITS live in `proc/_zip/{visitId}.zip`. Visit discovery and `.status` still use the archive visit path; catalogs are extracted with MATLAB `unzip` to `/dev/shm/insertArchiveCatalogs2DB/{visitId}/` (or `/tmp` / `C:/Temp`), read there, then only that extract folder is deleted. Shell: `USE_ZIP_CATALOGS=1` on `_run_insert_proc_last_perf.sh`. See [logging.md](logging.md).

### Run `ENV` line (host + MATLAB version)

**Why:** `perfLogRunBanner` writes timestamped `ENV host=... matlabRelease=... matlabVersion=...` to log and command window for WRX/euclid comparisons.

### WRX NFS vs euclid local (catalog read stages)

**Why:** Document that proc catalog read is CPU/small-file heavy; WRX can match euclid on warm cache; compare night-level `AVG` medians, not two visits. See [hosts_and_matlab.md](hosts_and_matlab.md#wrx80-vs-euclid-proc-timing) and [folder_file_estimates.md](folder_file_estimates.md).

### `bench_catalog_read/` (MATLAB + Python thread sweeps)

**Why:** Measure raw vs AstroCatalog vs header read rates vs worker count on euclid before parallelizing `ImageIO` or `_perf`. Entry: [bench_catalog_read/README.md](../bench_catalog_read/README.md).

### Documentation under `docs/`

**Why:** Machines, tables, logging, cron, and per-script processing are written down for WRX80/euclid operations and later optimization work.
