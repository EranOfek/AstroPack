# Performance logging (`perfLog`)

## Writer

Package writers: `perfLog.m`, `perfLogBlank.m`, `perfLogRunBanner.m` (shared I/O in `perfLogAppendLines.m`).

Each `*_perf.m` clone defines a **local** function `perfLog` at the bottom that forwards to `pipeline.last.insertDB.perfLog`.

## Log file

- Directory: `~/matlab/log/last/insertArchive/`
- File: `insertDB_perf.log`
- `~` is expanded from `HOME` / `USERPROFILE`; directory is created if missing.
- Written on whichever host runs MATLAB (WRX80 or euclid); see [hosts_and_matlab.md](hosts_and_matlab.md).
- On the first append to the log in a MATLAB session, the resolved log file path is printed to the command window (`insertDB_perf log: ...`).

## Run banner and blank lines

Each `*_perf` run starts with:

1. One **blank** line (no timestamp).
2. One **banner** line: `============== Started: <FnName> <yyyy-mm-dd HH:MM:SS.FFF>` (no leading timestamp column).
3. One **ENV** line (timestamped): `ENV host=... matlabRelease=... matlabVersion=...` (uses `INSERTDB_RUN_HOST` when set).
4. The usual timestamped `START RootDir=...` line (proc catalog clone also logs `CatalogFileInventory` / `CatalogRawFileRead` when passed).

The **banner** and **ENV** lines are written to `insertDB_perf.log` and echoed to the **command window**. Other `perfLog` lines are file-only.

After each visit folder’s `visitEnd` line, one **blank** line separates the next visit (or `SUMMARY` / `AVG`).

Example:

```text

============== Started: insertArchiveCatalogs2DB_perf 2026-10-12 14:30:00.123
2026-10-12 14:30:00.124 insertArchiveCatalogs2DB_perf ENV host=wrx80 matlabRelease=2025b matlabVersion=25.2.0.1234567
2026-10-12 14:30:00.125 insertArchiveCatalogs2DB_perf START RootDir=... SkipInsert=1 CatalogFileInventory=1 CatalogRawFileRead=1 ...
2026-10-12 14:30:01.001 insertArchiveCatalogs2DB_perf visitStart /bigdata2/.../000224v0
2026-10-12 14:30:02.000 insertArchiveCatalogs2DB_perf catalogFileInventory n=480 sizeMB=42.10 /bigdata2/.../000224v0
2026-10-12 14:30:05.000 insertArchiveCatalogs2DB_perf readCatalogFilesRaw n=480 sizeMB=155.17 secPerMB=0.0200 /bigdata2/.../000224v0 3.1s
2026-10-12 14:30:07.100 insertArchiveCatalogs2DB_perf readAstroCatalog sizeMB=155.17 secPerMB=0.0135 /bigdata2/.../000224v0 2.1s
2026-10-12 14:30:08.400 insertArchiveCatalogs2DB_perf readAstroHeader /bigdata2/.../000224v0 1.3s
2026-10-12 14:30:08.401 insertArchiveCatalogs2DB_perf catalogReadRates sizeMB=155.17 rawSecPerMB=0.0200 catSecPerMB=0.0135 hdrSecPerMB=0.0084 n=480 /bigdata2/.../000224v0
...
2026-10-12 14:35:00.000 insertArchiveCatalogs2DB_perf visitEnd injected /bigdata2/.../000224v0 299.0s

2026-10-12 14:35:00.001 insertArchiveCatalogs2DB_perf AVG fullWork n=1 ...
```

## Console vs log file

- **Log file** (`insertDB_perf.log`): every stage (`visitStart`, `catalogFileInventory`, `readCatalogFilesRaw`, `readAstroCatalog`, `readAstroHeader`, `readImages`, `visitEnd`, `AVG`, etc.) with timestamps.
- **Command window**: one summary per visit folder from `perfVisitEnd_`:

  ` ..visit done in X.Xs (log: insertDB_perf.log)`

  That duration is **visitTotal** — same as the `visitEnd` line (`toc(TicVisit)` from `visitStart` through optional compress/updateStatus, depending on job type).

**tic/toc in `*_perf.m`:** use named timers only (`TicVisit = tic`, `toc(TicVisit)`). Do not use bare `tic`/`toc`; stage lines use matching `TicStage` handles.

## Line format

```
YYYY-MM-DD HH:MM:SS.nnn <message> [X.Xs]
```

Milliseconds in the timestamp. Optional duration suffix uses one decimal and `s` (e.g. `3.2s`).

## Controls

In `perfLogAppendLines.m`, set `Enabled = false` to disable all file logging.

## Dummy measurement flags (clones only)

Pass to `*_perf` functions or use `DUMMY=1` via [scripts_wrx/run_insert_archive_wrx80.sh](../scripts_wrx/run_insert_archive_wrx80.sh):

- `SkipInsert` -- skip `imProc.db.insertCatalog` / `insertImages`
- `UpdateStatus` -- skip `su` append to `.status`
- `IgnoreStatus` -- do not skip visits based on `.status`

Proc catalog clone only ([`insertArchiveCatalogs2DB_perf.m`](../insertArchiveCatalogs2DB_perf.m)):

- `CatalogFileInventory` -- log file count and total size (MiB) per visit before catalog read.
- `CatalogRawFileRead` -- timed full-file `fread` baseline before `AstroCatalog` (adds one read pass per visit when true; included in `visitTotal`).

Measure-overhead shells enable both by default via `MEASURE_CATALOG_FILE_INVENTORY` and `MEASURE_CATALOG_RAW_FILE_READ`. See [astro_catalog_header_read_path.md](astro_catalog_header_read_path.md).

When inventory or raw is enabled and `sizeMB > 0`, each visit also logs **`catalogReadRates`**: `rawSecPerMB`, `catSecPerMB`, `hdrSecPerMB` (wall seconds divided by Cat MiB from `catalogFileInventory`). **`rawSecPerMB`** approximates cold disk+open cost per MiB; **`catSecPerMB`** after raw is mostly parse/CPU (production Cat without raw mixes I/O+parse). `rawSecPerMB=n/a` when `CatalogRawFileRead=false`.

Already-injected visits (when `IgnoreStatus` is false) increment summary counters only; they are not logged line-by-line.

## Per-folder averages (`AVG` summary)

Implemented in [perfVisitStats.m](../perfVisitStats.m); all four `*_perf.m` clones call `logSummary` before `SUMMARY`.

After each run, two lines are appended (no duration suffix on these lines):

- **visitTotal** -- wall time per visit folder (`visitStart` -> `visitEnd`).
- **visitInsert** -- sum of `insertCatalog` / `insertImages` time in that visit.
- **visitNonDb** -- per visit, `visitTotal - visitInsert` (read FITS/MAT, prep, status I/O; not ClickHouse insert).

| Cohort | Visits included |
|--------|-----------------|
| **fullWork** | `visitEnd injected` only (full read + prep; insert may be skipped via `SkipInsert`) |
| **allTimed** | every `visitEnd ...` outcome (`noData`, `broken`, etc.) |

Each line reports **n**, **mean** for total/insert/nonDb, and **median** / **p90** for **visitTotal** on that cohort. Prefer **fullWork** when folders have similar file counts.

Example:

```
insertArchiveCatalogs2DB_perf AVG fullWork n=1 visitTotal mean=42.3s med=42.3s p90=42.3s visitInsert mean=8.1s visitNonDb mean=34.2s
```

## Full-insert benchmark (`scripts_wrx/run_test_last_perf_insert_wrx80.sh`)

Set `TEST_SCOPE` to `visit` (one directory via `VISIT_DIR`), `night`, or `month` before running. All four jobs log to the same `insertDB_perf.log`. After a run, confirm rows in ClickHouse (e.g. `SELECT count() FROM last_perf.proc_src_perf`) and that archive `.status` files were not updated (`UpdateStatus=false`).
