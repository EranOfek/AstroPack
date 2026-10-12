# Performance logging (`perfLog`)

## Writer

Package function: `matlab/image/+pipeline/+last/+insertDB/perfLog.m`

Each `*_perf.m` clone defines a **local** function `perfLog` at the bottom that forwards to `pipeline.last.insertDB.perfLog`.

## Log file

- Directory: `~/matlab/log/last/insertArchive/`
- File: `insertDB_perf.log`
- `~` is expanded from `HOME` / `USERPROFILE`; directory is created if missing.
- Written on whichever host runs MATLAB (WRX80 or euclid); see [hosts_and_matlab.md](hosts_and_matlab.md).
- On the first `perfLog` write in a MATLAB session, the resolved log file path is printed to the command window (`insertDB_perf log: ...`).

## Line format

```
YYYY-MM-DD HH:MM:SS.nnn <message> [X.Xs]
```

Milliseconds in the timestamp. Optional duration suffix uses one decimal and `s` (e.g. `3.2s`).

## Controls

In `perfLog.m`, set `Enabled = false` to disable all file logging.

## Dummy measurement flags (clones only)

Pass to `*_perf` functions or use `DUMMY=1` via [scripts_wrx/run_insert_archive_wrx80.sh](../scripts_wrx/run_insert_archive_wrx80.sh):

- `SkipInsert` -- skip `imProc.db.insertCatalog` / `insertImages`
- `UpdateStatus` -- skip `su` append to `.status`
- `IgnoreStatus` -- do not skip visits based on `.status`

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
