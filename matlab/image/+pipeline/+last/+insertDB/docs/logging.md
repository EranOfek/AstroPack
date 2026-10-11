# Performance logging (`perfLog`)

## Writer

Package function: `matlab/image/+pipeline/+last/+insertDB/perfLog.m`

Each `*_perf.m` clone defines a **local** function `perfLog` at the bottom that forwards to `pipeline.last.insertDB.perfLog`.

## Log file

- Directory: `~/matlab/log/last/insertArchive/`
- File: `insertDB_perf.log`
- `~` is expanded from `HOME` / `USERPROFILE`; directory is created if missing.

## Line format

```
YYYY-MM-DD HH:MM:SS.nnn <message> [X.Xs]
```

Milliseconds in the timestamp. Optional duration suffix uses one decimal and `s` (e.g. `3.2s`).

## Controls

In `perfLog.m`, set `Enabled = false` to disable all file logging.

## Dummy measurement flags (clones only)

Pass to `*_perf` functions or use `DUMMY=1` in [run_insert_archive.sh](../run_insert_archive.sh):

- `SkipInsert` — skip `imProc.db.insertCatalog` / `insertImages`
- `UpdateStatus` — skip `su` append to `.status`
- `IgnoreStatus` — do not skip visits based on `.status`

Already-injected visits (when `IgnoreStatus` is false) increment summary counters only; they are not logged line-by-line.
