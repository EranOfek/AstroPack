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

**Default log directory:** `~/matlab/log/last/insertArchive/insertDB_perf.log`

### Dummy-run flags on clones

| Flag | Default | Why |
|------|---------|-----|
| `SkipInsert` | false | Measure FITS/MAT read and table prep without calling `imProc.db.insertCatalog` / `insertImages`. |
| `UpdateStatus` | true | Do not append `.status` on euclid when false (safe replays). |
| `IgnoreStatus` | false | Process every visit even if already injected (repeat timing on same dirs). |

### Host wrappers under `scripts_wrx/` and `scripts_euclid/`

**Why:** WRX80 uses `$HOME/Soft/M2025b/bin/matlab` and NFS `/bigdata2`; euclid uses `/usr/local/MATLAB/R2025b/bin/matlab` and local disk. Shared cores live in `scripts_wrx/lib/`. See [hosts_and_matlab.md](hosts_and_matlab.md).

### `scripts_wrx/run_smoke_proc_one_visit_wrx80.sh` (and euclid twin)

**Why:** One proc visit (~480 FITS), measure-only, for a short WRX80 vs euclid comparison before long benchmarks.

### `scripts_wrx/run_insert_archive_wrx80.sh` (cron)

**Why:** One cron entry runs all four jobs in order (no overlap with four staggered crons). Optional second pass after `DELAY_HOURS` catches archive files that arrive hours after the remote pipeline.

### `clone_perf_tables.py`

**Why:** Empty-structure clones in `last_perf.*_perf` on euclid for future insert tests without touching production `last.*`. Credentials in `.env`, not in git.

### Documentation under `docs/`

**Why:** Machines, tables, logging, cron, and per-script processing are written down for WRX80/euclid operations and later optimization work.
