# Cron and `scripts/wrx/run_insert_archive_wrx80.sh`

Script: `matlab/image/+pipeline/+last/+insertDB/scripts/wrx/run_insert_archive_wrx80.sh` (WRX80). See [hosts_and_matlab.md](hosts_and_matlab.md) for euclid wrappers and MATLAB paths.

Core logic: `scripts/common/_run_insert_archive.sh` (do not call directly).

## Variables (environment / core)

| Variable | Default (WRX80 wrapper) | Purpose |
|----------|-------------------------|---------|
| `MATLAB_BIN` | `$HOME/Soft/M2025b/bin/matlab` | euclid wrapper uses `/usr/local/MATLAB/R2025b/bin/matlab` |
| `ALLOW_WRITES` | `0` | `0` (default) adds `SkipInsert`, `UpdateStatus=false`, `IgnoreStatus` -- no DB, no archive `.status` |
| `DUMMY` | `1` | Legacy: `DUMMY=0` same as `ALLOW_WRITES=1` |
| `WORKDIR` | `ASTROPACK_ROOT` | MATLAB cwd; sidecar `*_dir.txt` files append here |
| `PROC_TEMPLATE` | `LAST.01.01.01/2026/01/*/proc/*` | Edit month/node in core or export before run |
| `SECOND_PASS` | `0` | Default: **single pass** only. `1` runs all four jobs again after `DELAY_HOURS` |
| `DELAY_HOURS` | (required if `SECOND_PASS=1`) | Sleep between passes |

## MATLAB calls (pass 1 and optional pass 2)

Same order both times:

```matlab
pipeline.last.insertDB.perf.insertArchiveCatalogs2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
pipeline.last.insertDB.perf.insertArchiveCoaddCatalogs2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
pipeline.last.insertDB.perf.insertArchiveAsteroids2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
pipeline.last.insertDB.perf.insertArchiveImages2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
```

Update the month segment in `ProcDirTemplate` as needed.

## Suggested crontab (WRX80)

Replace four separate lines with one, for example:

```
0 10 * * * cd /home/sasha/matlab/AstroPack && matlab/image/+pipeline/+last/+insertDB/scripts/wrx/run_insert_archive_wrx80.sh >> ~/matlab/log/last/insertArchive/cron_wrapper.log 2>&1
```

Ensure pass 1 + delay + pass 2 finishes before the next day's run if `SECOND_PASS=1`.

## Production cutover

Set `ALLOW_WRITES=1` (or `DUMMY=0`) when ready to write ClickHouse and `.status` again via `*_perf`.
