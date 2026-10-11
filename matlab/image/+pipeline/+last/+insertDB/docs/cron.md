# Cron and `run_insert_archive.sh`

Script: `matlab/image/+pipeline/+last/+insertDB/run_insert_archive.sh`

## Variables (top of script)

| Variable | Default | Purpose |
|----------|---------|---------|
| `MATLAB_BIN` | `/usr/local/MATLAB/R2020b/bin/matlab` | Switch to R2025b path when testing |
| `ALLOW_WRITES` | `0` | `0` (default) adds `SkipInsert`, `UpdateStatus=false`, `IgnoreStatus` — no DB, no archive `.status` |
| `DUMMY` | `1` | Legacy: `DUMMY=0` same as `ALLOW_WRITES=1` |
| `WORKDIR` | script start directory | MATLAB `cd` before batch (sidecar `*_dir.txt` files) |
| `SECOND_PASS` | `0` | Default: **single pass** only. `1` runs all four jobs again after `DELAY_HOURS` |
| `DELAY_HOURS` | (required if `SECOND_PASS=1`) | Sleep between passes |

## MATLAB calls (pass 1 and optional pass 2)

Same order both times:

```matlab
pipeline.last.insertDB.insertArchiveCatalogs2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
pipeline.last.insertDB.insertArchiveAsteroids2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
pipeline.last.insertDB.insertArchiveImages2DB_perf('/bigdata2/projects/last/data/','ProcDirTemplate','LAST.01.01.01/2026/01/*/proc/*')
```

Update the month segment in `ProcDirTemplate` as needed.

## Suggested crontab (WRX80)

Replace four separate lines with one, for example:

```
0 10 * * * /path/to/AstroPack/matlab/image/+pipeline/+last/+insertDB/run_insert_archive.sh >> ~/matlab/log/last/insertArchive/cron_wrapper.log 2>&1
```

Ensure pass 1 + delay + pass 2 finishes before the next day's run if `SECOND_PASS=1`.

## Production cutover

Set `ALLOW_WRITES=1` (or `DUMMY=0`) when ready to write ClickHouse and `.status` again via `*_perf`.
