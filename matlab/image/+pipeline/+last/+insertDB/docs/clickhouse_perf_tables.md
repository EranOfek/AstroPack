# ClickHouse perf table clones (`last_perf`)

Run on **euclid** where `clickhouse-client` reaches production `last`.

## Mapping

| Script | Production | Structure clone |
|--------|------------|-----------------|
| insertArchiveCatalogs2DB | `last.proc_src` | `last_perf.proc_src_perf` |
| insertArchiveCoaddCatalogs2DB | `last.visit_src` | `last_perf.visit_src_perf` |
| insertArchiveAsteroids2DB | `last.visit_asteroids` | `last_perf.visit_asteroids_perf` |
| insertArchiveImages2DB | `last.visit_images` | `last_perf.visit_images_perf` |

No data is copied; only `CREATE TABLE` from `SHOW CREATE TABLE`.

## Setup

1. Copy `matlab/image/+pipeline/+last/+insertDB/.env.example` to `.env` in the same folder (`.env` is gitignored).
2. Set `CLICKHOUSE_HOST`, `CLICKHOUSE_USER`, `CLICKHOUSE_PASSWORD`, optional `CLICKHOUSE_PORT`.

## Run

```bash
cd matlab/image/+pipeline/+last/+insertDB
python3 clone_perf_tables.py
```

Existing `last_perf.*_perf` tables are left in place (skip with message).

## Test shells (not for cron)

| Script | Purpose | ClickHouse | `.status` |
|--------|---------|------------|-----------|
| `run_measure_proc_overhead.sh` | Proc-catalog I/O + row build only (`SkipInsert=true`) | Connects to `last_perf.proc_src_perf`; no rows written | `UpdateStatus=false` |
| `run_test_last_perf_insert.sh` | All four `*_perf` jobs end-to-end | `SkipInsert=false` → `last_perf.*_perf` | `UpdateStatus=false` |

Both use `IgnoreStatus=true` and pass `DbName=last_perf`, `DbPassKey=last`, and `ColMapTableName` for the production XLS table key. Production cron remains `run_insert_archive.sh` → `last.*`.

`run_test_last_perf_insert.sh` accepts `TEST_SCOPE=visit|night|month` (default `visit` uses `VISIT_DIR` for a single proc visit).
