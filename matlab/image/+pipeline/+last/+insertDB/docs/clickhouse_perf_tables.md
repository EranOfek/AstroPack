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

Use `run_*_wrx80.sh` or `run_*_euclid.sh` ([hosts_and_matlab.md](hosts_and_matlab.md)).

| Wrapper (either host suffix) | Purpose | ClickHouse | `.status` |
|------------------------------|---------|------------|-----------|
| `run_smoke_proc_one_visit_*` | One proc visit, smoke | `SkipInsert=true`; optional `describe` on `last_perf` | `UpdateStatus=false` |
| `run_measure_proc_overhead_*` | Proc I/O + row build | `last_perf.proc_src_perf`; no rows | `UpdateStatus=false` |
| `run_test_last_perf_insert_*` | All four `*_perf` jobs | Inserts -> `last_perf.*_perf` | `UpdateStatus=false` |

Production cron on WRX80: `scripts_wrx/run_insert_archive_wrx80.sh` (writes via `*_perf` when `ALLOW_WRITES=1`).

`run_test_last_perf_insert_*` accepts `TEST_SCOPE=visit|night|month` (default `visit` uses `VISIT_DIR`).
