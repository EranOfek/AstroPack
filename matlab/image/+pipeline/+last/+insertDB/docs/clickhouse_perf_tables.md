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

MATLAB `*_perf` clones still target `last` in this pass; pointing inserts at `last_perf` is a follow-up.
