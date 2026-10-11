# LAST insertArchive — operations and performance docs

Post-processing ingest from LAST archive `proc/` visit folders on **euclid** into ClickHouse database **`last`**, driven from **WRX80**.

## Topology

| Role | Host | Notes |
|------|------|--------|
| Cron / MATLAB | WRX80 | Once daily (~10:00), four ingest types |
| Archive files | euclid | WRX80 path `/bigdata2/projects/last/data/` |
| ClickHouse | euclid | Host `euclid`, port `9000`, DB `last`, user `default` |

MATLAB passwords: `~/.astropack/Passwords.yml` via `PasswordsManager`.

## Daily job order

1. Proc catalogs → `last.proc_src` — see [insertArchiveCatalogs2DB.md](insertArchiveCatalogs2DB.md)
2. Coadd catalogs → `last.visit_src` — see [insertArchiveCoaddCatalogs2DB.md](insertArchiveCoaddCatalogs2DB.md)
3. Asteroids → `last.visit_asteroids` — see [insertArchiveAsteroids2DB.md](insertArchiveAsteroids2DB.md)
4. Visit images → `last.visit_images` — see [insertArchiveImages2DB.md](insertArchiveImages2DB.md)

Production entry points: `matlab/image/+pipeline/+last/+insertDB/insertArchive*.m` (no `_perf`).

Measurement entry points: `*_perf.m` clones and [run_insert_archive.sh](../run_insert_archive.sh).

## Related docs

- [changes.md](changes.md) — what was added and why
- [logging.md](logging.md) — `perfLog` format and flags
- [cron.md](cron.md) — shell script and crontab
- [clickhouse_perf_tables.md](clickhouse_perf_tables.md) — `last_perf` structure clones on euclid
- [folder_file_estimates.md](folder_file_estimates.md) — euclid `/bigdata2` visit and file counts, `dir` / `.status` overhead
- [PLAN_test_last_perf_inserts.md](PLAN_test_last_perf_inserts.md) — planned test shells + `last_perf` inserts (not implemented yet)

Parent package overview: [../README.md](../README.md).
