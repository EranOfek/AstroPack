# Hosts, MATLAB R2025b, and shell wrappers

Public notes for running LAST `insertDB` perf shells on **WRX80** vs **euclid**. No passwords or private keys in this doc.

## Topology

| Role | Host | Archive data | ClickHouse |
|------|------|--------------|------------|
| Cron / batch MATLAB (typical) | WRX80 | NFS mount `/bigdata2/projects/last/data/` | Network to host `euclid` (see `DbHost` in `*_perf.m`) |
| Archive + DB on same machine | euclid | Local `/bigdata2/projects/last/data/` | Local |

The LAST tree looks the same on both, e.g. `LAST.01.01.01/2026/01/06/proc/000224v0`.

## AstroPack checkout

Clone the repo on the machine where MATLAB runs, for example:

| Host | Example path |
|------|----------------|
| euclid (user `euclid`) | `/home/euclid/matlab/AstroPack` |
| WRX80 (e.g. user `sasha`) | `~/matlab/AstroPack` |

Wrappers derive `ASTROPACK_ROOT` from the script location when possible; override with `export ASTROPACK_ROOT=...` if needed.

## MATLAB R2025b

Use **`-nodisplay`** (not `nodispay`) and **`-sd <AstroPack>/matlab/startup`** so paths and `PasswordsManager` match an interactive session.

| Host | Default `MATLAB_BIN` (wrappers) |
|------|----------------------------------|
| WRX80 | `$HOME/Soft/M2025b/bin/matlab` |
| euclid | `/usr/local/MATLAB/R2025b/bin/matlab` |

Override anytime: `export MATLAB_BIN=/path/to/matlab`

### Optional interactive alias (euclid)

Adjust paths to your home and repo:

```bash
echo "alias matlab='/usr/local/MATLAB/R2025b/bin/matlab -nodisplay -sd /home/euclid/matlab/AstroPack/matlab/startup'" >> ~/.bashrc
```

### WRX80 example

```bash
~/Soft/M2025b/bin/matlab -nodisplay -sd ~/matlab/AstroPack/matlab/startup
```

## Shell layout

All bash drivers live under two subfolders of `+insertDB/` (no `.sh` files in the package root):

| Folder | Contents |
|--------|----------|
| `scripts_wrx/` | WRX80 wrappers, `insertdb_env_wrx80.sh`, shared `lib/` (`_run_*.sh`, `insertdb_run_matlab.sh`, legacy name forwarders) |
| `scripts_euclid/` | euclid wrappers and `insertdb_env_euclid.sh` (cores in `scripts_wrx/lib/`) |

## Which script to run

Base path: `matlab/image/+pipeline/+last/+insertDB/`

| Task | WRX80 | euclid |
|------|-------|--------|
| Quick smoke (one proc visit, no insert) | `scripts_wrx/run_smoke_proc_one_visit_wrx80.sh` | `scripts_euclid/run_smoke_proc_one_visit_euclid.sh` |
| Four-job perf / cron driver | `scripts_wrx/run_insert_archive_wrx80.sh` | `scripts_euclid/run_insert_archive_euclid.sh` |
| Proc overhead benchmark | `scripts_wrx/run_measure_proc_overhead_wrx80.sh` | `scripts_euclid/run_measure_proc_overhead_euclid.sh` |
| Full `last_perf` insert test | `scripts_wrx/run_test_last_perf_insert_wrx80.sh` | `scripts_euclid/run_test_last_perf_insert_euclid.sh` |
| Clone ClickHouse tables | -- | `clone_perf_tables.py` (package root) |

Legacy names in `scripts_wrx/lib/` (`run_insert_archive.sh`, etc.) forward to the WRX80 wrapper and print a hint.

## First run: smoke on both hosts

1. `cd ~/matlab/AstroPack/matlab/image/+pipeline/+last/+insertDB`
2. WRX80: `./scripts_wrx/run_smoke_proc_one_visit_wrx80.sh`
3. euclid: `./scripts_euclid/run_smoke_proc_one_visit_euclid.sh`

Default visit (~480 proc FITS):

```text
/bigdata2/projects/last/data/LAST.01.01.01/2026/01/06/proc/000224v0
```

Override: `export SMOKE_VISIT_DIR=/bigdata2/projects/last/data/.../proc/000224v0`

Smoke uses `SkipInsert=true`, `UpdateStatus=false`, `IgnoreStatus=true` (no ClickHouse rows, no `.status` edits). Check `~/matlab/log/last/insertArchive/insertDB_perf.log` for `visitEnd injected` and `AVG fullWork n=1`. Compare WRX80 (NFS) vs euclid (local disk) timings.

If `last_perf` is missing, DB connect may fail; FITS read still validates archive access. Create clones with [clickhouse_perf_tables.md](clickhouse_perf_tables.md).

## Environment overrides

| Variable | Purpose |
|----------|---------|
| `MATLAB_BIN` | MATLAB executable |
| `ASTROPACK_ROOT` | Repo root (default: derived or `~/matlab/AstroPack`) |
| `MATLAB_STARTUP_DIR` | Usually `$ASTROPACK_ROOT/matlab/startup` |
| `WORKDIR` | MATLAB working directory (default: `ASTROPACK_ROOT`) |
| `INSERTDB_ARCHIVE_ROOT` | Default `/bigdata2/projects/last/data/` |
| `SMOKE_VISIT_DIR` | Single visit for smoke scripts |
| `VISIT_DIR` / `TEST_SCOPE` | See [PLAN_test_last_perf_inserts.md](PLAN_test_last_perf_inserts.md) |

## Cron (WRX80)

See [cron.md](cron.md). Use `scripts_wrx/run_insert_archive_wrx80.sh` in crontab.

## Related

- [logging.md](logging.md) -- perf log path on whichever host runs MATLAB
- [README.md](README.md) -- ingest overview
