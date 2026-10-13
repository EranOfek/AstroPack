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
| Proc overhead benchmark (no insert) | `scripts_wrx/run_measure_proc_overhead_wrx80.sh` | `scripts_euclid/run_measure_proc_overhead_euclid.sh` |
| Proc insert into `last_perf` (same phases as overhead) | `scripts_wrx/run_insert_proc_last_perf_wrx80.sh` | `scripts_euclid/run_insert_proc_last_perf_euclid.sh` |
| Full `last_perf` insert test (all four jobs) | `scripts_wrx/run_test_last_perf_insert_wrx80.sh` | `scripts_euclid/run_test_last_perf_insert_euclid.sh` |
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

## WRX80 vs euclid proc timing

Use the same driver on both hosts (measure-only or insert twin):

- `scripts_wrx/run_measure_proc_overhead_wrx80.sh` / `scripts_euclid/run_measure_proc_overhead_euclid.sh` (`SkipInsert`, no rows)
- `scripts_wrx/run_insert_proc_last_perf_wrx80.sh` / `scripts_euclid/run_insert_proc_last_perf_euclid.sh` (insert into `last_perf.proc_src_perf` only)

Default scope: phase1 one night (`LAST.01.10.04/2026/01/28/proc/*`). Override with `PROC_ONE_NIGHT`, `RUN_PHASE2=0` to skip the full month.

**Bash pitfall:** variables set on one line and the script run on the **next** line are **not** passed to the child unless you `export` them. Use a **one-line prefix** or `measure_proc_overhead.env` (see below).

Measure-overhead env (defaults in [`_run_measure_proc_overhead.sh`](../scripts_wrx/lib/_run_measure_proc_overhead.sh)):

| Variable | Default | MATLAB flag | Purpose |
|----------|---------|-------------|---------|
| `MEASURE_CATALOG_FILE_INVENTORY` | 0 | `CatalogFileInventory` | Log `n` and total MiB per visit |
| `MEASURE_CATALOG_RAW_FILE_READ` | 0 | `CatalogRawFileRead` | Timed raw `fread` before `AstroCatalog` |
| `MEASURE_CATALOG_ASTRO_PERF` | 1 | `CatalogAstroPerf` | Row-based **`catalogAstroPerf`** / **`AVG catalogAstroPerf`** |

Cross-host read benchmark: compare **`AVG catalogAstroPerf`** **`rowsPerSec weighted`** and **med** (not raw fread). Set inventory/raw to `1` only for legacy MiB/s **`AVG catalogReadRates`** tests.

### Cold compare: disjoint months (euclid vs WRX)

To reduce **cross-host** cache contamination (euclid reads a night, then WRX reads the same paths over NFS and hits warm server cache), use **different archive months** on each host. Current read-benchmark examples: euclid **2026/06/12**, WRX **2026/07/12** (see `measure_proc_overhead.env.example`). Same telescope ID in `PROC_ONE_NIGHT` when possible.

From repo `+insertDB/scripts_euclid` (euclid) or `+insertDB/scripts_wrx` (WRX):

```bash
# Correct: env vars on the SAME command as the script
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.10.04/2026/06/12/proc/*' ./run_measure_proc_overhead_euclid.sh

# Also correct: export then run
export RUN_PHASE2=0
export PROC_ONE_NIGHT='LAST.01.10.04/2026/07/12/proc/*'
./run_measure_proc_overhead_wrx80.sh
```

After a multi-minute run, compare **`AVG catalogAstroPerf`** (**`rowsPerSec weighted`** and **med**) and **`ENV host=`** in `insertDB_perf.log` — not a single visit line.

**Wrong** (script keeps Jan default — banner shows `2026/01/28`):

```bash
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.10.04/2026/02/15/proc/*'
./run_measure_proc_overhead_euclid.sh
```

**Persistent override:** copy `measure_proc_overhead.env.example` → `measure_proc_overhead.env` in the same host scripts folder (auto-sourced by **measure** and **insert** wrappers; not committed).

Adjust day/telescope paths to match data on disk. Confirm the banner line `ProcDirTemplate=...` matches your intended night before letting a long run continue.

### Insert benchmark (`last_perf.proc_src_perf` only)

Real ClickHouse inserts via `run_insert_proc_last_perf_{euclid,wrx80}.sh` — **`DbName=last_perf`**, **`DbTable=proc_src_perf`** hard-coded in [`_run_insert_proc_last_perf.sh`](../scripts_wrx/lib/_run_insert_proc_last_perf.sh) (not production `last.proc_src`). `UpdateStatus=false`, `IgnoreStatus=true`.

**Suggested nights (disjoint from Feb/Mar read-only runs):**

| Host | `PROC_ONE_NIGHT` (verify on disk) |
|------|-----------------------------------|
| euclid | `LAST.01.10.04/2026/04/12/proc/*` |
| WRX80 | `LAST.01.10.04/2026/05/12/proc/*` |

Use `measure_proc_overhead.env` (see examples under `scripts_euclid/` and `scripts_wrx/`) or one-line prefix. Same **`MEASURE_CATALOG_*`** flags as measure-overhead (default on) for `catalogReadRates` / `insertCatalog` lines.

**Pre-flight (euclid):** `clickhouse-client -q "EXISTS TABLE last_perf.proc_src_perf"`. Re-running the same night duplicates rows — `TRUNCATE TABLE last_perf.proc_src_perf` before re-test if needed.

**Compare in log:** `AVG fullWork` → `visitInsert`, `visitNonDb`, `visitTotal`; per-visit `rawSecPerMB`, `catSecPerMB`; **`insertCatalog`** lines with `fieldId`, `nCat`, `nRows`, **`srcPerCat`**, `secPer100k`; run **`AVG insertCatalog`** (`srcPerCat` min/max, weighted `secPer100k`). Do not compare insert times across hosts unless **`srcPerCat` / `nRows`** are in a similar range (field density differs). Both hosts use **`DbHost=euclid`** in MATLAB — WRX pays NFS for FITS **and** network for ClickHouse insert; euclid is local disk + local CH. Insert timer is client-side only (CH background merges excluded); see [logging.md](logging.md#proc-insertcatalog-density-and-row-rates-proc-perf-clone-only).

```bash
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.10.04/2026/04/12/proc/*' ./run_insert_proc_last_perf_euclid.sh
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.10.04/2026/05/12/proc/*' ./run_insert_proc_last_perf_wrx80.sh
```

**What this does not clear:** Linux page cache on the host running MATLAB (later visits in the same job stay warm), or RAID controller cache on euclid for data that host already read. Compare **`AVG catalogReadRates`** across hosts after a full night, not one folder. Optional ops-only: `sync` + `echo 3 | sudo tee /proc/sys/vm/drop_caches` before a short smoke (agree with ops first).

### Why NFS catalog read can look as fast as local

Proc ingest logs **`readAstroCatalog`** and **`readAstroHeader`** separately (~480 small Cat FITS plus MEX decode; see [folder_file_estimates.md](folder_file_estimates.md)). WRX is not always slower than euclid on warm cache or a faster CPU. A pair of visits with similar stage times does not prove NFS is free.

### Controlled comparison protocol

1. **Aggregate, do not cherry-pick.** After a full phase1 run on each host, compare `AVG fullWork` lines in `insertDB_perf.log` (median `visitTotal`, cohort `fullWork`). Prefer medians over two folders.
2. **Warm repeat (same host).** Run phase1 twice back-to-back on WRX80 (or euclid). If the second pass is much faster, cache dominated the first run.
3. **Cross-host order.** Reading the same night on euclid then WRX (your order) warms server-side cache for WRX. For a stricter test, use a night neither host has read recently, or compare WRX-first vs euclid-first on different nights.
4. **Cold cache (ops only, if permitted).** Optional: drop client page cache before a run; never do this on a shared production host without agreement.
5. **Optional raw I/O (ops).** On one visit directory, time plain read of all `*proc_Cat_1.fits*` from WRX mount vs from euclid local path (without MATLAB) to separate filesystem from MEX parse.
6. **Heavier products.** If NFS cost matters for cron sizing, repeat with coadd/images `*_perf` jobs or `run_test_last_perf_insert_*` (larger FITS per visit).

### What to record

| Field | Where |
|-------|--------|
| Host | `INSERTDB_RUN_HOST` in shell banner; `ENV host=...` in log |
| MATLAB | `ENV matlabRelease=... matlabVersion=...` in log |
| Cohort stats | `AVG fullWork` / `AVG allTimed` at end of run |
| Per-stage | `catalogFileInventory`, `readCatalogFilesRaw`, `readAstroCatalog`, `readAstroHeader`, `catalogReadRates`, `visitEnd` |
| Throughput | `mbPerSec=` on raw/Cat lines; per-visit `catalogReadRates`; run **`AVG catalogReadRates`** |

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
