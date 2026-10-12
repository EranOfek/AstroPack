# Plan: test shells -- `last_perf` clones and full insert benchmark



**Status:** implemented.



Copy of the Cursor plan for this work. Paths use `/` relative to repo root.



## Todos



- [x] Add `DbPassKey`, `ColMapTableName` (and `ProcDirList` on coadd/images/asteroids) to all four `*_perf.m`

- [x] Update `run_measure_proc_overhead.sh` with `last_perf` DB args; keep `SkipInsert` true

- [x] Add `run_test_last_perf_insert.sh` -- four jobs, insert to `last_perf`, `TEST_SCOPE` visit|night|month

- [x] Document both test shells vs cron in `clickhouse_perf_tables.md` (+ short logging note)



Production cron: `scripts_wrx/run_insert_archive_wrx80.sh` -> `last.*` when `ALLOW_WRITES=1`. Host wrappers: [hosts_and_matlab.md](hosts_and_matlab.md).



Prerequisite on euclid: `matlab/image/+pipeline/+last/+insertDB/clone_perf_tables.py` created `last_perf` and `*_perf` tables.



## Two test shells (not for cron)



| Script | Purpose | ClickHouse insert | Archive `.status` |

|--------|---------|-------------------|-------------------|

| `run_measure_proc_overhead.sh` | Proc only; I/O + build rows, **no insert** | `SkipInsert=true` | `UpdateStatus=false` |

| **New** `run_test_last_perf_insert.sh` | All **four** `*_perf` jobs end-to-end **including insert** | `SkipInsert=false` -> `last_perf.*_perf` | `UpdateStatus=false` |



Both use clone DB/table names and **`IgnoreStatus=true`**.



## ClickHouse mapping (all four scripts)



| Job | `DbName` | `DbTable` | `ColMapTableName` (XLS key) | `DbPassKey` |

|-----|----------|-----------|-----------------------------|-------------|

| insertArchiveCatalogs2DB_perf | `last_perf` | `proc_src_perf` | `proc_src` | `last` |

| insertArchiveCoaddCatalogs2DB_perf | `last_perf` | `visit_src_perf` | `visit_src` | `last` |

| insertArchiveAsteroids2DB_perf | `last_perf` | `visit_asteroids_perf` | `visit_asteroids` | `last` |

| insertArchiveImages2DB_perf | `last_perf` | `visit_images_perf` | `visit_images` | `last` |



---



## Implementation reference (for Agent apply)



### MATLAB -- all four `*_perf.m`



**Arguments** (add near other `Db*` fields):



```matlab

Args.DbPassKey       = '';

Args.ColMapTableName = '';

```



Coadd / asteroids / images also add:



```matlab

Args.ProcDirList = [];

```



**Password** (replace `Pwd = PM.search(Args.DbName).Pass;`):



```matlab

if ~isempty(Args.DbPassKey)

    Pwd = PM.search(Args.DbPassKey).Pass;

else

    Pwd = PM.search(Args.DbName).Pass;

end

```



**perfLog after connect:**



```matlab

perfLog(sprintf('%s dbConnect DbName=%s DbTable=%s', FnName, Args.DbName, Args.DbTable), toc(TicDb));

```



**XLS column map** (Sources sheet for proc/coadd/asteroids; Images for images):



```matlab

TableNameForXls = Args.DbTable;

if ~isempty(Args.ColMapTableName)

    TableNameForXls = Args.ColMapTableName;

end

Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Sources','TableName',TableNameForXls);

```



**ProcDirList** -- copy from `insertArchiveCatalogs2DB_perf.m` (`dir` vs list + `DataDir = Dirs{Idir}` for list). Images filter stays `'_re'` not `'re'`.



### `run_measure_proc_overhead.sh`



Extend `MEASURE_ARGS`:



```bash

MEASURE_ARGS="'SkipInsert',true,'UpdateStatus',false,'IgnoreStatus',true,'DbName','last_perf','DbTable','proc_src_perf','DbPassKey','last','ColMapTableName','proc_src'"

```



Echo `ClickHouse target: last_perf.proc_src_perf` in the banner.



### New `run_test_last_perf_insert.sh`



```bash

#!/usr/bin/env bash

# Test driver: all four *_perf jobs with inserts into last_perf only.

# UpdateStatus=false, IgnoreStatus=true. Not for cron.

set -u



MATLAB_BIN="${MATLAB_BIN:-/usr/local/MATLAB/R2025b/bin/matlab}"

WORKDIR="${WORKDIR:-$(pwd)}"

TEST_SCOPE="${TEST_SCOPE:-visit}"

ROOT="/bigdata2/projects/last/data/"

VISIT_DIR="${VISIT_DIR:-${ROOT}LAST.01.10.04/2026/01/28/proc/194924v0}"

PROC_ONE_NIGHT="LAST.01.10.04/2026/01/28/proc/*"

PROC_ALL_MONTH="LAST.01.*.*/2026/01/*/proc/*"



COMMON_NV="'SkipInsert',false,'UpdateStatus',false,'IgnoreStatus',true,'DbName','last_perf','DbPassKey','last'"



case "${TEST_SCOPE}" in

  visit)

    SCOPE_NV="'ProcDirList',{'${VISIT_DIR}'}" ;;

  night)

    SCOPE_NV="'ProcDirTemplate','${PROC_ONE_NIGHT}'" ;;

  month)

    SCOPE_NV="'ProcDirTemplate','${PROC_ALL_MONTH}'" ;;

  *)

    echo "TEST_SCOPE must be visit|night|month (got ${TEST_SCOPE})" >&2

    exit 2 ;;

esac



run_job() {

  local Label="$1" Fn="$2" DbTable="$3" ColMap="$4"

  local BatchCmd="pipeline.last.insertDB.${Fn}('${ROOT}',${SCOPE_NV},${COMMON_NV},'DbTable','${DbTable}','ColMapTableName','${ColMap}')"

  echo "=== ${Label} -> last_perf.${DbTable} start $(date -Iseconds) ==="

  if ( cd "${WORKDIR}" && "${MATLAB_BIN}" -nodisplay -nosplash -batch "${BatchCmd}" ); then

    echo "=== ${Label} done $(date -Iseconds) ==="

    return 0

  fi

  echo "=== ${Label} FAILED $(date -Iseconds) ===" >&2

  return 1

}



OverallFail=0

run_job "1/4 proc catalogs" insertArchiveCatalogs2DB_perf proc_src_perf proc_src || OverallFail=1

run_job "2/4 coadd catalogs" insertArchiveCoaddCatalogs2DB_perf visit_src_perf visit_src || OverallFail=1

run_job "3/4 asteroids" insertArchiveAsteroids2DB_perf visit_asteroids_perf visit_asteroids || OverallFail=1

run_job "4/4 visit images" insertArchiveImages2DB_perf visit_images_perf visit_images || OverallFail=1

exit "${OverallFail}"

```



### Docs



- `clickhouse_perf_tables.md`: remove "follow-up" line; add sections for measure vs full-insert shells.

- `logging.md`: one paragraph on `TEST_SCOPE` and checking `last_perf` row counts.



## Manual verification (euclid)



1. `python3 clone_perf_tables.py` if needed.

2. `./run_test_last_perf_insert.sh` (default one visit).

3. `~/matlab/log/last/insertArchive/insertDB_perf.log`

4. `clickhouse-client` -- `SELECT count() FROM last_perf.proc_src_perf` (etc.).

5. Archive `.status` unchanged.



## Out of scope



- `scripts_wrx/lib/run_insert_archive.sh` (legacy forwarder) and production `last.*` targets when writes enabled.

- Auto-`TRUNCATE` on `last_perf`.



---



## Per-folder timing averages



**Status:** implemented ([perfVisitStats.m](../perfVisitStats.m)).



**Goal:** Keep all existing per-stage `perfLog` lines. At **`FINISH`**, add one **`AVG`** summary line (and optional **`AVG detail`**) so month-scale runs are readable without parsing thousands of `visitEnd` lines.



### Definitions (per visit folder = one `proc/*v0` directory)



| Metric | Meaning | How to measure in code |

|--------|---------|-------------------------|

| **visitTotal** | Wall time for everything done inside the visit loop for that folder | Existing `TicVisit` -> `toc(TicVisit)` on every `visitEnd ...` path (and any new paths that `continue` without a line today) |

| **visitInsert** | ClickHouse insert only | Sum of `toc(TicInsert)` over all `insertCatalog` / `insertImages` calls in that visit (0 when `SkipInsert=true`) |

| **visitNonDb** | Everything else in the visit | **`visitTotal - visitInsert`** (includes `readAstroCatalog` / `readAstroHeader` / `readImages`, `keywordPrep`, `readStatus`, optional decompress/compress, `updateStatus` via `su`) |



**Not** subtracting `updateStatus` from **visitNonDb** in v1 -- that keeps one simple "DB vs rest" split. If `UpdateStatus=false` in test shells, **visitNonDb** is pure I/O + MATLAB prep.



### Which folders go into the average (recommended)



Assuming proc visits have **similar file counts**, use **two cohorts** in the summary log:



1. **`fullWork` (primary -- use this to compare behaviour)**  

   Visits that **read data and finished the inject path**: emitted `visitEnd injected` (including `SkipInsert` after prep).  

   Excludes: already-injected skip (no timing today), `noStatus`, `noData`, `broken`, early `brokenFirstCatalog`, empty asteroid table, etc.  

   **Interpretation:** "Typical cost of processing one heavy visit folder."



2. **`allTimed` (secondary -- sanity check)**  

   Every visit that logged **`visitEnd ... [Xs]`** with any outcome tag.  

   **Interpretation:** Mixes cheap failures with full work; **mean will be lower** than `fullWork` if many broken/empty dirs appear in the template.



**Recommendation:** Report **`fullWork` means as the headline**; add **`nFullWork`** sample size. Optionally one line for `allTimed` if `nAllTimed > nFullWork`.



**Skipped-already-injected:** Today these increment `CntSkippedInjected` without `visitEnd` timing. For averages they should stay **out of `fullWork`**. Optional follow-up: short `visitEnd skippedInjected` + timing if you want "cost of entering every folder."



### Best average to report (headline + robustness)



When folders are ~same size (~480 proc Cat FITS per visit):



- **Arithmetic mean** of **visitTotal**, **visitInsert**, **visitNonDb** over the **`fullWork`** cohort -- matches "seconds per visit folder" for capacity planning (`N_visit x meanVisitTotal` ~ batch time minus startup).



Also log (same cohort, minimal extra code):



- **median** of **visitTotal** -- robust to a few NFS outliers  

- **p90** of **visitTotal** -- tail behaviour without parsing the whole log  



Optional later (if `Nobj` / file counts vary): **mean(visitTotal / Nobj)** or **sum(visitTotal) / sum(Nobj)** -- only if you see spread in logged `Nobj=` lines; not required for v1.



### Example summary lines (append after existing `SUMMARY`)



```

insertArchiveCatalogs2DB_perf AVG fullWork n=30500 visitTotal mean=42.3s med=38.1s p90=55.2s visitInsert mean=8.1s visitNonDb mean=34.2s

insertArchiveCatalogs2DB_perf AVG allTimed n=30747 visitTotal mean=41.0s visitInsert mean=7.9s visitNonDb mean=33.1s

```



When **`SkipInsert=true`**, expect **visitInsert mean~0** and **visitNonDb ~ visitTotal** -- useful for `run_measure_proc_overhead.sh`.



### Implementation (all four `*_perf.m` + small helper)



**Prefer one shared helper** in the package (avoid four copy-paste stat blocks):



- New [perfVisitStats.m](../perfVisitStats.m) (names flexible):  

  - `S = perfVisitStats('init')`  

  - `S = perfVisitStats('startVisit')` -> returns visit handle / resets per-visit insert accumulator  

  - `S = perfVisitStats('addInsert', S, sec)` on each successful insert timing  

  - `S = perfVisitStats('endVisit', S, totalSec, outcome)` where `outcome` is `'injected'`, `'noData'`, `'broken'`, ...  

  - `perfVisitStats('logSummary', S, FnName)` -> writes `AVG` lines via `pipeline.last.insertDB.perfLog`



**Loop changes (each clone):**



- Initialize `S` after `FnName` / counters.  

- Per visit: `visitInsertSec = 0` at `visitStart`; after each `insertCatalog`/`insertImages` log, add `toc(TicInsert)` to `visitInsertSec`.  

- On **every** exit path that already calls `visitEnd ...`, call `endVisit(toc(TicVisit), visitInsertSec, tag)` before or after the existing `perfLog`.  

- Before `FINISH`, call `logSummary`.



**Docs:** Extend [logging.md](logging.md) with metric definitions and cohort names.



### Todos (averages)



- [x] Add `perfVisitStats.m` (init / endVisit / logSummary with mean, median, p90)  

- [x] Wire `insertArchiveCatalogs2DB_perf.m` (reference; most visit exit paths)  

- [x] Wire coadd, asteroids, images `*_perf.m` (same pattern; insert fn name differs for images)  

- [x] Document `AVG` lines in `logging.md`

