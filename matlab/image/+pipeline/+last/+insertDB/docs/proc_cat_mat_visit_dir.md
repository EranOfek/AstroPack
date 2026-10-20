# Epoch proc Cat/Head MAT in visit folders

Canonical reference for **pipelineI-shaped** epoch catalogs on archive ingest and batch backfill (issue #1417). Distinct from **`+visitMat`** v3 `{visitId}_Cat.mat` under `{proc}/_mat/`.

## Purpose

A LAST visit has ~480 small **`LAST*proc_Cat_1.fits`** files. Reading them for ClickHouse ingest is slow. The pipeline can store **two MAT files per visit** (variables **`ProcCat`** and **`ProcHead`**) in the **visit directory**; ingest loads them with [`loadProcCatMat`](../../+load/loadProcCatMat.m) instead of 480 FITS opens.

## On-disk layout

Files live **only inside the visit folder** (`.../proc/000224v0/`). There is **no** `{proc}/_mat/` directory for this product.

### Per-crop FITS (many files)

Example visit:

`/bigdata2/projects/last/data/LAST.01.01.01/2026/01/06/proc/000224v0/`

Example catalog FITS basename:

`LAST.01.01.01_20260107.000224.401_clear_1355_001_001_001_sci_proc_Cat_1.fits`

### Merged epoch MAT (two files)

Same visit directory. Basenames use the **full LAST prefix** from the first sorted proc Cat FITS, with the crop index triple replaced by merged **`_000_001_000_`** and **`sci_proc`** product tails (same on-disk shape as [`PipelineDemon.saveProcCatMat`](../../+pipes/@PipelineDemon/PipelineDemon.m)):

```text
.../proc/003833v1/LAST.01.01.01_20261019.003833.999_clear_1347_000_001_000_sci_proc_Cat_1.mat
.../proc/003833v1/LAST.01.01.01_20261019.003833.999_clear_1347_000_001_000_sci_proc_Head_1.mat
```

| File | Variable | Content |
|------|----------|---------|
| `*_000_001_000_sci_proc_Cat_1.mat` | `ProcCat` | Plain struct grid: `Catalog`, `ColNames`, `ColUnits` per epoch×crop cell |
| `*_000_001_000_sci_proc_Head_1.mat` | `ProcHead` | Cell grid of header N×3 cells; `FILENAME` keywords name equivalent proc Cat FITS |

**Archive write** ([`insertArchiveCatalogs2DB_perf.m`](../+perf/insertArchiveCatalogs2DB_perf.m)): list the first sorted catalog FITS in the visit, derive Cat/Head `.mat` basenames via regex (triple → `_000_001_000_`), then [`imProc.io.saveProductMat`](../../../../+imProc/+io/saveProductMat.m) with **char** filenames and `'Path', DataDir` (no `AstroFileName` in the save call).

Load templates (exactly **one** match per visit in the folder):

- `*_sci_proc_Cat_1.mat`
- `*_sci_proc_Head_1.mat`

## Perf ingest (`PROC_CAT_MAT`)

[`insertArchiveCatalogs2DB_perf.m`](../+perf/insertArchiveCatalogs2DB_perf.m):

| Mode | Behavior |
|------|----------|
| `none` (default) | FITS read + full insert |
| `write` | FITS read → save epoch MAT in visit dir → **`visitEnd procMatWrite`** (no DB connect, no insert) |
| `load` | `loadProcCatMat(DataDir, ...)` then same insert prep as FITS |

Shell ([`_run_insert_proc_last_perf.sh`](../scripts/common/_run_insert_proc_last_perf.sh)):

- `PROC_CAT_MAT=write|load|none`
- `PROC_CAT_MAT_OVERWRITE=1` to replace existing Cat/Head pair

Example (one night):

```bash
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.01.01/2026/01/06/proc/*' \
PROC_CAT_MAT=write PROC_CAT_MAT_OVERWRITE=1 \
./scripts/euclid/run_insert_proc_last_perf_euclid.sh
```

## Batch backfill

[`+create_proc_mat/writeArchiveProcCatMat.m`](../+create_proc_mat/writeArchiveProcCatMat.m) calls perf with `ProcCatMat='write'` over a tree, verifies the first N visits (default 50), and writes four logs under `~/matlab/log/last/insertArchive/`:

- `insertDB_proc_mat_build.log` — full job log (including all `perfLog` lines for the batch run; not `insertDB_perf.log`)
- `insertDB_proc_mat_build_errors.log` — errors and warnings
- `insertDB_proc_mat_build_visits.log` — visit paths (**sorted**; updated after **each** visit)
- `insertDB_proc_mat_build_visit_rows.log` — path + tab + total catalog rows (**sorted**; updated after **each** visit)

Sidecar updates use [`+create_proc_mat/procMatBuildSidecarLogs.m`](../+create_proc_mat/procMatBuildSidecarLogs.m): before replacing each sidecar file, the previous file is renamed to **`.bak`**.

**Euclid load:** default **`VisitIdleSec=10`** — pause between visits during batch (perf default 0 for normal ingest). Override via MATLAB arg or shell `VISIT_IDLE_SEC=0` for full throughput.

Euclid example (January 2026, one LAST node):

```bash
INSERTDB_ARCHIVE_ROOT=/bigdata2/projects/last/data/ \
PROC_TEMPLATE='LAST.01.01.01/2026/01/*/proc/*' \
./+create_proc_mat/euclid_run_write_archive_proc_cat_mat.sh
```

## Operations

- **Re-run without overwrite (`OVERWRITE=0`):** Before reading FITS, perf checks the visit folder for exactly one non-empty `*_sci_proc_Cat_1.mat` and one `*_sci_proc_Head_1.mat`. If both exist, it logs **`visitSkipProcMatExists`**, ends with **`visitEnd procMatSkipExists`**, and does **not** open proc Cat FITS. Skip logic uses **on-disk MAT files only** (not the visits sidecar log). With `CatalogAstroPerf`, row counts for sidecars come from a single **`loadProcCatMat`** on the existing pair.
- **Late skipExists:** If the early check does not apply (e.g. ambiguous glob), after a FITS read perf may still log **`saveProcCat skipExists`** and skip writing MATs without overwriting.
- **480 FITS warning:** Batch builder warns when proc Cat FITS count ≠ 480.
- **Headers ingest:** [`insertArchiveImageHeaders2DB.m`](../insertArchiveImageHeaders2DB.m) can use `*_sci_proc_Head_1.mat` in the visit dir when present.

## Verification (euclid)

Single visit write:

```bash
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.01.01/2026/01/06/proc/000224v0' \
PROC_CAT_MAT=write PROC_CAT_MAT_OVERWRITE=1 \
./scripts/euclid/run_insert_proc_last_perf_euclid.sh
```

Expect `saveProcCat` / `saveProcHdr`, `visitEnd procMatWrite`, and exactly one `*_sci_proc_Cat_1.mat` and `*_sci_proc_Head_1.mat` in the visit folder (merged `_000_001_000_` in the basename).

Batch re-run (`OVERWRITE=0`): expect `visitSkipProcMatExists` and `skipProcMatExists=N` in SUMMARY, no `readAstroCatalog` for skipped visits. Optional: `VISIT_IDLE_SEC=0` for faster re-run.

## History

An earlier archive experiment wrote short-named MATs under `{proc}/_mat/`; that layout is **deprecated** and **not supported**. Re-run **`write`** with overwrite into visit dirs (or remove stale `_mat` trees manually).
