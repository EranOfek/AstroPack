# Epoch proc Cat/Head MAT in visit folders

Canonical reference for **pipelineI-shaped** epoch catalogs on archive ingest and batch backfill (issue #1417). Distinct from **`+visitMat`** v3 `{visitId}_Cat.mat` under `{proc}/_mat/`.

## Purpose

A LAST visit has ~480 small **`LAST*proc_Cat_1.fits`** files. Reading them for ClickHouse ingest is slow. The pipeline can store **two MAT files per visit** (variables **`ProcCat`** and **`ProcHead`**) in the **visit directory**; ingest loads them with [`loadEpochCatMat`](../../+load/loadEpochCatMat.m) instead of 480 FITS opens.

## On-disk layout

Files live **only inside the visit folder** (`.../proc/000224v0/`). There is **no** `{proc}/_mat/` directory for this product.

### Per-crop FITS (many files)

Example visit:

`/bigdata2/projects/last/data/LAST.01.01.01/2026/01/06/proc/000224v0/`

Example catalog FITS basename:

`LAST.01.01.01_20260107.000224.401_clear_1355_001_001_001_sci_proc_Cat_1.fits`

### Merged epoch MAT (two files)

Same visit directory. Basenames use the **full LAST AstroFileName prefix** from the proc products, with **merged-product** fields (`Counter=0`, `CropID=0` → `_000_001_000_` in the name), same as [`PipelineDemon.saveEpochCatMat`](../../+pipes/@PipelineDemon/PipelineDemon.m):

```text
.../proc/003833v1/LAST.01.01.01_20261019.003833.999_clear_1347_000_001_000_sci_proc_Cat_1.mat
.../proc/003833v1/LAST.01.01.01_20261019.003833.999_clear_1347_000_001_000_sci_proc_Head_1.mat
```

| File | Variable | Content |
|------|----------|---------|
| `*_000_001_000_sci_proc_Cat_1.mat` | `ProcCat` | Plain struct grid: `Catalog`, `ColNames`, `ColUnits` per epoch×crop cell |
| `*_000_001_000_sci_proc_Head_1.mat` | `ProcHead` | Cell grid of header N×3 cells; `FILENAME` keywords name equivalent proc Cat FITS |

Archive write path: [`insertArchiveCatalogs2DB_perf.m`](../insertArchiveCatalogs2DB_perf.m) builds **`AstroFileName.genFile`** (via local `perfEpochMatMergedAstroFileName_`) and saves with [`imProc.io.saveProductMat`](../../../../+imProc/+io/saveProductMat.m) into **`DataDir`**, matching pipeline naming — **not** `{visitId}_000_001_000_...` alone.

Load templates (exactly one match per visit):

- `*_000_001_000_sci_proc_Cat_1.mat`
- `*_000_001_000_sci_proc_Head_1.mat`

## Perf ingest (`EPOCH_CAT_MAT`)

[`insertArchiveCatalogs2DB_perf.m`](../insertArchiveCatalogs2DB_perf.m):

| Mode | Behavior |
|------|----------|
| `none` (default) | FITS read + full insert |
| `write` | FITS read → save epoch MAT in visit dir → **`visitEnd epochMatWrite`** (no DB connect, no insert) |
| `load` | `loadEpochCatMat(DataDir, ...)` then same insert prep as FITS |

Shell ([`_run_insert_proc_last_perf.sh`](../scripts_wrx/lib/_run_insert_proc_last_perf.sh)):

- `EPOCH_CAT_MAT=write|load|none`
- `EPOCH_CAT_MAT_OVERWRITE=1` to replace existing Cat/Head pair

Example (one night):

```bash
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.01.01/2026/01/06/proc/*' \
EPOCH_CAT_MAT=write EPOCH_CAT_MAT_OVERWRITE=1 \
./scripts_euclid/run_insert_proc_last_perf_euclid.sh
```

## Batch backfill

[`writeArchiveEpochCatMat.m`](../writeArchiveEpochCatMat.m) calls perf with `EpochCatMat='write'` over a tree, verifies the first N visits (default 50), and writes four logs under `~/matlab/log/last/insertArchive/`:

- `insertDB_epoch_mat_build.log` — full job log
- `insertDB_epoch_mat_build_errors.log` — errors and warnings
- `insertDB_epoch_mat_build_visits.log` — visit paths (sorted)
- `insertDB_epoch_mat_build_visit_rows.log` — path + tab + total catalog rows (sorted)

Euclid example (January 2026, one LAST node):

```bash
INSERTDB_ARCHIVE_ROOT=/bigdata2/projects/last/data/ \
PROC_TEMPLATE='LAST.01.01.01/2026/01/*/proc/*' \
./scripts_euclid/run_write_archive_epoch_cat_mat_euclid.sh
```

## Operations

- **skipExists:** If both Cat and Head MAT files already exist and overwrite is false, perf logs `saveEpochCat skipExists` and skips the visit.
- **480 FITS warning:** Batch builder warns when proc Cat FITS count ≠ 480.
- **Headers ingest:** [`insertArchiveImageHeaders2DB.m`](../insertArchiveImageHeaders2DB.m) can use `*_sci_proc_Head_1.mat` in the visit dir when present.

## History

An earlier archive experiment wrote short-named MATs under `{proc}/_mat/`; that layout is **deprecated** and **not supported**. Re-run **`write`** with overwrite into visit dirs (or remove stale `_mat` trees manually).
