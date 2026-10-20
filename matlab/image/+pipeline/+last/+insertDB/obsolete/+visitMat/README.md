# Visit MAT (v3) — `pipeline.last.insertDB.visitMat`

FITS → compact MAT cache under `{proc}/_mat/`, then mat → ClickHouse via the same `insertCatalog_perf` path as FITS ingest. Production `insertCatalog` is unchanged.

## Files per visit

| Path | Role |
|------|------|
| `{proc}/_mat/{visitId}_Cat.mat` | Catalog payload |
| `{proc}/_mat/{visitId}_Hdr.mat` | HDU-3 header payload |

Both use **MAT `-v7.3`** and **`InsertDbMatVersion = 3`**. v1 (full `Cat`/`AH` objects) and v2 (`Catalog` cell / `HdrData` cell) are **rejected on load** — re-dump with `OverWrite=true` / `OVERWRITE=1`.

### `_Cat.mat`

- `CatTable` — one table, all sources; column **`CropIdx`** (`uint16`, 1…N crops)
- `ColNames`, `ColUnits` — shared across crops

### `_Hdr.mat`

- `HdrKeys` — K×1 string (keyword names, same order on every crop)
- `HdrValStr`, `HdrCommentStr` — N×K string matrices (crop × keyword)

Load rebuilds `AstroCatalog` / `AstroHeader` (dictionaries from config at runtime):

- **`loadVisitCatalogMat`:** one `table2array`, split by `CropIdx` into numeric matrices, single `AstroCatalog(cell-of-matrices, …)` (avoids 480 table filters + per-crop object setup).
- **`loadVisitHeaderMat`:** vectorized N×K string → value cells, then attach shared dictionaries.

MAT replaces **FITS read only**. **`insertCatalog_perf`** (`tableBuild`, `baryJD`, healpix, DB) is unchanged — mat does not skip per-crop table assembly.

### FITS vs mat (same visit, euclid)

Use the same flags and visit (`000224v0` is the default smoke path). Compare **`insertDB_perf.log`**:

| Path | Shell | Read stages |
|------|--------|-------------|
| FITS | `run_insert_proc_last_perf_euclid.sh` | `readAstroCatalog`, `readAstroHeader` |
| MAT | `run_insert_proc_last_perf_mat_euclid.sh` | `loadMatCat`, `loadMatHdr` |

Prep-only (no ClickHouse rows):

```bash
RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.01.01/2026/01/06/proc/000224v0' \
MEASURE_CATALOG_ASTRO_PERF=1 MEASURE_SKIP_DB_INSERT=1 \
./scripts/euclid/run_insert_proc_last_perf_mat_euclid.sh

RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.01.01/2026/01/06/proc/000224v0' \
MEASURE_CATALOG_ASTRO_PERF=1 MEASURE_SKIP_DB_INSERT=1 \
./scripts/euclid/run_insert_proc_last_perf_euclid.sh
```

On euclid warm local disk, documented FITS MEX read is often **~2.7 s** combined (`readAstroCatalog` ~1.4 s + `readAstroHeader` ~1.3 s — see [../docs/euclid_vs_wrx_catalog_read_timing.md](../docs/euclid_vs_wrx_catalog_read_timing.md)). Mat load should be **below that** to win on read; **`tableBuild`** / **`baryJD`** should match between the two runs (`nRows` must match).

## Entry points

| MATLAB | Shell (WRX / euclid) |
|--------|----------------------|
| `visitMat.writeVisitCatalogs2Mat_perf` | `run_write_visit_catalogs2mat_{wrx80,euclid}.sh` |
| `visitMat.insertArchiveCatalogs2DB_perf_mat` | `run_insert_proc_last_perf_mat_{wrx80,euclid}.sh` |

I/O: `saveVisitCatalogMat`, `saveVisitHeaderMat`, `loadVisitCatalogMat`, `loadVisitHeaderMat`, `assertVisitMatFileVersion`, `visitMatFileVersion`, `perfDiscoverProcVisitDirs`.

Shared logging: `pipeline.last.insertDB.perfLog`, `perfVisitStats`.

## Ops (euclid)

After upgrading to v3, **re-dump all nights** with `OVERWRITE=1`. Smoke one visit:

```matlab
whos('-file','/bigdata2/.../proc/_mat/000224v0_Cat.mat')
% Expect: CatTable, ColNames, ColUnits, InsertDbMatVersion (not Catalog)

whos('-file','/bigdata2/.../proc/_mat/000224v0_Hdr.mat')
% Expect: HdrKeys, HdrValStr, HdrCommentStr (not HdrData); file size ≪ 100 MB
```

See [../docs/hosts_and_matlab.md](../docs/hosts_and_matlab.md#visit-mat-dump-and-mat-ingest-euclid) and [../docs/logging.md](../docs/logging.md).
