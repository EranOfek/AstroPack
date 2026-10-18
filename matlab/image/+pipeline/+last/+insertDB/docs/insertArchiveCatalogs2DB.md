# insertArchiveCatalogs2DB

**Source:** `matlab/image/+pipeline/+last/+insertDB/insertArchiveCatalogs2DB.m`  
**Perf clone:** `insertArchiveCatalogs2DB_perf.m`  
**MAT experiment (`{proc}/_mat/{visitId}_Cat.mat` + `{visitId}_Hdr.mat`):** `insertArchiveCatalogs2DB_perf_mat.m` — writer: `writeVisitCatalogs2Mat_perf.m`

## ClickHouse

| Property | Value |
|----------|--------|
| `Args.DbTable` | `proc_src` |
| Database | `last` |
| Perf structure clone | `last_perf.proc_src_perf` |

## Input

- **RootDir** -- archive root (WRX80: `/bigdata2/projects/last/data/`)
- **FileNameTemplate** -- default `LAST*proc_Cat_1.fits*`
- **ProcDirTemplate** -- e.g. `LAST.01.01.01/2026/01/*/proc/*`

## Processing

1. Connect native ClickHouse on `euclid`, `describe(proc_src)`.
2. Load column map from XLS sheet `Sources`.
3. Discover visit dirs (`v0`, exclude `re`).
4. Per visit: read `.status`; skip if injected or broken marker present.
5. `AstroCatalog` + `AstroHeader` (HDU 3, `UseMex` true).
6. Fill JD, NODENUMB, MOUNTNUM, SUBDIR, INGESTION_TIME_JD.
7. **`imProc.db.insertCatalog`** -> `last.proc_src` (native, batches 200000).
8. Append `.status` via `su euclid`.

## `.status` phrases

- Skip: `injected into the proc catalog DB`
- Skip: `not injectable into the proc catalog DB due to broken data files`

## Timed stages (`*_perf`)

DB connect, XLS read, `dir`, per processed visit: read FITS, keyword prep, `insertCatalog` (or skip), status update.
