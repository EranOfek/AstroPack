# insertArchiveCoaddCatalogs2DB

**Source:** `matlab/image/+pipeline/+last/+insertDB/insertArchiveCoaddCatalogs2DB.m`  
**Perf clone:** `insertArchiveCoaddCatalogs2DB_perf.m`

## ClickHouse

| Property | Value |
|----------|--------|
| `Args.DbTable` | `visit_src` |
| Database | `last` |
| Perf structure clone | `last_perf.visit_src_perf` |
| Key IDs | `KeyID=id_visit_im`, `ColNameID=id_visit_src` |

## Input

- **FileNameTemplate** — default `LAST*coadd_Cat_1.fits`

## Processing

Same walk as proc catalogs; **`imProc.db.insertCatalog`** with visit source IDs. Native path updates `.status` with `su euclid`.

## `.status` phrases

- Skip: `injected into the visit catalog DB`
- Skip: `not injectable into the visit catalog DB due to broken data files`

## Timed stages (`*_perf`)

Same pattern as proc; coadd uses `AstroCatalog` without `UseMex` in the original.
