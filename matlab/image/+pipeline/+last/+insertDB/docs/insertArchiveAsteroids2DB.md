# insertArchiveAsteroids2DB

**Source:** `matlab/image/+pipeline/+last/+insertDB/insertArchiveAsteroids2DB.m`  
**Perf clone:** `insertArchiveAsteroids2DB_perf.m`

## ClickHouse

| Property | Value |
|----------|--------|
| `Args.DbTable` | `visit_asteroids` |
| Database | `last` |
| Perf structure clone | `last_perf.visit_asteroids_perf` |

## Input

- **FileNameTemplate** — default `LAST*coadd_Aster*.mat`
- **RemoteUser** — default `samar` (`su - samar` for `.status`)

## Processing

1. Load MAT table, split by `CropID`.
2. Read coadd Cat FITS headers per crop.
3. **`imProc.db.insertCatalog`** on struct array → `last.visit_asteroids`.

## `.status` phrase

- Skip: `injected into the visit asteriods catalog DB`

## Timed stages (`*_perf`)

MAT load, header reads, keyword prep, `insertCatalog`, status update.
