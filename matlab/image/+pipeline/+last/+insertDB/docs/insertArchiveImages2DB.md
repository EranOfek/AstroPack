# insertArchiveImages2DB

**Source:** `matlab/image/+pipeline/+last/+insertDB/insertArchiveImages2DB.m`  
**Perf clone:** `insertArchiveImages2DB_perf.m`

## ClickHouse

| Property | Value |
|----------|--------|
| `Args.DbTable` | `visit_images` |
| Database | `last` |
| Perf structure clone | `last_perf.visit_images_perf` |
| `Args.ColNameID` | `id_visit` |

## Input

- **FileNameTemplate** -- default `LAST*coadd_Image_1.fits`
- **StatusStamp** -- default `Injected into the visit image table`

## Processing

1. `AstroImage` read; filter non-empty images.
2. Header keyword repair and DIRYEAR/DIRMON/DIRDAY from filename.
3. **`imProc.db.insertImages`** -> `last.visit_images` (not `insertCatalog`).

## `.status`

Skip when `.status` contains `Args.StatusStamp`.

## Timed stages (`*_perf`)

`AstroImage` read, keyword/filename prep, `insertImages`, status update.
