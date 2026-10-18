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

Load rebuilds `AstroCatalog` / `AstroHeader` (dictionaries from config at runtime).

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
