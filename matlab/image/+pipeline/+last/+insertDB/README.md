# pipeline.last.insertDB — LAST archive ingest to ClickHouse

Post-processing scripts that walk LAST archive `proc/` visit directories and inject metadata or catalog rows into the ClickHouse `last` database. These are **not** intended for real-time pipeline use (except `insertTransients2DB`).

**Domain:** `matlab/image/+pipeline/+last/+insertDB` (LAST-specific pipeline).

## Script inventory

| Script | Input | ClickHouse table | FITS sources? |
|--------|-------|------------------|---------------|
| `insertArchiveCoaddCatalogs2DB` | `LAST*coadd_Cat_1.fits` | `visit_src` | **Yes** — catalog table FITS |
| `insertArchiveCatalogs2DB` | `LAST*proc_Cat_1.fits*` | `proc_src` | **Yes** — catalog table FITS |
| `insertArchiveCoaddCatalogs2DB_client` | same as coadd | via Unique Sources API → `visit_src` | **Yes** — SourcesClient twin |
| `insertArchiveCatalogs2DB_client` | same as proc | via Unique Sources API → `proc_src` | **Yes** — SourcesClient twin |
| `insertArchiveImages2DB` | `LAST*coadd_Image_1.fits` | `visit_images` | No — image headers only |
| `insertArchiveRawImages2DB` | `LAST*sci_raw_Image_1.fits.fz` | `raw_images` | No — raw image headers |
| `insertArchiveAsteroids2DB` | `LAST*coadd_Aster*.mat` | `visit_asteroids` | No — MAT tables + Cat FITS headers |
| `insertTransients2DB` | in-memory catalog | `diff_src` | No — pipeline transients |
| `insertTranDBTransients2DB` | `TranDB.mat` | `diff_src` | No — one-shot legacy load |

Only the four catalog rows above insert **source detections**. Image and asteroid scripts use different helpers and XLS sheets.

See [MIGRATION.md](MIGRATION.md) for switching coadd/proc ingest from direct ClickHouse to `SourcesClient`.

## FITS catalog → ClickHouse sources (direct path)

Used by `insertArchiveCoaddCatalogs2DB` and `insertArchiveCatalogs2DB`.

```
proc visit dirs (v0, not re)
  → read .status (skip if already injected)
  → AstroCatalog + AstroHeader(HDU 3) from Cat FITS
  → fill missing NODENUMB, MOUNTNUM, SUBDIR, JD, INGESTION_TIME_JD
  → column map from Design-Database-Pipeline-ClickHouse.xlsx (Sheet: Sources)
  → imProc.db.insertCatalog
  → ClickHouse last.visit_src or last.proc_src
  → append line to .status
```

### Directory discovery

- `RootDir` + `ProcDirTemplate` (default `/proc/*`) via `dir(fullfile(...))`.
- Keep dirs whose name contains `v0`; drop dirs whose folder path contains `re`.
- Proc script also accepts `ProcDirList` (cell of explicit paths).

### Skip / failure handling

Each visit dir must have a `.status` file. Directories without it are logged to `*_no_status_dir.txt` in the **current working directory** when the script starts.

| Script | Skip phrases in `.status` | Failure logs |
|--------|---------------------------|--------------|
| Coadd | `injected into the visit catalog DB`, `not injectable into the visit catalog DB due to broken data files` | `cat_visit_no_status_dir.txt`, `cat_visit_no_data_dir.txt`, `cat_visit_broken_data_dir.txt` |
| Proc | `injected into the proc catalog DB`, `not injectable into the proc catalog DB due to broken data files` | `cat_no_status_dir.txt`, `cat_no_data_dir.txt`, `cat_broken_data_dir.txt` |

Broken FITS or catalogs with fewer than 2 `AstroCatalog` elements are skipped (logged, not fatal).

### Column mapping

Both catalog scripts call:

```matlab
Columns = db.util.read_xls2tableFormat(Template, 'Sheet', 'Sources', 'TableName', DbTable);
```

The XLS file defaults to `~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx` (not in this repo). It maps FITS/header column names to ClickHouse column names.

### imProc.db.insertCatalog

Shared helper at `matlab/image/+imProc/+db/insertCatalog.m`. For each crop/catalog:

1. Select and rename columns per `ColNameDic`.
2. Add image ID (`KeyID`) and per-row source ID (`ColSrcID`) when passed.
3. Drop rows with NaN JD, RA, or Dec.
4. Add BJD, barycentric velocity, healpix indices (`UPIX_PARTITION`, `UPIX_LOW`, `UPIX_HIGH`).
5. Insert to ClickHouse (native `db.mex.ClickHouseClient.insert` in batches of 200000) or legacy CSV load.

**Coadd vs proc ID quirk (match in `_client` twins):**

- Coadd passes `KeyID='id_visit_im'`, `ColSrcID='id_visit_src'`.
- Proc defines `ColNameID='id_proc_src'` but does **not** pass `KeyID`/`ColSrcID` into `insertCatalog`, so the helper defaults apply (`ID_PROC_IM` / `ID_PROC_SRC`).

### Database connection (direct scripts)

Default: **native** connector — `db.mex.ClickHouseClient('euclid', 9000, 'default', pwd)` with ZSTD compression, database `last`. Password from `~/.astropack/Passwords.yml` via `PasswordsManager`.

Legacy connector: `db.Db` + CSV file copied back into the visit dir.

### Examples

```matlab
% Coadd catalogs → visit_src
pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB('/mnt/marvin/', ...
    'LAST*coadd_Cat_1.fits', 'ProcDirTemplate', 'LAST.01.*/2023/*/*/proc/*');

% Proc catalogs → proc_src
pipeline.last.insertDB.insertArchiveCatalogs2DB('/mnt/marvin/LAST.01.01.01/2023/04/24/', ...
    'ProcDirTemplate', '/proc/*');
```

## Non-source scripts (brief)

### insertArchiveImages2DB

Reads coadd (or reference) **image** FITS headers via `imProc.db.insertImages`. XLS sheet `Images`, default table `visit_images`. Same directory walk and `.status` pattern as coadd catalogs.

### insertArchiveRawImages2DB

Raw `.fits.fz` headers → `raw_images`. Uses legacy `db.Db` only. Walks `raw` subdirs under `RootDir`.

### insertArchiveAsteroids2DB

Loads `LAST*coadd_Aster*.mat`, splits by `CropID`, reads matching coadd Cat FITS headers for metadata, inserts via `insertCatalog` → `visit_asteroids`.

### insertTransients2DB

Real-time pipeline helper: accepts in-memory transient catalog + headers, splits by crop, inserts → `diff_src`. Can reuse an open `db.Db` handle.

### insertTranDBTransients2DB

One-shot legacy loader from `TranDB.mat` → `diff_src`.

## SourcesClient twins

| Original | Twin | API target |
|----------|------|------------|
| `insertArchiveCoaddCatalogs2DB` | `insertArchiveCoaddCatalogs2DB_client` | `DetectionTable='visit_src'` |
| `insertArchiveCatalogs2DB` | `insertArchiveCatalogs2DB_client` | `DetectionTable='proc_src'` |

Twins reuse the same FITS read and table build (`insertCatalog` with empty `DbTable`) but POST the full table through [`db.sources.SourcesClient`](../../../util/+db/+sources/SourcesClient.m) instead of opening ClickHouse from MATLAB.

Environment: `US_BASE_URL`, `US_API_KEY`, `ASTROPACK_DATA_PATH` (outbox). See [MIGRATION.md](MIGRATION.md).

## Dependencies

| Component | Role |
|-----------|------|
| `AstroCatalog`, `AstroHeader` | Read FITS catalog tables and headers |
| `imProc.db.insertCatalog` | Build and (direct path) insert source rows |
| `imProc.db.insertImages` | Image header insert |
| `db.util.read_xls2tableFormat` | XLS → column dictionary |
| `db.mex.ClickHouseClient` / `db.Db` | Direct ClickHouse (original scripts) |
| `db.sources.SourcesClient` | HTTP insert (`*_client` scripts) |
| `PasswordsManager` | ClickHouse credentials (direct scripts only) |

## Related documentation

- [MIGRATION.md](MIGRATION.md) — direct ClickHouse vs SourcesClient
- [db.sources README](../../../util/+db/+sources/README.md) — Unique Sources HTTP client
