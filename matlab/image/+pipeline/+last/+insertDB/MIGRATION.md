# Migration: direct ClickHouse → SourcesClient catalog ingest

Guide for moving LAST archive **catalog** ingest from direct ClickHouse inserts to the Unique Sources HTTP service. Original scripts are unchanged; use the parallel `*_client.m` twins.

## Why twins instead of editing originals?

- Production archive backfill still uses direct ClickHouse (`insertArchiveCoaddCatalogs2DB`, `insertArchiveCatalogs2DB`).
- New path is **opt-in** until the Unique Sources service is validated on full LAST schema.
- No dual-write: each run uses either the direct script or the `_client` script, not both.

## Side-by-side

| Aspect | Direct (original) | SourcesClient (`*_client`) |
|--------|-------------------|----------------------------|
| Entry point | `insertArchiveCoaddCatalogs2DB` / `insertArchiveCatalogs2DB` | `insertArchiveCoaddCatalogs2DB_client` / `insertArchiveCatalogs2DB_client` |
| Transport | `db.mex.ClickHouseClient.insert` | HTTP POST Parquet → `/api/jobs/insert/upload` |
| Auth | `Passwords.yml` → ClickHouse | `US_BASE_URL`, `US_API_KEY` |
| Target table | `last.visit_src` / `last.proc_src` | Same names via `DetectionTable` + `Domain='last'` |
| Table build | `imProc.db.insertCatalog` (writes DB) | `imProc.db.insertCatalog` with `DbTable=[]` (build only) |
| Idempotency | `.status` file on disk | `.status` **and** stable `RequestId` per visit dir |
| Offline | N/A (ClickHouse must be up) | Outbox under `$ASTROPACK_DATA_PATH/sources/outbox/pending/` |
| Success stamp | `injected into the visit/proc catalog DB` | `injected into the visit/proc catalog DB via SourcesClient` |

## What is migrated

- Coadd FITS catalogs → `visit_src`
- Proc FITS catalogs → `proc_src`

## What is NOT migrated (this pass)

- Image headers (`insertArchiveImages2DB`, `insertArchiveRawImages2DB`)
- Asteroids (`insertArchiveAsteroids2DB`)
- Transients (`insertTransients2DB`, `insertTranDBTransients2DB`)

## Prerequisites

1. AstroPack `startup` (paths include `matlab/util` and `matlab/image`).
2. Unique Sources API + manager running (see astro-clickhouse `python/services/unique_sources/`).
3. Environment variables:

| Variable | Purpose |
|----------|---------|
| `US_BASE_URL` | e.g. `euclid` or `http://127.0.0.1:8151` |
| `US_API_KEY` | Required on Euclid; optional locally |
| `ASTROPACK_DATA_PATH` | Outbox root when API is down |

4. XLS column template unchanged: `~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx`.

## How the full table is built (no ClickHouse from MATLAB)

The `_client` scripts call the **existing** helper without modifying it:

```matlab
% Coadd — same KeyID/ColSrcID as the direct script
[T, Error] = imProc.db.insertCatalog(Cat, 'Header', AH, 'ColNameDic', Columns, ...
    'DbTable', [], 'CreateCsv', false, ...
    'ColSrcID', Args.ColNameID, 'KeyID', Args.KeyID);

% Proc — do NOT pass KeyID/ColSrcID (matches direct proc script)
[T, Error] = imProc.db.insertCatalog(Cat, 'Header', AH, 'ColNameDic', Columns, ...
    'DbTable', [], 'CreateCsv', false);
```

`T` includes the same columns as a direct insert: photometry, IDs, BJD, healpix, etc. `SourcesClient` lowercases column names and coerces types before Parquet write (ClickHouse-style names).

## RequestId and idempotency

Each visit directory gets a **stable** request id derived from subdir and detection table:

```
last-visit_src-<SUBDIR>     % coadd
last-proc_src-<SUBDIR>      % proc
```

- Re-running with the same id after a successful service insert is treated as duplicate (safe).
- If the API is down, the request is queued locally; **do not** stamp `.status` until `waitJob` returns `done`.
- Replay queued inserts: `client = db.sources.SourcesClient(); client.health();`

## .status handling

Skip injection if `.status` already contains **any** of:

- Original phrase: `injected into the visit catalog DB` / `injected into the proc catalog DB`
- Client phrase: `injected into the visit catalog DB via SourcesClient` / `injected into the proc catalog DB via SourcesClient`
- Broken-data phrase (unchanged from originals)

On successful job completion only:

```matlab
% Appended via su RemoteUser, same as direct scripts
'<timestamp> injected into the visit catalog DB via SourcesClient'
```

## Usage examples

```matlab
% Coadd — same RootDir / ProcDirTemplate as direct script
pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB_client('/mnt/marvin/', ...
    'LAST*coadd_Cat_1.fits', 'ProcDirTemplate', 'LAST.01.*/2023/*/*/proc/*');

% Proc
pipeline.last.insertDB.insertArchiveCatalogs2DB_client('/mnt/marvin/LAST.01.01.01/2023/04/24/', ...
    'ProcDirTemplate', '/proc/*');

% Explicit API endpoint (overrides US_BASE_URL)
pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB_client('/mnt/marvin/', ...
    'LAST*coadd_Cat_1.fits', 'BaseUrl', 'euclid', 'WaitTimeout', 1800);
```

## Queued vs live responses

```matlab
resp = client.insertSources(T, 'RequestId', reqId, 'Domain', 'last', ...
    'DetectionTable', 'visit_src');

if isfield(resp, 'queued') && resp.queued
    % Saved under ASTROPACK_DATA_PATH/sources/outbox/pending/
    % No .status stamp — rerun health/flushPending later
else
    job = client.waitJob(resp.job_id, 'Timeout', WaitTimeout);
    % Stamp .status on job.status == 'done'
end
```

## Service-side requirement (open risk)

Debug/smoke inserts use a **reduced** schema (`ra`, `dec`, `magnitude`, `flux`, `flags`, `timestamp`). Archive ingest sends the **full** LAST table from `insertCatalog` (dozens of columns, uint64 IDs, healpix, BJD, …).

The Unique Sources service must accept that full Parquet schema for `DetectionTable=visit_src|proc_src`. Validate on a single visit dir before large backfills. If the service rejects unknown columns, coordinate a schema update in astro-clickhouse — do not strip columns in MATLAB without an explicit mapping spec.

## Rollback

Use the original scripts unchanged. `.status` lines from the client path are distinct; direct scripts skip dirs already marked with the original stamp. To re-ingest via direct ClickHouse after a client run, remove or edit the client stamp line in `.status` (operational decision — avoid duplicate rows in ClickHouse).

## Checklist for operators

- [ ] `client.health()` succeeds
- [ ] Single visit dir trial with `_client` script
- [ ] Confirm rows in `last.visit_src` or `last.proc_src` (via service or ClickHouse query)
- [ ] Confirm `.status` client stamp written only after `waitJob` done
- [ ] Test offline queue: stop API, run one dir, restart API, `client.health()` replays pending
- [ ] Scale to full `RootDir` backfill
