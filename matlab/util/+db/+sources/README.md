# db.sources — service clients (synced from astro-clickhouse)

**Do not edit `SourcesClient.m`, `SourcesClientLocal.m`, or `prepareTable.m` here.** Source of truth is astro-clickhouse; re-run `matlab/astropack_sync` after pulling that repo.

| Class | Transport |
|-------|-----------|
| `SourcesClient` | Unique Sources HTTP (Layer 1) |
| `SourcesClientLocal` | Object Matcher NVMe filedrop (Route B) |
| `prepareTable` | Shared Parquet coercion for both |

Pipeline code submits detection batches; services write detection tables and catalogs.

## Usage

Requires AstroPack `startup` (adds `matlab/util` to the path):

```matlab
client = db.sources.SourcesClient();
client.Verbose = true;
client.health();

tbl = table(ra, dec, magnitude, flux, flags, timestamp, ...
    'VariableNames', {'ra','dec','magnitude','flux','flags','timestamp'});
resp = client.insertSources(tbl, 'RequestId', 'visit-raw123');

if isfield(resp, 'queued') && resp.queued
    % Service unreachable — request persisted under ASTROPACK_DATA_PATH
else
    job = client.waitJob(resp.job_id);
end
```

## Environment

| Variable | Default | Purpose |
|----------|---------|---------|
| `US_BASE_URL` | `http://127.0.0.1:8151` | Full URL or alias `local` / `euclid` |
| `US_API_KEY` | (empty) | Optional locally; required on Euclid |
| `ASTROPACK_DATA_PATH` | OS-specific fallback | Parent for offline outbox (see below) |

### Named hosts (`US_BASE_URL` or constructor arg)

| Alias | URL |
|-------|-----|
| `local` | `http://127.0.0.1:8151` |
| `euclid` | `http://euclid/unique-sources` |

## Offline outbox

When the service is unreachable (connection refused, timeout, DNS), mutating calls (`insertSources`, `insertParquetFile`, `matchSources`) are **write-ahead persisted** and return `queued=true` with empty `job_id` — no exception.

```
$ASTROPACK_DATA_PATH/sources/outbox/
  pending/<timestamp>_<request_id>/
    meta.json
    payload.parquet    # insert only
  failed/              # HTTP 4xx/5xx (non-transport)
```

- **Auto-flush:** `flushPending()` runs at the start of insert/match and after a successful `health()`.
- **Explicit flush:** `client.flushPending()` replays pending entries FIFO.
- **waitJob** requires a live `job_id`; do not call it on queued responses.

## Debug walkthroughs

See [`+debug/README.md`](+debug/README.md) — smoke insert, continuous realistic batches.

## Service repo

Python client, FastAPI service and the native manager (Object Matcher) live in **astro-clickhouse**: `python/services/object_matcher/`.

## Fast local hand-off: `matlab_table_dump`

For a producer on the same machine as the manager, `matlab/util/+matlab_table_dump/` writes a table as one columnar file into a queue folder
(`/dev/shm/object_matcher/<domain>`, fallback `/fastdata/astro-clickhouse/object_matcher/queue/<domain>`); the manager matches it and inserts
into ClickHouse. Both folders and the batch attributes are described in astro-clickhouse
`python/services/object_matcher/docs/operations/MATLAB_TABLE_DUMP_INPUT.md`.

```matlab
matlab_table_dump.publish('/dev/shm/object_matcher/src2b', T, matlab_table_dump.batch_id('field0412', k), ...
                          'ProducerId', 'matlab1', 'Engine', 'mex', 'Attrs', struct('family', 'visit'));
```

`Engine 'mex'` needs `matlab_table_dump.build_mex` once per machine (source: `matlab/util/mex/matlab_table_dump_mex.c`); the default engine is pure MATLAB.
Do not edit `+matlab_table_dump/` or `mex/` here: they are synced from astro-clickhouse (`matlab/astropack_sync/`).
