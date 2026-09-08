# db.sources — Unique Sources HTTP client

MATLAB HTTP client for the **Unique Sources FastAPI service** (Layer 1). Pipeline code submits detection batches; the service writes the **sources** (detection) table and the **unique_sources** catalog.

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
| `US_BASE_URL` | `http://127.0.0.1:8151` | Unique Sources API base URL |
| `US_API_KEY` | (empty) | Optional API key header |
| `ASTROPACK_DATA_PATH` | OS-specific fallback | Parent for offline outbox (see below) |

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

Python client and FastAPI service live in **astro-clickhouse**: `python/services/unique_sources/`.
