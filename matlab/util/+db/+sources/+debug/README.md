# db.sources.debug — MATLAB HTTP client walkthroughs

Interactive scripts for **health checks** and **continuous realistic insert** via the Unique Sources API. Requires the **API** and **manager** (same as Python client walkthroughs in astro-clickhouse).

Library: [`../SourcesClient.m`](../SourcesClient.m) (`db.sources.SourcesClient`).

## Prerequisites

| Variable | Default |
|----------|---------|
| `US_BASE_URL` | `http://127.0.0.1:8151` |
| `US_API_KEY` | (empty) |
| `ASTROPACK_DATA_PATH` | OS fallback via `tools.os.getAstroPackDataPath()` |
| `MATLAB_ROOT` | `c:\Matlab\R2025b` (Windows launcher) |

MATLAB **R2025b** with `parquetwrite` and `matlab.net.http`.

## Quick start (MATLAB IDE)

After AstroPack `startup`:

```matlab
db.sources.debug.debug_sources_client();          % 3-row smoke + waitJob or offline queue
db.sources.debug.debug_insert_continuous();       % 1000 rows / 60s until Ctrl+C
db.sources.debug.debug_insert_continuous('MaxRounds', 3, 'IntervalSec', 10);
```

### Offline smoke (API stopped)

Health failure is **expected** when the Unique Sources API is not running. The script **continues** and `insertSources` queues under `$ASTROPACK_DATA_PATH/sources/outbox/pending/`:

```matlab
% Stop API/manager first, then:
db.sources.debug.debug_sources_client()
% WARN on health, OK on "Insert queued offline", pending count increases

% When API is back:
client = db.sources.SourcesClient();
client.health();   % replays pending via flushPending
```

Strict mode (abort if health fails — for CI when API must be up):

```matlab
db.sources.debug.debug_sources_client('RequireHealth', true);
db.sources.debug.debug_insert_continuous('RequireHealth', true, 'MaxRounds', 1);
```

## Launcher scripts (no IDE)

From this folder:

**Windows:**

```bat
cd matlab\util\+db\+sources\+debug
run_debug_insert_continuous.bat
```

Optional env before running:

```bat
set ASTROPACK_PATH=C:\Ultrasat\AstroPack
set MATLAB_ROOT=c:\Matlab\R2025b
set US_INTERVAL_SEC=60
set US_MAX_ROUNDS=0
set US_ROWS=1000
```

**Linux / Git Bash:**

```bash
cd matlab/util/+db/+sources/+debug
chmod +x run_debug_insert_continuous.sh
./run_debug_insert_continuous.sh
```

Smoke one round:

```bat
set US_MAX_ROUNDS=1
set US_INTERVAL_SEC=1
run_debug_insert_continuous.bat
```

## Runtime data

Debug scripts write under `$ASTROPACK_DATA_PATH/sources/debug/`:

| Path | Purpose |
|------|---------|
| `continuous/catalog.json` | Sky-field catalog state |
| `tmp/` | Temporary Parquet batches |
| `logs/` | Timestamped debug logs |

Offline insert queue (production): `$ASTROPACK_DATA_PATH/sources/outbox/` — see [`../README.md`](../README.md).

## Identity in logs and archive

| Source | `request_id` prefix | Parquet name |
|--------|---------------------|--------------|
| Python | `debug-continuous-NNNN-` | `continuous_NNNN.parquet` |
| MATLAB | `debug-matlab-continuous-NNNN-` | `matlab_continuous_NNNN.parquet` |

MATLAB uses a **separate** sky catalog with **+12° RA** offset vs Python so first visits are mostly NEW alongside a running Python inserter.

## Files

| File | Purpose |
|------|---------|
| `debug_sources_client.m` | 3-row smoke: health (best-effort) → insert → waitJob or offline queue |
| `debug_insert_continuous.m` | Continuous 1000-row realistic cadence loop |
| `debug_realistic_batch.m` | Sky fields, catalog persistence, visit simulation |
| `debug_log.m` | Timestamped section/ok/warn/err/info logging |
| `debugOutboxStatus_.m` | Print pending/failed outbox folder counts and paths |
| `run_debug_insert_continuous.bat` | Windows R2025b `-batch` launcher |
| `run_debug_insert_continuous.sh` | Linux/Git-Bash launcher |

## Internal helpers (private)

Shared by smoke and continuous scripts. Trailing `_` marks package-private helpers.

| File | Purpose |
|------|---------|
| `debugConstants_.m` | Named defaults (timeouts, poll interval, simulation fractions) |
| `debugSetupLog_.m` | Create daily log file under `sources/debug/logs/` |
| `debugResolveBaseUrl_.m` | Read `US_BASE_URL` or localhost default |
| `debugCreateClient_.m` | Build verbose `SourcesClient` |
| `debugCheckHealth_.m` | Best-effort health probe; strict `RequireHealth` mode |
| `debugHandleInsertResponse_.m` | Queued / waitJob / result logging (smoke vs continuous) |
| `debugDataRoot_.m` | `$ASTROPACK_DATA_PATH/sources/debug` |
| `debugOutboxStatus_.m` | Outbox pending/failed folder summary |
| `ensurePath_.m` | Add `matlab/util` when `ASTROPACK_PATH` is set (batch launchers) |
