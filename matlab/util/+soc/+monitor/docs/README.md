# soc.monitor — Pipeline Monitoring Client

MATLAB package for writing structured pipeline monitoring records to local JSONL files. Records are consumed later by Python forwarders and the SOC monitoring backend.

**Package location:** `matlab/util/+soc/+monitor`  
**MATLAB namespace:** `soc.monitor`

---

## Quick start

### 1. Add AstroPack to the MATLAB path

Ensure `matlab/util` is on the path so `soc.monitor` resolves.

### 2. Create a config file

Example `monitor_config.json`:

```json
{
  "pipeline_id": "ultrasat_pipeline",
  "instance_name": "main",
  "jsonl_folder": "C:/SOC/monitor/jsonl",
  "schema_version": "1.0",
  "write_enabled": true,
  "print_to_console": false
}
```

### 3. Initialize once at pipeline startup

```matlab
soc.monitor.init('C:/SOC/config/monitor_config.json');
```

### 4. Emit records during processing

```matlab
soc.monitor.heartbeat();

Filename = 'IMG_20260604_001234.fits';
soc.monitor.image_started(Filename, struct('telescope', 'ULTRASAT'));

ImageId = 'IMG_20260604_001234';
soc.monitor.stage_started(ImageId, 'astrometry', struct());
soc.monitor.stage_done(ImageId, 'astrometry', struct('n_stars', 412));
soc.monitor.image_done(ImageId, struct());

soc.monitor.metric('detections_count', 1234, 'count', struct());
```

### 5. Run debug scripts (no backend required)

```matlab
soc.monitor.debug.debug_monitor()
soc.monitor.debug.debug_schema()
```

---

## Architecture

See [diagrams/architecture.md](diagrams/architecture.md) and [diagrams/record_flow.md](diagrams/record_flow.md).

Pipeline code calls simple `soc.monitor.*` functions. A singleton `MonitorClient` appends one JSON line per record to a daily JSONL file.

---

## Configuration

| Field | Type | Description |
|-------|------|-------------|
| `pipeline_id` | string | Logical pipeline name (e.g. `ultrasat_pipeline`) |
| `instance_name` | string | Process role name (e.g. `main`, `worker2`) |
| `jsonl_folder` | string | Folder for JSONL output (created if missing) |
| `schema_version` | string | Record schema version (default `1.0`) |
| `write_enabled` | logical | If `false`, records are not written |
| `print_to_console` | logical | If `true`, JSON lines are printed on write errors and successful writes |

**Instance ID** is computed automatically as `<instance_name>_<pid>` using `feature('getpid')`.

---

## JSONL file naming

One file per pipeline instance per UTC day:

```text
pipeline_monitor_<pipeline_id>_<instance_id>_<yyyyMMdd>.jsonl
```

Example:

```text
pipeline_monitor_ultrasat_pipeline_main_12345_20260604.jsonl
```

Rules:

- Append only — never rewrite the file.
- One MATLAB process → one JSONL file (do not share files across processes).

---

## Record format

Each line is one JSON object with required fields:

```json
{
  "schema_version": "1.0",
  "dt": "2026-06-04T12:30:01.123Z",
  "source": "matlab_pipeline",
  "pipeline_id": "ultrasat_pipeline",
  "instance_id": "main_12345",
  "record_kind": "heartbeat",
  "severity": "info",
  "status": "alive",
  "message": "Pipeline heartbeat",
  "data": {}
}
```

Optional fields (when relevant): `image_id`, `filename`, `stage`, `product_type`, `product_filename`, `event_code`, `metric_name`, `metric_value`, `metric_unit`, `duration_sec`, `correlation_id`, `parent_correlation_id`.

Constants are in `soc.monitor.MonitorConst`.

---

## API reference

### Initialization

| Function | Description |
|----------|-------------|
| `soc.monitor.init(configFilename)` | Load config and create singleton client |
| `soc.monitor.get_client()` | Return singleton (creates default if missing) |
| `soc.monitor.reset()` | Clear singleton (tests/debug) |

### Lifecycle

| Function | Description |
|----------|-------------|
| `soc.monitor.heartbeat()` | Pipeline alive signal |
| `soc.monitor.image_started(filename, info)` | Image processing started |
| `soc.monitor.image_done(imageId, info)` | Image processing completed |
| `soc.monitor.image_failed(imageId, info)` | Image processing failed |
| `soc.monitor.stage_started(imageId, stageName, info)` | Stage started |
| `soc.monitor.stage_done(imageId, stageName, info)` | Stage completed |
| `soc.monitor.stage_failed(imageId, stageName, info)` | Stage failed |
| `soc.monitor.product_created(imageId, productType, productFilename, info)` | Product file created |

### ClickHouse

| Function | Description |
|----------|-------------|
| `soc.monitor.clickhouse_insert_started(imageId, info)` | ClickHouse insert started |
| `soc.monitor.clickhouse_insert_done(imageId, info)` | ClickHouse insert done |
| `soc.monitor.clickhouse_insert_failed(imageId, info)` | ClickHouse insert failed |

### Faults, metrics, logs

| Function | Description |
|----------|-------------|
| `soc.monitor.fault(eventCode, message, info)` | Fault with hierarchical event code |
| `soc.monitor.metric(name, value, unit, info)` | Numeric metric |
| `soc.monitor.log_record(severity, message, info)` | General log record |

The `info` argument is always optional; use `struct()` or omit it. It is stored in the record `data` field.

---

## Integration examples

### Main pipeline loop

```matlab
% Startup (once)
soc.monitor.init(fullfile(getenv('SOC_PATH'), 'config', 'monitor_config.json'));

% Periodic heartbeat (e.g. every N images or on timer)
soc.monitor.heartbeat();

% Per image
soc.monitor.image_started(FitsPath, struct('visit_id', VisitId));
try
    soc.monitor.stage_started(ImageId, 'preprocess', struct());
    % ... preprocess ...
    soc.monitor.stage_done(ImageId, 'preprocess', struct());

    soc.monitor.stage_started(ImageId, 'photometry', struct());
    % ... photometry ...
    soc.monitor.stage_done(ImageId, 'photometry', struct('n_sources', NSources));

    soc.monitor.product_created(ImageId, 'catalog', CatPath, struct());
    soc.monitor.clickhouse_insert_started(ImageId, struct('table', 'detections'));
    % ... insert ...
    soc.monitor.clickhouse_insert_done(ImageId, struct('rows', NRows));

    soc.monitor.image_done(ImageId, struct());
catch ME
    soc.monitor.fault('pipeline.stage.failed', ME.message, struct('identifier', ME.identifier));
    soc.monitor.image_failed(ImageId, struct('reason', ME.message));
end
```

### Fault with standard event code

```matlab
soc.monitor.fault( ...
    soc.monitor.MonitorConst.EventStageFailed, ...
    'Astrometry failed', ...
    struct('image_id', ImageId, 'stage', 'astrometry'));
```

---

## Error handling

Monitoring **must not crash the pipeline**. All write paths use `try/catch` inside `MonitorClient.writeRecord`. Failures are swallowed; warnings appear only when `print_to_console` is `true` in config.

---

## Classes (advanced)

| Class | Role |
|-------|------|
| `soc.monitor.MonitorConst` | Schema constants and `validateRecord` |
| `soc.monitor.MonitorConfig` | Config load/save |
| `soc.monitor.MonitorClient` | JSONL writer |
| `soc.monitor.make_record` | Build record struct |
| `soc.monitor.utc_now_str` | UTC ISO timestamp |

---

## Debug and testing

Run scripts individually from MATLAB. Each writes to the debug JSONL folder unless noted.

**Recommended starting point for pipeline integrators:**

| Script | Purpose |
|--------|---------|
| `soc.monitor.debug.debug_pipeline_loop()` | Full per-image loop template (stages, product, ClickHouse, error paths) |

**End-to-end and schema:**

| Script | Purpose |
|--------|---------|
| `soc.monitor.debug.debug_monitor()` | All scenarios in one run (init through reset + JSONL summary) |
| `soc.monitor.debug.debug_schema()` | Constants, UTC timestamp, make_record, validateRecord |

**Individual scenarios (run without full debug_monitor):**

| Script | Purpose |
|--------|---------|
| `soc.monitor.debug.debug_init()` | Init client and print configuration |
| `soc.monitor.debug.debug_heartbeat()` | Heartbeat record |
| `soc.monitor.debug.debug_image_lifecycle()` | image_started, image_done |
| `soc.monitor.debug.debug_stage_lifecycle()` | stage_started, stage_done |
| `soc.monitor.debug.debug_fault_metric_log()` | fault, metric, log_record |
| `soc.monitor.debug.debug_reset()` | reset, re-init, heartbeat |
| `soc.monitor.debug.debug_clickhouse()` | ClickHouse insert started/done/failed |
| `soc.monitor.debug.debug_product()` | product_created |
| `soc.monitor.debug.debug_image_failure()` | stage_failed, fault, image_failed |

**Helpers:**

| Script | Purpose |
|--------|---------|
| `soc.monitor.debug.createDebugConfigFile()` | Write temporary debug config JSON |
| `soc.monitor.debug.printJsonlSummary()` | List debug JSONL files and sample lines |

Debug JSONL output folder (default):

- Windows: `C:/SOC/monitor/debug_jsonl`
- Linux: `/var/opt/soc/monitor/debug_jsonl`

---

## Related specifications

Design specs live under `+soc/+monitor/specs/pipeline_monitor_specs/` (tasks 01–10). This package implements MATLAB tasks 01–03.
