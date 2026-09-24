# 01 - Shared Monitoring Schema and Constants

## Goal

Create the common monitoring contract used by both MATLAB and Python.

This task defines record fields, constants, event codes, severities, and status values.

## Scope

This task does not implement the MATLAB client, Python forwarder, backend service, or external monitor.

It only defines the shared schema and constants.

## Main Design

All monitoring messages are JSON records.

Each JSON record is written as one line in a JSONL file.

The same record structure shall be used by:

- MATLAB pipeline monitoring client.
- Python JSONL forwarder.
- FastAPI monitoring backend.
- Python external monitor.
- Debug tools.

## MATLAB Package Name

Use this MATLAB package:

```text
soc.monitor
```

MATLAB folder:

```text
C:\Ultrasat\AstroPack\matlab\util\+soc\+monitor
```

All MATLAB functions and classes shall be called using the package prefix:

```matlab
soc.monitor.init(...)
soc.monitor.heartbeat(...)
soc.monitor.image_started(...)
soc.monitor.stage_done(...)
soc.monitor.fault(...)
```

Do not use `soc.mon` for this implementation.

`soc.monitor` is clearer for project users and future maintainers.

## Required Record Fields

Every record shall contain these fields:

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

## Field Definitions

### schema_version

Schema version string.

Initial value:

```text
1.0
```

### dt

UTC timestamp in ISO format.

Use `Z` suffix.

Example:

```text
2026-06-04T12:30:01.123Z
```

### source

Origin of the record.

Allowed initial values:

```text
matlab_pipeline
python_forwarder
python_external_monitor
backend_service
```

### pipeline_id

Logical pipeline name.

Example:

```text
ultrasat_pipeline
```

### instance_id

Specific running instance.

Recommended format:

```text
<name>_<pid>
```

Example:

```text
main_12345
worker2_23456
```

### record_kind

Type of monitoring record.

Allowed initial values:

```text
heartbeat
image_lifecycle
stage_lifecycle
product_lifecycle
clickhouse_lifecycle
fault
metric
log
state
external_check
soc_event
```

### severity

Severity level.

Allowed values:

```text
debug
info
notice
warning
error
critical
```

### status

Short status value.

Allowed initial values:

```text
alive
started
done
failed
timeout
ok
warning
error
cleared
created
skipped
```

### message

Human-readable short text.

### data

Free JSON object for extra data.

Must always exist.

Use empty object when no extra data is needed.

```json
"data": {}
```

## Recommended Optional Fields

These fields may be added when relevant:

```text
image_id
filename
stage
product_type
product_filename
event_code
metric_name
metric_value
metric_unit
duration_sec
correlation_id
parent_correlation_id
```

## Event Codes

Use hierarchical event codes.

Initial required codes:

```text
pipeline.heartbeat.timeout
pipeline.process.crashed
pipeline.process.not_running
pipeline.image.started
pipeline.image.done
pipeline.image.failed
pipeline.stage.started
pipeline.stage.done
pipeline.stage.failed
pipeline.stage.timeout
pipeline.product.created
pipeline.product.missing
pipeline.clickhouse.insert.started
pipeline.clickhouse.insert.done
pipeline.clickhouse.insert.failed
pipeline.disk.full
pipeline.disk.warning
pipeline.memory.high
pipeline.cpu.high
pipeline.backlog.high
pipeline.log.stale
pipeline.external_check.failed
```

## Clearable Events

Some events represent abnormal current state and can be cleared.

Examples:

```text
pipeline.heartbeat.timeout
pipeline.process.not_running
pipeline.stage.timeout
pipeline.disk.full
pipeline.memory.high
pipeline.cpu.high
pipeline.backlog.high
pipeline.log.stale
```

Historical events should usually not be cleared.

Examples:

```text
pipeline.image.failed
pipeline.stage.failed
pipeline.clickhouse.insert.failed
```

## MATLAB Constants

Cursor should create a MATLAB constants class or functions under:

```text
+soc/+monitor/MonitorConst.m
```

Example usage:

```matlab
soc.monitor.MonitorConst.SchemaVersion
soc.monitor.MonitorConst.KindHeartbeat
soc.monitor.MonitorConst.SeverityInfo
soc.monitor.MonitorConst.StatusAlive
```

## Python Constants

Cursor should create a Python constants module.

Suggested file:

```text
python/monitoring/monitor_const.py
```

The values must match the MATLAB constants.

## Validation

For the first version, validation may be simple.

Required checks:

- `schema_version` exists.
- `dt` exists.
- `source` exists.
- `pipeline_id` exists.
- `instance_id` exists.
- `record_kind` exists.
- `severity` exists.
- `status` exists.
- `message` exists.
- `data` exists and is a JSON object.

## Debug Requirement

Create a debug file for this task.

MATLAB debug file:

```text
+soc/+monitor/debug_monitor_schema.m
```

Python debug file:

```text
python/debug/debug_monitor_schema.py
```

Debug files shall include:

```text
debug_func1()
debug_func2()
debug()
```

The `debug()` function shall call all debug functions.

All debug functions shall print clear progress messages.
