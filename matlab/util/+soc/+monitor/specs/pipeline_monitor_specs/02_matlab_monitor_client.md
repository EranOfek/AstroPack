# 02 - MATLAB JSONL Monitor Client

## Goal

Create the MATLAB-side monitoring client for the ULTRASAT pipeline.

The client shall append structured monitoring records to local JSONL files.

The client shall not call REST APIs directly in the first version.

## MATLAB Package

Use package:

```text
soc.monitor
```

Folder:

```text
C:\Ultrasat\AstroPack\matlab\util\+soc\+monitor
```

All public calls shall use this prefix:

```matlab
soc.monitor.*
```

## Required Design

Use a package-level singleton wrapper.

Pipeline code should call simple package functions.

Example:

```matlab
soc.monitor.init(configFilename)
soc.monitor.heartbeat()
soc.monitor.image_started(filename, info)
soc.monitor.stage_started(imageId, stageName, info)
soc.monitor.stage_done(imageId, stageName, info)
soc.monitor.image_done(imageId, info)
soc.monitor.fault(code, message, info)
soc.monitor.metric(name, value, unit, info)
```

Internally, all functions shall use one shared `MonitorClient` object.

## Suggested Files

```text
+soc/+monitor/MonitorClient.m
+soc/+monitor/MonitorConfig.m
+soc/+monitor/MonitorConst.m
+soc/+monitor/init.m
+soc/+monitor/get_client.m
+soc/+monitor/reset.m
+soc/+monitor/heartbeat.m
+soc/+monitor/image_started.m
+soc/+monitor/image_done.m
+soc/+monitor/image_failed.m
+soc/+monitor/stage_started.m
+soc/+monitor/stage_done.m
+soc/+monitor/stage_failed.m
+soc/+monitor/product_created.m
+soc/+monitor/clickhouse_insert_started.m
+soc/+monitor/clickhouse_insert_done.m
+soc/+monitor/clickhouse_insert_failed.m
+soc/+monitor/fault.m
+soc/+monitor/metric.m
+soc/+monitor/log_record.m
+soc/+monitor/make_record.m
+soc/+monitor/utc_now_str.m
```

## JSONL File Naming

Use one writer per file.

Recommended filename:

```text
pipeline_monitor_<pipeline_id>_<instance_id>_<date>.jsonl
```

Example:

```text
pipeline_monitor_ultrasat_pipeline_main_12345_20260604.jsonl
```

The MATLAB process shall append only.

Do not rewrite the JSONL file.

Do not share one JSONL file between multiple MATLAB processes.

## File Writing Behavior

For each record:

1. Create MATLAB struct.
2. Encode as JSON.
3. Open file in append mode.
4. Write one JSON line.
5. Close file.

This simple behavior is acceptable for the first version.

It is robust and easy to debug.

## Configuration

Load configuration from JSON file.

Example config:

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

## Singleton Behavior

`init(configFilename)` shall create the singleton client.

`get_client()` shall return the existing client.

If no client exists, `get_client()` may create a default client using default config.

`reset()` shall clear the singleton.

This is useful for tests and debugging.

## Required Public Functions

### heartbeat

Writes a heartbeat record.

### image_started

Writes image processing start record.

### image_done

Writes image processing done record.

### image_failed

Writes image processing failed record.

### stage_started

Writes stage start record.

### stage_done

Writes stage done record.

### stage_failed

Writes stage failed record.

### product_created

Writes product creation record.

### clickhouse_insert_started

Writes ClickHouse insertion start record.

### clickhouse_insert_done

Writes ClickHouse insertion done record.

### clickhouse_insert_failed

Writes ClickHouse insertion failure record.

### fault

Writes a fault record.

### metric

Writes a metric record.

### log_record

Writes a general log/debug record.

## Error Handling

Monitoring must never crash the pipeline.

If writing fails:

- Catch the error.
- Optionally print warning if configured.
- Return without throwing.

## Data Field

The `info` parameter shall be stored in the `data` field.

If `info` is missing or empty, use empty struct.

## Debug Requirement

Create debug file:

```text
+soc/+monitor/debug_monitor_client.m
```

Functions:

```matlab
debug_func1_basic_init()
debug_func2_write_heartbeat()
debug_func3_write_image_lifecycle()
debug_func4_write_fault_and_metric()
debug()
```

`debug()` shall call all debug functions.

Debug functions shall print clear messages.

Debug functions shall create temporary JSONL files in a debug folder.
