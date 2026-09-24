# 03 - MATLAB Debug Scripts for Monitoring Package

## Goal

Create MATLAB debug scripts that demonstrate and test the `soc.monitor` package.

These scripts are not unit tests.

They are developer debug entry points.

They shall be simple to run manually from MATLAB.

## Location

Use folder:

```text
C:\Ultrasat\AstroPack\matlab\util\+soc\+monitor\+debug\
```

## Required Debug File

```text
+soc/+monitor/+debug/debug_monitor.m
```

## Required Functions

The file shall contain:

```matlab
function debug_func1_basic_init()
function debug_func2_heartbeat()
function debug_func3_image_lifecycle()
function debug_func4_stage_lifecycle()
function debug_func5_fault_metric_log()
function debug_func6_reset_and_reinit()
function debug()
```

The `debug()` function shall call all other debug functions.

## Printouts

Every debug function shall print:

- Start message.
- Important parameter values.
- JSONL filename.
- Number of records expected.
- Done message.

## Debug Folder

Use a temporary debug output folder.

Recommended default:

```text
C:/SOC/monitor/debug_jsonl
```

Create the folder if it does not exist.

## Debug Config File

The debug script may create a temporary JSON config file.

Example:

```json
{
  "pipeline_id": "debug_pipeline",
  "instance_name": "debug_main",
  "jsonl_folder": "C:/SOC/monitor/debug_jsonl",
  "schema_version": "1.0",
  "write_enabled": true,
  "print_to_console": true
}
```

## Required Debug Scenarios

### Basic Init

Call:

```matlab
soc.monitor.init(configFilename)
```

Print the resulting client configuration.

### Heartbeat

Call:

```matlab
soc.monitor.heartbeat()
```

### Image Lifecycle

Call:

```matlab
soc.monitor.image_started(filename, info)
soc.monitor.image_done(imageId, info)
```

### Stage Lifecycle

Call:

```matlab
soc.monitor.stage_started(imageId, "crop", info)
soc.monitor.stage_done(imageId, "crop", info)
```

### Fault, Metric, Log

Call:

```matlab
soc.monitor.fault("pipeline.test_fault", "Test fault message", info)
soc.monitor.metric("detections_count", 1234, "count", info)
soc.monitor.log_record("debug", "Test log message", info)
```

### Reset and Reinit

Call:

```matlab
soc.monitor.reset()
soc.monitor.init(configFilename)
```

## Validation

At the end of `debug()`, print the JSONL files created.

Optional:

- Read the JSONL file.
- Print first few lines.
- Count number of lines.

## Rule

The debug script shall not require the FastAPI backend.

The debug script shall only test local JSONL writing.