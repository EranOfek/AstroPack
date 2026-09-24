# 04 - Python JSONL Forwarder

## Goal

Create a Python service that reads MATLAB monitoring JSONL files and forwards records to the monitoring backend.

The forwarder shall run on the same server as the MATLAB pipeline.

## Main Flow

```text
MATLAB JSONL files -> Python Forwarder -> FastAPI Monitoring Backend
```

## Scope

This task implements only the forwarder.

It does not implement the backend service.

It does not implement the external monitor.

## Suggested Python Package

Suggested folder:

```text
python/prj/nova/soc/monitoring/forwarder
```

Adjust to the existing ULTRASAT repository structure if needed.

## Suggested Files

```text
forwarder_config.py
jsonl_forwarder.py
jsonl_file_state.py
monitoring_backend_client.py
monitor_const.py
run_forwarder.py
```

## Configuration

Use JSON config file.

Example:

```json
{
  "jsonl_folder": "C:/SOC/monitor/jsonl",
  "file_pattern": "pipeline_monitor_*.jsonl",
  "state_folder": "C:/SOC/monitor/forwarder_state",
  "backend_base_url": "http://127.0.0.1:8150",
  "backend_endpoint": "/monitor/records",
  "poll_interval_sec": 2.0,
  "batch_size": 100,
  "request_timeout_sec": 5.0,
  "archive_enabled": false,
  "archive_folder": "C:/SOC/monitor/archive",
  "print_debug": true
}
```

## State Handling

The forwarder shall remember the last byte offset read for each JSONL file.

Use a state file.

Example:

```text
forwarder_state.json
```

State example:

```json
{
  "files": {
    "C:/SOC/monitor/jsonl/pipeline_monitor_main_12345_20260604.jsonl": {
      "offset": 123456,
      "last_dt": "2026-06-04T12:30:01.123Z"
    }
  }
}
```

## Reading Behavior

For each matching JSONL file:

- Open file for read.
- Seek to last stored offset.
- Read new lines.
- Ignore empty lines.
- Ignore incomplete last line.
- Parse each complete line as JSON.
- Send records in batches.
- Update offset only after successful send.

## Backend Send Behavior

Send records to:

```text
POST /monitor/records
```

Payload:

```json
{
  "records": [ ... ]
}
```

## Failure Handling

If backend is down:

- Do not lose records.
- Do not advance offset.
- Retry in next poll cycle.
- Print warning if configured.

## Duplicate Handling

The first version may rely on offset state.

Backend should later support idempotency.

Optional field for future:

```text
record_id
```

## Log Rotation

First version may ignore log rotation.

Recommended behavior:

- Detect if file size is smaller than stored offset.
- Reset offset to 0 for that file.
- Print warning.

## Archive

Archive is optional for first version.

Do not delete source JSONL files in the first version.

## Service Mode

The forwarder should support:

```bash
python run_forwarder.py --config config/forwarder_config.json
```

It can later run under systemd or Docker.

## Code Style

Use clear classes.

Use docstrings with `:param` style.

Use embedded comments where helpful.

Avoid over-engineering.

## Debug Requirement

Create debug file:

```text
python/debug/debug_jsonl_forwarder.py
```

Functions:

```python
def debug_func1_create_sample_jsonl():
def debug_func2_read_records():
def debug_func3_send_to_fake_backend():
def debug_func4_state_resume():
def debug():
```

`debug()` shall call all debug functions.

Debug functions shall print clear progress messages.

The debug script shall work without the real backend by using a fake backend client.
