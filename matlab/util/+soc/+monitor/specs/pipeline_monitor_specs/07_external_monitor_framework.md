# 07 - Python External Monitor Framework

## Goal

Create the framework for external monitoring of the MATLAB pipeline.

The external monitor checks the pipeline from outside the MATLAB process.

It detects problems that the pipeline cannot report if it is crashed, stuck, or blocked.

## Main Flow

```text
Python External Monitor -> Monitoring Backend API -> SOC Events / PostgreSQL / Dashboard
```

## Scope

This task creates the framework.

Individual checks can be implemented gradually.

## Suggested Package

```text
python/prj/nova/soc/monitoring/external_monitor
```

## Suggested Files

```text
external_monitor_config.py
external_monitor_runner.py
external_check_base.py
external_check_result.py
checks/process_check.py
checks/heartbeat_check.py
checks/stage_timeout_check.py
checks/disk_check.py
checks/memory_check.py
checks/cpu_check.py
checks/clickhouse_check.py
checks/log_file_check.py
run_external_monitor.py
```

## Configuration

Use JSON config.

Example:

```json
{
  "monitor_id": "pipeline_external_monitor_1",
  "pipeline_id": "ultrasat_pipeline",
  "backend_base_url": "http://127.0.0.1:8150",
  "backend_endpoint": "/monitor/records",
  "poll_interval_sec": 5.0,
  "checks": {
    "process": {
      "enabled": true,
      "process_name_contains": "MATLAB",
      "expected_min_count": 1
    },
    "heartbeat": {
      "enabled": true,
      "jsonl_folder": "C:/SOC/monitor/jsonl",
      "file_pattern": "pipeline_monitor_*.jsonl",
      "timeout_sec": 60
    },
    "disk": {
      "enabled": true,
      "paths": ["C:/SOC", "D:/data"],
      "warning_percent": 80,
      "critical_percent": 90
    },
    "memory": {
      "enabled": true,
      "warning_percent": 80,
      "critical_percent": 90
    },
    "cpu": {
      "enabled": true,
      "warning_percent": 90,
      "critical_percent": 98,
      "window_sec": 60
    }
  },
  "print_debug": true
}
```

## Check Result Model

Each check shall return a result object.

Suggested fields:

```text
check_name
ok
severity
status
message
event_code
data
clear_event_codes
```

## Record Creation

The framework shall convert check results to monitoring records.

Use:

```text
source = python_external_monitor
record_kind = external_check
```

## Required Framework Behavior

The runner shall:

1. Load config.
2. Create enabled checks.
3. Run checks periodically.
4. Convert results to monitoring records.
5. Send records to backend.
6. Catch exceptions per check.
7. Continue running even if one check fails.

## Error Handling

A failed check implementation shall not crash the whole monitor.

If a check raises an exception, create a monitoring record:

```text
record_kind = external_check
status = failed
severity = error
event_code = pipeline.external_check.failed
```

## Backend Failure

If backend is unavailable:

- Print warning.
- Continue next cycle.
- First version does not need durable queue for external monitor.

Optional future improvement:

- External monitor can also write to JSONL and use the same forwarder.

## Debug Requirement

Create debug file:

```text
python/debug/debug_external_monitor_framework.py
```

Functions:

```python
def debug_func1_load_config():
def debug_func2_run_fake_check():
def debug_func3_run_multiple_checks():
def debug_func4_fake_backend_send():
def debug():
```

`debug()` shall call all debug functions.

All debug functions shall print clear messages.
