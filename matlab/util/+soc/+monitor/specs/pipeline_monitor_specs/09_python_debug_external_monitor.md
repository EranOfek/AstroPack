# 09 - Python Debug Script for External Monitor

## Goal

Create one main debug script for the Python external monitor.

This script shall help manually test the external monitor framework and checks.

## Location

Suggested file:

```text
python/debug/debug_external_monitor.py
```

## Required Functions

```python
def debug_func1_make_debug_config():
def debug_func2_run_process_check():
def debug_func3_run_heartbeat_check():
def debug_func4_run_resource_checks():
def debug_func5_run_stage_timeout_check():
def debug_func6_run_all_checks_once():
def debug():
```

`debug()` shall call all debug functions.

## Debug Data

The script shall create sample JSONL records where needed.

Examples:

- recent heartbeat
- old heartbeat
- stage started without done
- stage done

## Debug Backend

Use a fake backend sender.

The fake backend shall print records instead of sending them to real FastAPI.

## Required Printouts

Print:

- Config path.
- Check names.
- Check results.
- Generated records.
- Fake backend received count.
- Expected abnormal events.
- Expected clear events.

## Required Scenarios

### Process Check

Run the process check and print result.

Do not fail if MATLAB is not running.

Just print the result.

### Heartbeat Check

Create a sample recent heartbeat JSONL file.

Verify OK result.

Create old heartbeat or no heartbeat case.

Verify timeout result.

### Resource Checks

Run disk, memory, and CPU checks.

Print current usage.

### Stage Timeout Check

Create stage started record with old timestamp.

Verify timeout result.

Create matching stage done record.

Verify cleared/OK result.

### All Checks Once

Run all enabled checks once.

Send records to fake backend.

## Rule

This script shall not require PostgreSQL, SOC Events service, or the real monitoring backend.
