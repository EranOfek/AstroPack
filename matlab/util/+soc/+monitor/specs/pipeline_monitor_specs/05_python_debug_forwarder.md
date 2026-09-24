# 05 - Python Debug Script for JSONL Forwarder

## Goal

Create a Python debug script for manually testing the JSONL forwarder.

This script shall help development before the real backend exists.

## Location

Suggested file:

```text
python/debug/debug_jsonl_forwarder.py
```

## Required Functions

```python
def debug_func1_create_sample_jsonl():
def debug_func2_forward_once_with_fake_backend():
def debug_func3_forward_resume_from_state():
def debug_func4_incomplete_last_line():
def debug_func5_backend_failure_no_offset_advance():
def debug():
```

The `debug()` function shall call all debug functions.

Every debug function shall print clear messages.

## Debug Folder

Use temporary debug folders.

Example:

```text
C:/SOC/monitor/debug_forwarder/jsonl
C:/SOC/monitor/debug_forwarder/state
```

On Linux, allow equivalent paths from config.

## Required Scenarios

### Create Sample JSONL

Create a sample JSONL file with records:

- heartbeat
- image_started
- stage_started
- stage_done
- image_done
- fault
- metric

### Forward Once With Fake Backend

Use a fake backend class.

The fake backend shall collect records in memory.

Print number of records received.

### Resume From State

Run forwarder once.

Append more records.

Run forwarder again.

Verify only new records are forwarded.

### Incomplete Last Line

Create a JSONL file where the last line is incomplete.

Verify the incomplete line is ignored.

Verify offset does not move beyond the last complete line.

### Backend Failure

Use a fake backend that raises an exception.

Verify the forwarder does not update the offset.

Then use a working fake backend.

Verify the same records are sent successfully.

## Printouts

Print:

- Config used.
- Files created.
- Offsets before and after.
- Number of records read.
- Number of records sent.
- Any expected errors.

## Rule

This debug script shall not require FastAPI, PostgreSQL, ClickHouse, or SOC services.
