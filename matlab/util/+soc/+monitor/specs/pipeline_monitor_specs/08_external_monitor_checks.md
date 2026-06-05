# 08 - External Monitor Checks

## Goal

Implement the first important external monitor checks.

These checks detect pipeline problems from outside MATLAB.

## Required Checks

Implement only the important checks first.

Do not over-engineer.

## Check 1 - MATLAB Process Check

### Purpose

Detect if the MATLAB pipeline process is not running.

### Config

```json
{
  "enabled": true,
  "process_name_contains": "MATLAB",
  "expected_min_count": 1
}
```

### Abnormal Event

```text
pipeline.process.not_running
```

### Clear Condition

At least one matching process is running.

## Check 2 - Heartbeat Freshness Check

### Purpose

Detect if the pipeline stopped reporting heartbeat.

### Input

Read newest heartbeat record from JSONL files.

### Config

```json
{
  "enabled": true,
  "jsonl_folder": "C:/SOC/monitor/jsonl",
  "file_pattern": "pipeline_monitor_*.jsonl",
  "timeout_sec": 60
}
```

### Abnormal Event

```text
pipeline.heartbeat.timeout
```

### Clear Condition

A recent heartbeat exists.

## Check 3 - Stage Timeout Check

### Purpose

Detect if an image stage started but did not finish in time.

### Input

Read recent JSONL records.

Track:

```text
stage_started
stage_done
stage_failed
```

### Config

```json
{
  "enabled": true,
  "default_timeout_sec": 1800,
  "stage_timeouts_sec": {
    "crop": 300,
    "calibration": 900,
    "source_detection": 1800,
    "clickhouse_insert": 1200
  }
}
```

### Abnormal Event

```text
pipeline.stage.timeout
```

### Clear Condition

Stage done or stage failed is reported.

## Check 4 - Disk Usage Check

### Purpose

Detect high disk usage.

### Config

```json
{
  "enabled": true,
  "paths": ["C:/SOC", "D:/data"],
  "warning_percent": 80,
  "critical_percent": 90
}
```

### Abnormal Events

```text
pipeline.disk.warning
pipeline.disk.full
```

### Clear Condition

Disk usage returns below warning threshold.

## Check 5 - Memory Usage Check

### Purpose

Detect high RAM usage.

### Config

```json
{
  "enabled": true,
  "warning_percent": 80,
  "critical_percent": 90
}
```

### Abnormal Event

```text
pipeline.memory.high
```

### Clear Condition

Memory usage returns below warning threshold.

## Check 6 - CPU Usage Check

### Purpose

Detect abnormal CPU load.

This should be warning only in the first version.

Pipeline may legitimately use high CPU.

### Config

```json
{
  "enabled": true,
  "warning_percent": 90,
  "critical_percent": 98,
  "window_sec": 60
}
```

### Abnormal Event

```text
pipeline.cpu.high
```

### Clear Condition

CPU usage returns below warning threshold.

## Check 7 - ClickHouse Reachability Check

### Purpose

Detect if ClickHouse is unreachable.

### Config

```json
{
  "enabled": true,
  "host": "127.0.0.1",
  "port": 8123,
  "database": "last",
  "query": "SELECT 1",
  "timeout_sec": 5
}
```

### Abnormal Event

```text
pipeline.clickhouse.unreachable
```

Note: add this event code to constants if implemented.

### Clear Condition

Query succeeds.

## Check 8 - Log File Freshness Check

### Purpose

Detect if pipeline log files are stale when the pipeline is expected to run.

### Config

```json
{
  "enabled": true,
  "log_files": ["C:/SOC/logs/pipeline.log"],
  "max_age_sec": 300
}
```

### Abnormal Event

```text
pipeline.log.stale
```

### Clear Condition

Log file modified recently.

## Implementation Notes

Use `psutil` for process, CPU, RAM, and disk checks.

Use existing ClickHouse client style used in ULTRASAT if available.

Keep each check small and independent.

Each check shall return a standard check result.

## Debug Requirement

Create debug file:

```text
python/debug/debug_external_monitor_checks.py
```

Functions:

```python
def debug_func1_process_check():
def debug_func2_heartbeat_check_with_sample_jsonl():
def debug_func3_disk_memory_cpu_checks():
def debug_func4_stage_timeout_check_with_sample_jsonl():
def debug_func5_log_file_check():
def debug():
```

`debug()` shall call all debug functions.

Print clear messages and result objects.
