# 06 - Backend Receiver Service and SOC Event Adapter

## Goal

Create the monitoring backend receiver API.

The backend receives monitoring records from the Python JSONL forwarder.

It stores useful monitoring history and creates or clears SOC command-and-control events when needed.

## Important Decision

The JSONL forwarder shall not call only the SOC Events API directly.

The preferred design is:

```text
JSONL Forwarder -> Monitoring Backend API -> PostgreSQL / SOC Events API / Dashboard State
```

Reason:

Not all monitoring records are SOC events.

Many records are history, lifecycle, metrics, telemetry, heartbeat, and debug traces.

## First Version

The first version may be thin.

It can receive records and call the existing SOC Events client for selected event records.

PostgreSQL storage can be simple or added later.

## Suggested Endpoint

```text
POST /monitor/records
```

Request:

```json
{
  "records": [ ... ]
}
```

Response:

```json
{
  "ok": true,
  "received_count": 10,
  "stored_count": 10,
  "event_actions_count": 2,
  "message": "Records accepted"
}
```

## Suggested Files

```text
monitoring_models.py
monitoring_service.py
monitoring_event_mapper.py
monitoring_datastore.py
monitoring_api.py
soc_events_adapter.py
```

Use existing SOC project structure and naming conventions.

## Required Behavior

For each record:

1. Validate minimal schema.
2. Store record or print/store as first simple version.
3. Update latest state if relevant.
4. Map selected records to SOC events.
5. Add or clear SOC events through the existing SOC Events client.

## SOC Event Mapping

Examples:

```text
pipeline.heartbeat.timeout -> add clearable SOC event
pipeline.process.not_running -> add clearable SOC event
pipeline.stage.timeout -> add clearable SOC event
pipeline.disk.full -> add clearable SOC event
pipeline.memory.high -> add clearable SOC event
pipeline.cpu.high -> add clearable SOC event
pipeline.image.failed -> add historical SOC event
pipeline.clickhouse.insert.failed -> add historical SOC event
```

Clearable events shall be cleared when a corresponding normal record arrives.

Example:

```text
heartbeat alive -> clear pipeline.heartbeat.timeout
process running -> clear pipeline.process.not_running
stage done -> clear pipeline.stage.timeout for that image/stage
```

## PostgreSQL Tables

First version may postpone detailed schema.

Suggested future tables:

```text
monitor.records
monitor.latest_pipeline_state
monitor.latest_image_state
monitor.event_state
```

## Configuration

Use JSON config.

Example:

```json
{
  "service_name": "pipeline_monitor_backend",
  "soc_events_api_base_url": "http://127.0.0.1:8100",
  "store_records_enabled": true,
  "soc_events_enabled": true,
  "print_debug": true
}
```

## Validation

Reject request only if it is structurally invalid.

For individual bad records:

- Count bad records.
- Continue processing valid records.
- Return warning in response.

## Debug Requirement

Create debug file:

```text
python/debug/debug_monitoring_backend.py
```

Functions:

```python
def debug_func1_make_sample_records():
def debug_func2_map_records_to_events():
def debug_func3_fake_soc_events_adapter():
def debug_func4_post_to_local_api():
def debug():
```

`debug()` shall call all debug functions.

The debug script shall include printouts.

The debug script shall support fake SOC Events client for local testing.
