# 10 - End-to-End Monitoring Demo

## Goal

Create a small end-to-end demo that verifies the full monitoring flow.

This is a development integration test.

It is not the final production deployment.

## Full Flow

```text
MATLAB soc.monitor package -> JSONL files -> Python forwarder -> monitoring backend -> fake or real SOC event adapter
```

## Required Demo Steps

### Step 1 - Start Backend

Run local monitoring backend.

Example:

```bash
uvicorn monitoring_api:app --host 127.0.0.1 --port 8150
```

First version may use fake SOC Events adapter.

### Step 2 - Run MATLAB Debug

From MATLAB, run:

```matlab
soc.monitor.debug_monitor.debug()
```

Or if implemented as a simple function file:

```matlab
soc.monitor.debug_monitor()
```

This shall create JSONL records.

### Step 3 - Run Python Forwarder

Run:

```bash
python run_forwarder.py --config config/forwarder_config_debug.json
```

The forwarder shall read the JSONL records and post them to the backend.

### Step 4 - Verify Backend Received Records

Backend shall print or expose received count.

Optional endpoint:

```text
GET /monitor/debug/records
```

### Step 5 - Run External Monitor Once

Run:

```bash
python run_external_monitor.py --config config/external_monitor_debug.json --once
```

The external monitor shall send check records to the backend.

## Required Files

```text
config/matlab_monitor_debug_config.json
config/forwarder_config_debug.json
config/backend_config_debug.json
config/external_monitor_debug_config.json
```

## Expected Output

The demo shall show:

- MATLAB writes JSONL records.
- Python forwarder reads JSONL records.
- Python forwarder updates offset state.
- Backend receives records.
- Backend maps selected records to fake SOC events.
- External monitor creates health records.

## Debug Printouts

Every component shall print clear messages.

Required print style:

```text
[DEBUG] Starting ...
[DEBUG] Config: ...
[DEBUG] Created file: ...
[DEBUG] Records written: ...
[DEBUG] Records forwarded: ...
[DEBUG] Backend received: ...
[DEBUG] Done.
```

## Success Criteria

The demo is successful if:

- JSONL file exists.
- JSONL file has valid JSON lines.
- Forwarder sends records to backend.
- Forwarder state file is updated.
- Backend receives records.
- No component requires manual database setup for the debug demo.

## Later Production Steps

After the demo works, connect backend to:

- PostgreSQL monitoring tables.
- Existing SOC Events API.
- SOC Portal dashboard.
- systemd or Docker deployment.
