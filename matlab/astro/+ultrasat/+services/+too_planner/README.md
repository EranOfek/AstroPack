# ULTRASAT ToO Planner MATLAB Service

## Overview

The `too_planner` MATLAB service is part of the ULTRASAT SOC architecture.

The service allows Python/FastAPI systems to execute the ULTRASAT ToO
(Target of Opportunity) planner inside MATLAB using a file-based IPC
(JSON exchange) mechanism.

The architecture intentionally separates:

- Python / FastAPI API layer
- MATLAB scientific execution layer
- IPC transport layer
- Artifact generation layer

This provides:
- process isolation
- reproducibility
- durable artifacts
- easier debugging
- easier deployment
- long-term maintainability

---

# High-Level Architecture

```text
Python Client
    |
    v
FastAPI Bridge
    |
    v
MatlabBridge (JSON file IPC)
    |
    v
runtime/exchange/too_planner/input/*.json
    |
    v
MATLAB too_planner_service()
    |
    v
processRequest()
    |
    v
TooPlannerRunner.runFromJson()
    |
    +--> summary.json
    +--> plan_01.json
    +--> plan_01.mat
    +--> images (*.png, *.fig)
```

---

# Main Components

## MATLAB Service

### File

```text
+ultrasat/+services/+too_planner/too_planner_service.m
```

### Purpose

Main long-running MATLAB service process.

Responsibilities:

- monitor IPC input folder
- process incoming JSON requests
- call `processRequest()`
- maintain watchdog file
- archive processed requests

### Runtime folders

The service uses:

```text
SOC_PATH/runtime/exchange/too_planner/
```

Subfolders:

```text
input/
processed/
```

Watchdog:

```text
too_planner.watchdog
```

---

## JsonFileIpc

### File

```text
ultrasat.services.common.JsonFileIpc
```

### Purpose

Generic JSON file IPC framework.

Responsibilities:

- monitor input folder
- load JSON request
- call MATLAB callback
- write `.out.json`
- archive processed files
- watchdog updates
- cleanup old processed files

### Request flow

Input:

```text
request_001.json
```

Output:

```text
request_001.out.json
```

Processed original request moved into:

```text
processed/yyyy/MM/dd/
```

---

# MATLAB Request Dispatcher

## File

```text
+ultrasat/+services/+too_planner/processRequest.m
```

## Purpose

Dispatches incoming actions.

Current actions:

- `health`
- `too_planner`

---

# TOO Planner Runner

## File

```text
ultrasat.planner.TooPlannerRunner
```

## Purpose

Core orchestration layer for running one or more ULTRASAT TOO planner runs
from a single JSON configuration.

This class is intentionally isolated from:
- FastAPI
- HTTP
- IPC details

It focuses only on:
- planner execution
- artifact generation
- summary generation

---

# TooPlannerRunner Responsibilities

## 1. Load JSON Configuration

Input JSON contains:

- planner_name
- csv_filename
- output_folder
- plans[]

---

## 2. Load Probability Map

The CSV probability map is loaded once:

```matlab
probMapTable = readtable(csvFile);
```

and reused for all plan executions.

---

## 3. Run Multiple Plans Independently

Each plan is executed independently:

```matlab
runOnePlanSafe(...)
```

Failures in one plan do NOT stop the entire request.

This is intentional and important for SOC robustness.

---

## 4. Generate Artifacts

For each successful plan:

Generated files:

```text
<run_id>.json
<run_id>.mat
<run_id>_sky.png
<run_id>_coverage.png
<run_id>_sky.fig
<run_id>_coverage.fig
```

---

## 5. Generate summary.json

A final aggregation file:

```text
summary.json
```

contains:
- request metadata
- plan results
- generated files
- image references
- statistics

---

# IPC Request Format

## Example Input JSON

```json
{
  "action": "too_planner",
  "planner_name": "AK",
  "csv_filename": "S:/too/lvc.csv",
  "output_folder": "S:/too/output",
  "plans": [
    {
      "label": "fast_4",
      "TOOMaxTargets": 4,
      "TOOMinCoveredProb": 0.3,
      "TOOWindowDurationHours": 3,
      "Verbosity": 0,
      "DrawMaps": 1
    }
  ]
}
```

---

# IPC Output Format

## request.out.json

The IPC output contains a compact API response.

Example:

```json
{
  "status": "ok",
  "message": "TooPlannerRunner: OK",
  "summary_file": "S:/too/output/summary.json",
  "total_plans_attempted": 1,
  "total_plans_succeeded": 1,
  "total_plans_failed": 0,
  "plans": [
    {
      "run_id": "too_01_fast_4_20260517_120000_123",
      "json_file": "S:/too/output/too_01_fast_4_20260517_120000_123.json",
      "mat_file": "S:/too/output/too_01_fast_4_20260517_120000_123.mat",
      "plan_index": 1,
      "status": "success",
      "exposures_scheduled": 10,
      "images": {
        "sky_png": "S:/too/output/too_01_fast_4_20260517_120000_123_sky.png",
        "coverage_png": "S:/too/output/too_01_fast_4_20260517_120000_123_coverage.png"
      }
    }
  ]
}
```

---

# summary.json Structure

## Purpose

`summary.json` is a durable artifact intended for:

- debugging
- replayability
- audit trail
- workflow orchestration
- external systems
- scientific traceability

This is separate from the transient HTTP response.

---

# FastAPI Bridge

## File

```text
soc/matlab_bridges/too_planner_bridge/api.py
```

## Purpose

Acts as the boundary between:
- HTTP/API world
- MATLAB scientific runtime

Responsibilities:

- validate requests
- authenticate API calls
- send requests via JSON IPC
- normalize MATLAB outputs
- return typed API responses

---

# FastAPI Endpoints

## Health Endpoint

```http
GET /health
```

Response:

```json
{
  "status": "ok",
  "message": "health: OK"
}
```

---

## TOO Planner Endpoint

```http
POST /too_planner
```

Request body:

```json
{
  "action": "too_planner",
  "planner_name": "AK",
  "csv_filename": "...",
  "output_folder": "...",
  "plans": [...]
}
```

Response:

```json
{
  "status": "ok",
  "message": "...",
  "summary_file": "...",
  "plans": [...]
}
```

---

# Python Models

## File

```text
soc/common/models/matlab_bridges/api/too_planner.py
```

## Purpose

Defines typed Pydantic models for:

- requests
- responses
- plan configs
- image metadata

These models are the formal API contract between:
- clients
- FastAPI bridge
- MATLAB runtime

---

# Python Client

## File

```text
soc/common/clients/matlab_bridges/too_planner.py
```

## Purpose

Convenience Python client for interacting with the MATLAB bridge API.

Supports:

- typed request generation
- typed response validation
- timeout handling
- API key headers

---

# Important Design Decisions

## File-Based IPC Instead of MATLAB Engine

The system intentionally uses JSON file IPC instead of direct MATLAB Engine.

Advantages:

- MATLAB process isolation
- easier debugging
- durable requests
- replayable requests
- easier deployment
- crash isolation
- easier operations
- works across machines
- easier future async workflows

---

## Typed Pydantic Models

Python uses typed Pydantic models instead of raw dictionaries.

Advantages:

- explicit contracts
- safer refactoring
- easier LLM-assisted development
- API documentation
- validation
- long-term maintainability

---

## Bridge Normalization Layer

The FastAPI bridge intentionally maps MATLAB outputs field-by-field.

This prevents:
- MATLAB internal structures leaking upward
- unstable scientific runtime structures becoming API contracts

The bridge acts as:
- adapter
- compatibility layer
- validator
- normalizer

---

# Debug Infrastructure

## File

```text
ultrasat.planner.debug.debug_TooPlannerRunner
```

## Includes

- success scenario
- failure scenario
- invalid config scenario
- fixture generation
- assertions
- summary validation

This debug infrastructure is important for long-term maintainability.

---

# Environment Variables

## Required

### SOC_PATH

Used for:
- runtime folders
- IPC folders
- watchdogs
- logs

### ASTROPACK_DATA_PATH

Used by:
- planner grids
- ULTRASAT planner internals

---

# Runtime Flow Summary

```text
1. Python client sends HTTP POST
2. FastAPI validates request
3. MatlabBridge writes JSON request file
4. MATLAB service detects request
5. processRequest() dispatches action
6. TooPlannerRunner executes plans
7. Artifacts are generated
8. summary.json created
9. MATLAB writes .out.json
10. FastAPI returns typed response
```

---

# Notes

- The MATLAB service is designed to be long-running.
- Multiple plans may be executed from a single request.
- One failed plan does not stop other plans.
- The system is designed for reproducibility and auditability.
- `summary.json` is considered a first-class artifact.
- The API response is intentionally normalized and typed.
