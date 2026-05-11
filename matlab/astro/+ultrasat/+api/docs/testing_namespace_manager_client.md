# Testing: NamespaceManagerClient

## Overview

This document describes how to test the MATLAB `NamespaceManagerClient` against the live Python FastAPI server.

---

## Files

| File | Purpose |
|------|---------|
| `+clients/NamespaceManagerClient.m` | MATLAB client — only `getNamespaceList()` |
| `+debug/+clients/debug_NamespaceManagerClient.m` | Debug script to run against the server |
| `python/prj/nova/soc/platform/namespace_manager/api.py` | Server-side FastAPI (reference) |
| `python/prj/nova/debug/platform/namespace_manager/debug_namespace_manager.html` | Browser debug console (reference) |

---

## Environment Requirements

| Variable | Value |
|----------|-------|
| `SOC_PATH` | `S:\` |
| `SOC_API_KEY` | `ULTRASOC-2024-10-17` |

These are already set in the Windows system environment. MATLAB inherits them on startup.

---

## Server Endpoints (for reference)

Base URL (nginx mode, from `S:\config\services.json`):  
`http://socsrv/api/namespace-manager`

| Method | Path | Auth | Description |
|--------|------|------|-------------|
| GET | `/health` | none | Health check |
| POST | `/get-namespaces` | api-key | List namespaces (optional `is_active` filter) |
| POST | `/add-namespace` | api-key | Add namespace (not exposed in MATLAB client) |
| POST | `/edit-namespace` | api-key | Edit namespace (not exposed in MATLAB client) |
| POST | `/delete-namespace` | api-key | Delete namespace (not exposed in MATLAB client) |

The MATLAB client only wraps **`/get-namespaces`**. The other endpoints are accessible from the browser debug console or directly via curl/Python if needed.

---

## How to Run the Debug Script

### Option 1 — MATLAB batch (command line)

```bat
"C:\Matlab\R2025b\bin\win64\MATLAB.exe" -batch "ultrasat.api.debug.clients.debug_NamespaceManagerClient()"
```

MATLAB's `startup.m` (in `C:\Users\chent\Documents\MATLAB\startup.m`) automatically adds AstroPack paths, so no manual `addpath` is needed.

### Option 2 — Interactive MATLAB

Open MATLAB, then in the Command Window:

```matlab
ultrasat.api.debug.clients.debug_NamespaceManagerClient()
```

---

## Expected Output (verified 2026-04-26)

```
2026-04-26 14:15:40 [NamespaceManagerClient] NamespaceManagerClient constructor started
2026-04-26 14:15:40 [NamespaceManagerClient] getNamespaceList: Getting list of namespaces
          status: 'ok'
         message: []
      namespaces: [2×1 struct]
    display_list: {'dev:Development'  'qa:QA'}

Namespaces:
    namespace     display_name    description                is_active    created_time    updated_time
    ---------    ------------    --------------------       ---------    ------------    ------------
    'dev'        'Development'   'Development only ...'     true         '2026-02-15...' '2026-02-15...'
    'qa'         'QA'            'QA only (Sasha ...)'      true         '2026-02-15...' '2026-02-15...'

    {'dev:Development'}    {'qa:QA'}
```

---

## Quick Curl Test (verify server is up before running MATLAB)

```bash
# Health
curl http://socsrv/api/namespace-manager/health

# Get namespaces
curl -X POST http://socsrv/api/namespace-manager/get-namespaces \
  -H "Content-Type: application/json" \
  -H "api-key: ULTRASOC-2024-10-17" \
  -d '{}'
```

---

## Workflow for Testing a New Client

1. Get the HTML debug console path from the user (e.g. `debug_<service>.html`).
2. Get the Python API file path (e.g. `soc/platform/<service>/api.py`).
3. Read both files to understand endpoints and request/response shapes.
4. Check the MATLAB client (`+clients/<Service>Client.m`) covers the needed endpoints.
5. Check/update the debug script (`+debug/+clients/debug_<Service>Client.m`).
6. Curl-test each endpoint to confirm the server is up and check response shape.
7. Run: `"C:\Matlab\R2025b\bin\win64\MATLAB.exe" -batch "ultrasat.api.debug.clients.debug_<Service>Client()"`
8. Compare MATLAB output to curl output — they should match.
9. Document results in this `docs/` folder.
