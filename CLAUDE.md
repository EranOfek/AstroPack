# CLAUDE.md 

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running Tests

```matlab
runtests('tests')                        % All tests
runtests('tests/TestStruct2KeyVal')      % Single test file
ultrasat.planner.uplanner.unitTest       % Planner core unit test
```

Tests are function-based (`return functiontests(localfunctions)`), not class-based. Directory structure in `tests/` mirrors `matlab/`.

## Running Debug Scripts

Debug scripts follow the convention `debug_<ClassName>.m` in a `+debug/` sub-package alongside the source they test. Run them directly from MATLAB:

```matlab
ultrasat.api.debug.clients.debug_NamespaceManagerClient()
ultrasat.api.debug.clients.debug_ClientFactory()
ultrasat.planner.guiutils.debug.debug_MainModule()
```

## Environment Variables

Required before running any API client or the planner:

| Variable | Purpose |
|---|---|
| `SOC_PATH` | Root of the SOC deployment (contains `config/services.json`, `log/`) |
| `SOC_API_KEY` | API key for all FastAPI service calls |
| `ASTROPACK_DATA_PATH` | Root for MATLAB data files (Windows only) |

## Architecture

### Layer Order (strict, top-to-bottom dependency only)

```
App Designer GUIs (.mlapp)  ← thin shells, short callbacks only
    ↓
PlannerMain*Helper classes  ← one helper per functional area
    ↓
MainModule                  ← central state mediator (DataModule pattern)
    ↓
API Clients (ClientBase)    ← HTTP/REST, via ClientFactory + services.json
    ↓
Core Logic (uplanner, etc.) ← deterministic, no UI, no API dependencies
    ↓
Base Classes (Loggable, @Base, @Component)
```

GUI classes never contain scientific or API logic. `MainModule` owns all application state — the App Designer shell never calls API clients or `uplanner` directly.

### Package Map

| Package | Location | Purpose |
|---|---|---|
| `+ultrasat/+api/+clients/` | `matlab/astro/` | REST clients for FastAPI services |
| `+ultrasat/+api/+core/` | `matlab/astro/` | `Loggable`, `Config` base classes |
| `+ultrasat/+planner/` | `matlab/astro/` | `uplanner` core planning logic |
| `+ultrasat/+planner/+gui/` | `matlab/astro/` | App Designer `.mlapp` files |
| `+ultrasat/+planner/+guiutils/` | `matlab/astro/` | Helper classes for PlannerMain |
| `+ultrasat/+planner/+guiutils/+debug/` | `matlab/astro/` | Debug scripts for guiutils |
| `+imProc/`, `+pipeline/` | `matlab/image/` | Image processing algorithms |
| `+db/` | `matlab/util/` | Database access layer |
| `matlab/base/` | — | `@Base`, `@Component`, `@Configuration`, `@LogFile` |
| `matlab/external/` | — | Third-party; read-only, never modify |
| `matlab/obsolete/` | — | Historical reference; read-only, never call |

### API Client Pattern

All REST clients extend `ClientBase` (which extends `Loggable`). `ClientFactory` reads `$SOC_PATH/config/services.json` to resolve service base URLs. Clients never hardcode URLs.

```matlab
factory = ultrasat.api.clients.ClientFactory();
url     = factory.getServiceBaseUrl('namespace_manager');  % or 'user_manager', etc.
client  = ultrasat.api.clients.NamespaceManagerClient(url);
response = client.getNamespaceList();  % returns struct from JSON
```

`postRequest(endpoint, params)` is the sole HTTP method — all API calls go through it. Responses are structs decoded from JSON; check `response.status == 'ok'` or `response.ok`.

### App Designer + Extracted Source

`.mlapp` files are binary. A Python script (`extract_mlapp_code.py`, invoked via `_extract_mlapp_code.bat`) exports readable `.m` snapshots into `+gui/mlapp_source/` for code review and search. **Edit only the `.mlapp` file in App Designer**; the `_code.m` files are read-only snapshots.

### MATLAB–Python IPC

Long-running MATLAB worker services (`snr_service`, `slew_service`, `too_service`, `incoming_alerts_filter`) communicate with Python via file-based IPC: Python writes a JSON/MAT request file → MATLAB polls and processes → MATLAB writes a response file. No sockets or REST between MATLAB workers and Python.

### Database

- **ClickHouse**: analytical queries, pipeline results — bulk inserts only, no row-by-row loops.
- **PostgreSQL**: transactional data (LAST mission).
- All DB access is isolated in `matlab/util/+db/`; no inline SQL elsewhere.
- Connection profiles live in `config/Database.DbConnections.*`.

## Coding Conventions

- One class or function per file; filename matches the class/function name.
- MATLAB packages use `+folder`; classes use `@folder`.
- No hardcoded absolute paths — use `@Configuration` or environment variables.
- Vectorization preferred over loops.
- MEX only after profiling; always keep a MATLAB reference implementation alongside.
- `Drafts-*` folders are dead-end experiments — never build on them.
