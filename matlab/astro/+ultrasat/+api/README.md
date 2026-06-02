# SOC API — MATLAB Clients

https://chatgpt.com/c/6756dedd-4c2c-8012-adad-4772c6780623

### Links

- https://chatgpt.com/c/6756dedd-4c2c-8012-adad-4772c6780623
- https://chatgpt.com/c/67ab1715-f968-8012-8b0c-af5e8fd8e61f

### Documentation

https://docs.google.com/document/d/1ODzIBimM61pVooufYwQnOLG_k1QfiemzKeZ0UIfo_9Y/edit?usp=sharing

---

## Overview

MATLAB REST clients for the ULTRASAT SOC FastAPI services. Used by the Observation Planner GUI (`MainModule`) to load/save plans, validate targets, manage sessions, and query virtual time.

**Python is the source of truth.** MATLAB clients mirror `python/prj/nova/soc/common/clients/`. When verifying or extending clients, read the Python client and Pydantic models first.

---

## Folder Structure

```
matlab/astro/+ultrasat/+api/
├── +core/
│   ├── Loggable.m          # Base logging for clients and helpers
│   └── Config.m            # Configuration handle
├── +clients/
│   ├── ClientBase.m        # HTTP POST, api-key header
│   ├── ClientFactory.m     # Loads $SOC_PATH/config/services.json
│   ├── PlansManagerClient.m
│   ├── ScheduleManagerClient.m
│   ├── NamespaceManagerClient.m
│   ├── UserManagerClient.m
│   ├── ValidatorManagerClient.m
│   ├── VirtualTimeClient.m
│   ├── UplannerClient.m    # Legacy/remote handle
│   ├── config/services.json  # Reference copy (live file is under SOC_PATH)
│   └── obsolete/           # Legacy MissionClient, sim clients — do not extend
├── +models/
│   ├── PlanData.m          # Plan DB/API model
│   └── VirtualTimeModels.m
├── +utils/
│   ├── JsonUtils.m
│   ├── PlanDataUtils.m
│   ├── DateTimeUtils.m
│   ├── MatBase64Utils.m
│   ├── PathUtils.m
│   └── LogManager.m
├── +debug/
│   ├── +clients/           # Headless verification (debug_ClientFactory, ...)
│   ├── +core/
│   └── +utils/
├── docs/
│   ├── testing_namespace_manager_client.md
│   └── README.md
└── obsolete/               # MissionClientInterface — reference only
```

---

## Key Components

### ClientFactory

Loads `$SOC_PATH/config/services.json` once and resolves service base URLs:

- **`direct` mode** — per-service port from config
- **`nginx` mode** — `base_api_url + service_path`

Also reads `SOC_API_KEY` for the `api-key` header.

```matlab
factory = ultrasat.api.clients.ClientFactory();
url = factory.getServiceBaseUrl('plans_manager');
client = ultrasat.api.clients.PlansManagerClient('BaseUrl', url, 'ApiKey', factory.getApiKey());
```

### ClientBase

All active clients extend `ClientBase` → `Loggable`. HTTP goes through `postRequest(endpoint, params)`. Clients return error structs — they do not call `error()`.

### Active clients

| MATLAB client | Python counterpart | Purpose |
|---------------|-------------------|---------|
| `PlansManagerClient` | `soc/common/clients/plans_manager.py` | Plan CRUD, MAT upload/download |
| `ScheduleManagerClient` | `soc/common/clients/schedule_manager.py` | Scheduled targets |
| `NamespaceManagerClient` | `soc/common/clients/namespace_manager.py` | Namespaces, login context |
| `UserManagerClient` | `soc/common/clients/user_manager.py` | Users, permissions |
| `ValidatorManagerClient` | `soc/common/clients/validator_manager.py` | Target validation |
| `VirtualTimeClient` | `soc/common/clients/virtual_time_manager.py` | Simulation virtual time |

---

## Environment

| Variable | Purpose |
|----------|---------|
| `SOC_PATH` | Must contain `config/services.json` and `log/` |
| `SOC_API_KEY` | API key for all service calls |

Preflight:

```matlab
ultrasat.api.debug.clients.debug_ClientFactory()
```

See [[docs/testing_namespace_manager_client|Namespace Manager testing]] for a full walkthrough.

---

## Usage

### Typical planner flow (via MainModule)

The GUI never calls clients directly. `MainModule` wires clients after login:

```
GUI callback → MainModule → PlansManagerClient / ValidatorManagerClient / ...
```

### Debug verification loop

1. Read Python client + response model in `python/prj/nova/soc/common/models/`
2. Confirm MATLAB sends/reads only those fields
3. Run matching `debug_*` script with `-batch`
4. Fix MATLAB until struct matches Python contract

Example:

```powershell
matlab -batch "ultrasat.api.debug.clients.debug_PlansManagerClient()"
```

---

## Python cross-references

| Topic | Path |
|-------|------|
| Sync HTTP clients | `python/prj/nova/soc/common/clients/` |
| API models | `python/prj/nova/soc/common/models/` |
| FastAPI services | `python/prj/nova/soc/platform/`, `python/prj/nova/soc/mission/` |
| Env conventions | `python/prj/nova/integration/soc_env_conventions.md` |

---

## What not to use

Files under `+clients/obsolete/` and `+debug/+clients/obsolete/` (MissionApiClient, SimpleFileClient, UserManagerSim, etc.) are legacy reference only. Do not extend them for new work.

---

_Last updated: 2026-06_
