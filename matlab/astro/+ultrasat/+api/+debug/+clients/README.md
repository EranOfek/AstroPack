# API Client Debug Scripts

Headless verification scripts for MATLAB REST clients. Python is the source of truth — run these after changing a client to confirm the struct contract matches `python/prj/nova/soc/common/clients/`.

Parent: [[../../README|+api README]] · Verification loop: `.cursor/rules/04_python_api_matlab_client.mdc`

---

## Environment

| Variable | Required |
|----------|----------|
| `SOC_PATH` | Yes — must contain `config/services.json` |
| `SOC_API_KEY` | Yes — sent in `api-key` header |

Preflight:

```matlab
ultrasat.api.debug.clients.debug_ClientFactory()
```

---

## Scripts

| Script | Verifies |
|--------|----------|
| `debug_ClientFactory` | `ClientFactory` — `services.json`, API key, URL resolution |
| `debug_ClientBase` | `ClientBase` — HTTP POST, auth header |
| `debug_NamespaceManagerClient` | `NamespaceManagerClient` — namespace list/login |
| `debug_PlansManagerClient` | `PlansManagerClient` — list/get plans |
| `debug_PlansManagerSavePlan` | Save new plan |
| `debug_PlansManagerSaveLoadMatlabMat` | MAT upload/download |
| `debug_PlansManagerSaveUpdatePlan` | Update existing plan |
| `debug_PlansManagerSaveUpdatePlanWithHistoryAndMetadata` | Full plan lifecycle with history |
| `debug_plannerWorkflow` | End-to-end planner workflow via API |
| `debug_ScheduleManagerClient` | `ScheduleManagerClient` — scheduled targets |
| `debug_UserManagerClient` | `UserManagerClient` — login, permissions |
| `debug_ValidatorManagerClient` | `ValidatorManagerClient` — target validation |
| `debug_VirtualTime` | `VirtualTimeClient` — virtual time ops |
| `debug_Mission` | Mission-related client checks |
| `debug_UserManagerBase` | User manager base class |
| `debug_UserManagerSim` | User manager sim (legacy) |

---

## Obsolete — do not use

`obsolete/` contains legacy MissionClient, SimpleFileClient, SkyExposureTracker, and sim debug scripts. Reference only.

---

## How to run

```powershell
matlab -batch "ultrasat.api.debug.clients.debug_PlansManagerClient()" 2>&1
```

Capture output to a log file when iterating on client fixes.

---

## Python cross-references

| Topic | Path |
|-------|------|
| Sync clients | `python/prj/nova/soc/common/clients/` |
| API models | `python/prj/nova/soc/common/models/` |
| Namespace testing guide | [[../../docs/testing_namespace_manager_client|testing_namespace_manager_client]] |

---

_Last updated: 2026-06_
