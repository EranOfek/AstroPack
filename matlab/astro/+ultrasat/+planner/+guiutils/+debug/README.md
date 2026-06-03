# GUI Utils Debug Scripts

Headless tests for `MainModule` and GUI helper classes. Used to verify login, plan storage, and preferences without launching App Designer.

Parent: [[../README|+guiutils README]]

---

## Environment

| Script | `SOC_PATH` | `SOC_API_KEY` |
|--------|------------|---------------|
| `debug_MainModule` | Required | Required |
| `debug_login` | Required | Required |
| `debug_PlannerStorageHelper` | Required | Required |
| `debug_AppUtils` | Not required | Not required |
| `debug_Preferences` | Not required | Not required |

---

## Scripts

| Script | Tests |
|--------|-------|
| `debug_MainModule` | Full `MainModule` init — login, client wiring, planner instance |
| `debug_login` | Login flow only |
| `debug_PlannerStorageHelper` | Open/save/load plans via storage helper |
| `debug_AppUtils` | `AppUtils` helper methods |
| `debug_Preferences` | User preferences JSON load/save |

---

## How to run

```matlab
ultrasat.planner.guiutils.debug.debug_MainModule()
ultrasat.planner.guiutils.debug.debug_login()
```

Headless:

```powershell
matlab -batch "ultrasat.planner.guiutils.debug.debug_MainModule()" 2>&1
```

Ensure `SOC_PATH` and `SOC_API_KEY` are set before running session/storage scripts.

---

## Related

- API client debug: [[../../+api/+debug/+clients/README|+api/+debug/+clients]]
- Planner core debug: [[../../+debug/README|+planner/+debug]]

---

_Last updated: 2026-06_
