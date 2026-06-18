# +alerts_filters — Alert Filter Logic

Science-side package for parsing and filtering incoming alerts (currently LVK gravitational-wave alerts). Used by the MATLAB service at `+services/+alerts_filter/` and ultimately by the Python SOC alert workflow.

---

## Architecture separation

| Layer | Path | Who edits |
|-------|------|-----------|
| Service / IPC | `+services/+alerts_filter/` | Platform / ops |
| Models | `+lvk/+models/` | Shared schema |
| Filters | `+lvk/+filters/` | Scientists |
| Debug / fixtures | `+debug/+ultrasat/+alerts_filters/+lvk/` | Developers |

Scientists modify **only** `+lvk/+filters/`. Never touch the service loop, IPC, or watchdog.

---

## Subpackages

```
+alerts_filters/
└── +lvk/
    ├── +models/     LvkParsedAlert, LvkFilterCriteria, LvkFilterResult, LvkFilterBase
    ├── +filters/    lvk_filter, lvk_filter_simple, lvk_filter_with_criteria
    ├── (debug scripts)  → +debug/+ultrasat/+alerts_filters/+lvk/
    └── doc/         LVK format reference
```

Docs: [[+lvk/README|+lvk README]]

---

## End-to-end flow

```
Python alerts_filter_bridge
  → JSON { action, filter, alert_file }
  → alerts_filter_service (MATLAB)
  → processFilterLvk
  → LvkParsedAlert.loadFromJsonFile
  → lvk_filter (dispatch)
  → LvkFilterResult.toStruct()
  → response JSON
```

Diagram: [[../diagrams/ipc_flow.mmd]]

---

## Python cross-references

| Topic | Path |
|-------|------|
| Alerts filter bridge | `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/` |
| Alert listeners / parsers | `python/prj/nova/soc/alert_listeners/`, `alert_parsers/` |
| TOO alert workflow | `python/prj/nova/soc/mission/too_manager/too_alert_workflow_architecture_design.md` |

---

_Last updated: 2026-06_
