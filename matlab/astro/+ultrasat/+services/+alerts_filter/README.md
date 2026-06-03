# Alerts Filter Service

MATLAB worker that filters incoming gravitational-wave alerts (LVK) for ULTRASAT TOO planning. Thin service wrapper around science logic in `+alerts_filters/+lvk/`.

Python bridge: `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/`

---

## Overview

**Service side** (this folder): IPC loop, file loading, dispatch.  
**Science side** (`+alerts_filters/+lvk/`): alert models and filter implementations. Scientists edit only `+filters/`.

---

## Main files

| File | Role |
|------|------|
| `alerts_filter_service.m` | Entry — creates `JsonFileIpc`, calls `processLoop` |
| `processRequest.m` | Dispatches `health`, `filter_lvk` |
| `+debug/debug_processRequest.m` | Headless test scenarios |

---

## IPC folders

```
$SOC_PATH/runtime/exchange/alerts_filter/
├── input/
├── processed/
└── alerts_filter.watchdog
```

---

## Actions

### `health`

Returns `{ "status": "ok", "message": "health: OK" }`.

### `filter_lvk`

Minimal request JSON (alert content stays in a separate file):

```json
{
  "action": "filter_lvk",
  "filter": "simple",
  "alert_file": "/path/to/alert.json"
}
```

Optional for criteria-based filter:

```json
{
  "action": "filter_lvk",
  "filter": "with_criteria",
  "alert_file": "/path/to/alert.json",
  "criteria_file": "/path/to/criteria.json"
}
```

### Response

```json
{
  "status": "ok",
  "message": "processFilterLvk: OK",
  "result": {
    "score": 1.5,
    "reasons": ["BNS contribution: 0.750", "..."]
  }
}
```

---

## Processing flow

```
processRequest  →  processFilterLvk  →  LvkParsedAlert.loadFromJsonFile
  →  lvk_filter  →  lvk_filter_simple | lvk_filter_with_criteria  →  LvkFilterResult
```

Filter names: `simple`, `with_criteria` (or `criteria`).

---

## Running

```matlab
ultrasat.services.alerts_filter.alerts_filter_service()
```

Debug:

```matlab
ultrasat.services.alerts_filter.debug.debug_processRequest()
```

Sample alerts: `+alerts_filters/+lvk/+debug/sample_alerts/`

---

## Python cross-references

| Topic | Path |
|-------|------|
| Bridge README | `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/README.md` |
| Bridge API | `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/api.py` |
| Endpoint | `POST /filter_lvk` |

---

## Related docs

- [[../../+alerts_filters/README|+alerts_filters package]]
- [[../../+alerts_filters/+lvk/README|LVK subsystem]]
- Cursor rule: `.cursor/rules/05_alerts_filter_end_to_end_flow.mdc`

---

_Last updated: 2026-06_
