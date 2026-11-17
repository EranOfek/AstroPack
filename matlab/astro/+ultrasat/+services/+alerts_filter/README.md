# Alerts Filter Service

MATLAB worker that filters incoming gravitational-wave alerts (LVK) for ULTRASAT TOO planning. Thin service wrapper around science logic in `+alerts_filters/+lvk/`.

Python bridge: `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/`

**Path rule:** every path in `result_<filter>.json` and IPC output uses `/` (via `ultrasat.tools.pathForJson`). The filter service does not create OS temp files.

---

## Overview

**Service side** (this folder): IPC loop, file loading, dispatch, **result file on disk** (like too_planner `summary.json`).  
**Science side** (`+alerts_filters/+lvk/`): alert models, filters, and **final TOO decision** (`is_too`). Scientists edit `+filters/` decision blocks.

---

## Main files

| File | Role |
|------|------|
| `alerts_filter_service.m` | Entry — creates `JsonFileIpc`, calls `processLoop` |
| `processRequest.m` | Dispatches `health`, `filter_lvk` (alias `filter_lvc`); writes `result_<filter>.json` |
| `+debug/debug_runAlertFiltersOnShare.m` | Batch all share stages; writes `alert_filter_summary.csv` |

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

### `filter_lvk` / `filter_lvc`

Request (written by Python under each alert stage as `alert_filter/request.json`):

```json
{
  "action": "filter_lvk",
  "filter": "simple",
  "alert_file": "//socsrv/.../parsed_alert.json",
  "output_folder": "//socsrv/.../alert_filter",
  "criteria_file": "optional for with_criteria"
}
```

- `output_folder` optional — defaults to `<parsed_alert parent>/alert_filter`.

**On disk:** `result_<filter>.json` in `output_folder` (full envelope + `result` object).

**IPC `.out.json`:** `status`, `message`, `result_file`, `filter`, `alert_id`, `superevent_id`, inline `result`.

### Result (`result` object)

| Field | Meaning |
|-------|---------|
| `score` | Weighted classification score |
| `is_too` | **Final TOO decision** (MATLAB science rule) |
| `decision` | `accept` / `reject` |
| `priority` | `none` / `low` / `medium` / `high` |
| `decision_reasons` | Why accepted or rejected |
| `class_probs`, `flags`, `reasons` | Scoring snapshot (unchanged) |

Filter names: `simple`, `with_criteria` (or `criteria`).

---

## Processing flow

```
processRequest  →  processFilterLvk  →  LvkParsedAlert.loadFromJsonFile
  →  lvk_filter  →  lvk_filter_simple | lvk_filter_with_criteria
  →  LvkFilterResult (is_too + score)  →  result_<filter>.json
```

---

## Running

Service:

```matlab
ultrasat.services.alerts_filter.alerts_filter_service()
```

GW share batch (148 stages, both filters):

```matlab
ultrasat.services.alerts_filter.debug.debug_runAlertFiltersOnShare()
```

---

## Python cross-references

| Topic | Path |
|-------|------|
| Bridge README | `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/README.md` |
| API models | `python/prj/nova/soc/common/models/matlab_bridges/api/alerts_filter.py` |
| Request builder | `python/prj/nova/soc/alert_parsers/lvc/alert_products.py` (`build_filter_request`) |
| Parsed alert schema | `python/prj/nova/soc/alert_parsers/lvc/PARSED_ALERT_PRODUCTS.md` |

---

_Last updated: 2026-11_