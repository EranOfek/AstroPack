# Alerts Filter Service — Debug Scripts

Headless test for the alerts filter IPC dispatcher.

Parent: [[../README|+alerts_filter README]]

---

## Environment

| Variable | Required |
|----------|----------|
| `SOC_PATH` | Recommended — log paths and exchange folders |

Alert files are referenced by path in the request JSON (not embedded in IPC).

---

## Script

| Script | Tests |
|--------|-------|
| `debug_processRequest` | `health` and `filter_lvk` actions |

---

## Example JSON

Service examples in `../examples/`:

| File | Action |
|------|--------|
| `01_health.json` | Health check |
| `02_filter_lvk.json` | LVK filter with `alert_file` path |

Sample alert payloads (for `alert_file`):  
`+alerts_filters/+lvk/+debug/sample_alerts/`

---

## How to run

```matlab
ultrasat.services.alerts_filter.debug.debug_processRequest()
```

Headless:

```powershell
matlab -batch "ultrasat.services.alerts_filter.debug.debug_processRequest()" 2>&1
```

---

## Flow under test

```
processRequest → processFilterLvk → LvkParsedAlert.loadFromJsonFile
  → lvk_filter → LvkFilterResult.toStruct()
```

Filter logic docs: [[../../../+alerts_filters/+lvk/+filters/README|+filters]]

---

_Last updated: 2026-06_
