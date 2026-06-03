# API Utility Classes

Static helper classes for JSON, datetime, path, and plan conversions. Used by `+clients/`, `MainModule`, and debug scripts.

Parent: [[../README|+api README]]

---

## Class reference

| Class | Key methods | Role |
|-------|-------------|------|
| `JsonUtils` | `json2struct`, `struct2json`, `class2struct`, `struct2class` | JSON ↔ struct/class; auto datetime conversion |
| `DateTimeUtils` | `convertStringToDatetime`, `convertDatetimeToString` | UTC/ISO datetime parsing for API payloads |
| `MatBase64Utils` | encode/decode MAT ↔ base64 | Transport plan MAT files via REST |
| `PathUtils` | SOC path helpers | Resolve paths under `$SOC_PATH` |
| `PlanDataUtils` | plan struct conversions | Map between `uplanner` tables and API structs |
| `LogManager` | log routing | File/console log configuration |

---

## `JsonUtils`

Central JSON handling for API clients:

```matlab
s = ultrasat.api.utils.JsonUtils.json2struct(jsonText);
js = ultrasat.api.utils.JsonUtils.struct2json(s);
```

`json2struct` automatically converts ISO datetime strings via `DateTimeUtils`.

---

## `DateTimeUtils`

Handles the mismatch between JSON string datetimes and MATLAB `datetime` objects. All planner/API datetime fields pass through here during encode/decode.

---

## `MatBase64Utils`

Used by `PlansManagerClient` for uploading/downloading `.mat` plan snapshots through the API.

---

## Debug

```matlab
ultrasat.api.debug.utils.debug_parseIsoDatetime()
ultrasat.api.debug.utils.debug_PathUtils()
```

---

_Last updated: 2026-06_
