# LVK Model Classes

Data models for parsed LVK alerts and filter results. Shared between Python SOC parsers and MATLAB filters.

Parent: [[../README|+lvk README]] · Filter logic: [[../+filters/README|+filters]] · JSON format: [[../doc/README|doc]]

---

## Classes

| Class | Role |
|-------|------|
| `LvkParsedAlert` | Normalized alert after parsing |
| `LvkFilterCriteria` | Configurable filter thresholds |
| `LvkFilterResult` | Filter output — score + reasons |
| `LvkFilterBase` | Base filter class (constructor only) |

---

## `LvkParsedAlert`

Parsed representation of an LVK alert. Key fields:

| Field group | Fields |
|-------------|--------|
| Identity | `alert_id`, `superevent_id`, `alert_type` |
| Timing | `time_created`, `event_time` (UTC datetime) |
| Classification | `prob_bns`, `prob_nsbh`, `prob_bbh`, `prob_terrestrial` |
| Event | `has_ns`, `has_remnant`, `has_mass_gap` |
| Rates | `far_hz`, `far_per_year` |
| Localization | `skymap_path`, `localization_area_deg2` |
| Metadata | `instruments`, `pipeline`, `search`, `raw_fields` |

### JSON round-trip

| Method | Role |
|--------|------|
| `toJsonString()` | Serialize to JSON string |
| `fromJsonString(js)` | Parse from JSON string |
| `saveToJsonFile(path)` | Write JSON file |
| `loadFromJsonFile(path)` | Load from JSON file |
| `fromStruct(s)` | Build from struct (Python parser output) |

```matlab
alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert.loadFromJsonFile('alert.json');
```

---

## `LvkFilterCriteria`

Configurable acceptance/rejection thresholds. Defaults are permissive (high max FAR, low min probs).

| Property | Default | Meaning |
|----------|---------|---------|
| `bns_min` | 0.0 | Min BNS probability |
| `terrestrial_max` | 999.0 | Max terrestrial probability |
| `far_max` | 999.0 | Max FAR [1/year] |
| `area_max` | 999999.0 | Max localization area [deg²] |

Load/save: `loadFromJsonFile`, `toJsonString`, `fromStruct`.

---

## `LvkFilterResult`

Filter output returned to the service layer.

| Property | Type | Description |
|----------|------|-------------|
| `score` | double | Weighted filter score |
| `reasons` | cell of string | Human-readable decision log |

| Method | Role |
|--------|------|
| `toStruct()` | Convert for IPC response JSON |
| `toJsonString()` | Serialize |

---

## `LvkFilterBase`

Minimal base class for filter implementations. Extend for new filter types if needed.

---

## Sample data

Fixtures: `+debug/+ultrasat/+alerts_filters/+lvk/sample_alerts/` (e.g. `lvk_alert_bns_good.json`)

Debug:

```matlab
debug.ultrasat.alerts_filters.lvk.debug_LvkParsedAlert()
debug.ultrasat.alerts_filters.lvk.debug_LvkFilterCriteria()
debug.ultrasat.alerts_filters.lvk.debug_LvkFilterResult()
```

---

_Last updated: 2026-06_
