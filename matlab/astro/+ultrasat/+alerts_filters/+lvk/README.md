# LVK Alert Filtering

Parsed LVK (LIGO-Virgo-KAGRA) alert models and filter implementations for ULTRASAT incoming-alert triage.

Service wrapper: [[../../+services/+alerts_filter/README|+services/+alerts_filter]]

---

## Package layout

```
+lvk/
├── +models/
│   ├── LvkParsedAlert.m      Parsed alert (JSON round-trip)
│   ├── LvkFilterCriteria.m   Configurable thresholds
│   ├── LvkFilterResult.m     Score + reasons
│   └── LvkFilterBase.m       Base class
├── +filters/
│   ├── lvk_filter.m          Entry point — dispatches by filter name
│   ├── lvk_filter_simple.m   FAR / classification scoring
│   └── lvk_filter_with_criteria.m  Criteria-driven rules
├── (debug scripts)  → +debug/+ultrasat/+alerts_filters/+lvk/
│   ├── debug_lvk_filter*.m
│   └── sample_alerts/        Example JSON alerts
└── doc/
    └── README.md             Alert field reference
```

---

## Models

### LvkParsedAlert

Normalized alert after Python parser or JSON load. Key fields:

| Field | Type | Description |
|-------|------|-------------|
| `alert_id` | string | Alert identifier |
| `superevent_id` | string | Superevent ID |
| `prob_bns`, `prob_nsbh`, `prob_bbh`, `prob_terrestrial` | double | Classification probs |
| `far_hz`, `far_per_year` | double | False alarm rate |
| `skymap_path` | string | Path to skymap FITS |
| `localization_area_deg2` | double | 90% area |

Load from file:

```matlab
alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert.loadFromJsonFile('alert.json');
```

### LvkFilterResult

Output of any filter: `score` (double), `reasons` (cell of strings).

---

## Filters

Entry point: `ultrasat.alerts_filters.lvk.filters.lvk_filter(Input, logger)`

| Filter name | Function | Use case |
|-------------|----------|----------|
| `simple` | `lvk_filter_simple` | Default — weighted classification score |
| `with_criteria`, `criteria` | `lvk_filter_with_criteria` | Configurable thresholds via `LvkFilterCriteria` |

See [[+filters/README|+filters README]] for dispatch details.

---

## Debug

Sample alerts in `+debug/+ultrasat/+alerts_filters/+lvk/sample_alerts/`:

- `lvk_alert_bns_good.json`
- `lvk_alert_high_far.json`
- `lvk_alert_terrestrial.json`
- `lvk_alert_nsbh.json`
- `lvk_alert_minimal.json`

Run:

```matlab
ultrasat.alerts_filters.lvk.debug.debug_lvk_filter()
ultrasat.alerts_filters.lvk.debug.debug_lvk_filter_simple()
ultrasat.alerts_filters.lvk.debug.debug_lvk_filter_with_criteria()
```

---

## Python cross-references

Alert parsing and ingestion happen in Python before the MATLAB filter runs:

- `python/prj/nova/soc/alert_parsers/`
- `python/prj/nova/soc/alert_listeners/`

The MATLAB side expects a **parsed** JSON file compatible with `LvkParsedAlert.fromStruct`.

---

_Last updated: 2026-06_
