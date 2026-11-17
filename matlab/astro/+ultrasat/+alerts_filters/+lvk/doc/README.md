# LVK Parsed Alert Format

Reference for JSON files consumed by `LvkParsedAlert.loadFromJsonFile`. Python writes slim `parsed_alert.json` under the alert work directory; the MATLAB bridge passes only `alert_file` in the IPC request (path, not embedded JSON).

Model: `+models/LvkParsedAlert.m` — **flat filter properties are hydrated at load time**, not required on disk.

Other MATLAB tools (e.g. GW `debug_runGwCoverageRequest` plan-compare CSV) should call `loadFromJsonFile` instead of reading flat top-level keys from disk JSON.

Canonical schema (Python): [PARSED_ALERT_PRODUCTS.md](c:/Ultrasat/Ultrasat.git/python/prj/nova/soc/alert_parsers/lvc/PARSED_ALERT_PRODUCTS.md) (Ultrasat repo).

---

## Slim schema (current)

Top-level **identity and times**, plus nested nodes. Flat fields such as `prob_bns` and `far_per_year` come from `alert.event.*` and `derived.*` inside `fromStruct` → `hydrateFromAlertAndDerived`.

| Top-level | Description |
|-----------|-------------|
| `alert_id`, `superevent_id`, `alert_type` | Identity |
| `time_created`, `event_time` | UTC ISO 8601 |
| `alert` | GraceDB notice payload without embedded skymap blob |
| `fits_header` | Decoded FITS header dict (may be empty) |
| `derived` | Python-derived: `far_hz`, `far_per_year`, `localization_area_deg2`, `skymap_path`, etc. |

Typical nested sources (see Python doc for full tree):

| Runtime property | JSON source |
|------------------|-------------|
| `prob_bns`, `prob_nsbh`, … | `alert.event.classification.*` |
| `has_ns`, `has_remnant`, … | `alert.event.properties.*` |
| `far_hz`, `far_per_year` | `derived.*` preferred, else `alert.event.far` |
| `skymap_path`, `localization_area_deg2` | `derived.*` |

**Transitional** GW share files may omit `alert` but include `raw_fields.lvc_notice`; MATLAB rebuilds `derived` via `buildDerivedFromRawFields` before hydration.

---

## Legacy flat schema

Older fixtures and pre-slim exports may expose classification, FAR, and skymap fields **at the top level** (and optionally `raw_fields`). `fromStruct` branch C calls `hydrateLegacyFlatFields`. **New Python output should not use this shape.**

| Field | Type | Description |
|-------|------|-------------|
| `alert_id` | string | Unique alert ID (e.g. `G2026-0001`) |
| `superevent_id` | string | Superevent identifier |
| `alert_type` | string | e.g. `PRELIMINARY`, `UPDATE` |
| `time_created`, `event_time` | string/datetime | Timing (UTC) |
| `prob_bns`, `prob_nsbh`, `prob_bbh`, `prob_terrestrial` | double | Classification (0..1) |
| `has_ns`, `has_remnant`, `has_mass_gap` | double | Event properties (0..1) |
| `far_hz`, `far_per_year` | double | False alarm rate |
| `skymap_path` | string | Localization skymap file |
| `localization_area_deg2` | double | 90% credible area [deg²] |
| `instruments`, `pipeline`, `search` | various | Metadata |
| `raw_fields` | struct | Unmapped parser fields (legacy) |

### Example (legacy minimal BNS)

See `+debug/+ultrasat/+alerts_filters/+lvk/sample_alerts/lvk_alert_bns_good.json`:

```json
{
  "alert_id": "G2026-0001",
  "superevent_id": "S2026abcd",
  "alert_type": "PRELIMINARY",
  "prob_bns": 0.82,
  "prob_terrestrial": 0.05,
  "far_per_year": 0.004,
  "skymap_path": "/data/lvk/.../skymap.fits",
  "localization_area_deg2": 45.2
}
```

Other fixtures: `lvk_alert_high_far.json`, `lvk_alert_terrestrial.json`, `lvk_alert_nsbh.json`, `lvk_alert_minimal.json`

---

## Filter decision logic

Service entry: `+services/+alerts_filter/processRequest.m` → `loadFromJsonFile` → `lvk_filter`.

### Simple filter (`lvk_filter_simple`)

Weighted score from hydrated `prob_*`; flags use `far_per_year` and `skymap_path`. See [[../+filters/README|+filters README]].

### Criteria filter (`lvk_filter_with_criteria`)

Thresholds in `LvkFilterCriteria` (IPC JSON, `criteria_file`, or defaults):

| Property | Default | Meaning |
|----------|---------|---------|
| `bns_min` | 0.0 | Min BNS probability |
| `terrestrial_max` | 999.0 | Max terrestrial probability |
| `far_max` | 999.0 | Max FAR [1/year] |
| `area_max` | 999999.0 | Max localization area |

Criteria JSON example:

```json
{
  "bns_min": 0.5,
  "terrestrial_max": 0.3,
  "far_max": 10.0
}
```

---

## Python cross-references

- Schema: `python/prj/nova/soc/alert_parsers/lvc/PARSED_ALERT_PRODUCTS.md` (Ultrasat)
- Parsers: `python/prj/nova/soc/alert_parsers/`
- Bridge: `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/README.md`

---

_Last updated: 2026-11_