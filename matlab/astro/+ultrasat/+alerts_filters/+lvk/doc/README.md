# LVK Parsed Alert Format

Reference for JSON files consumed by `LvkParsedAlert.loadFromJsonFile`. These files are produced by the Python alert parser and passed to MATLAB via `alert_file` in the IPC request (not embedded in the IPC JSON).

Model class: `+models/LvkParsedAlert.m`

---

## Required fields

| Field | Type | Description |
|-------|------|-------------|
| `alert_id` | string | Unique alert ID (e.g. `G2026-0001`) |
| `superevent_id` | string | Superevent identifier |
| `alert_type` | string | e.g. `PRELIMINARY`, `UPDATE` |

---

## Timing (UTC ISO 8601)

| Field | Type | Description |
|-------|------|-------------|
| `time_created` | string/datetime | Alert creation time |
| `event_time` | string/datetime | Event time |

---

## Classification probabilities (0..1)

| Field | Description |
|-------|-------------|
| `prob_bns` | Binary neutron star |
| `prob_nsbh` | Neutron star + black hole |
| `prob_bbh` | Binary black hole |
| `prob_terrestrial` | Terrestrial origin |

---

## Event properties (0..1)

| Field | Description |
|-------|-------------|
| `has_ns` | Has neutron star |
| `has_remnant` | Has remnant |
| `has_mass_gap` | Mass gap event |

---

## Rates and localization

| Field | Type | Description |
|-------|------|-------------|
| `far_hz` | double | False alarm rate [Hz] |
| `far_per_year` | double | FAR [1/year] |
| `skymap_path` | string | Path to localization skymap |
| `localization_area_deg2` | double | 90% credible area [deg²] |

---

## Metadata

| Field | Type | Description |
|-------|------|-------------|
| `instruments` | string array | e.g. `["H1","L1","V1"]` |
| `pipeline` | string | e.g. `gstlal` |
| `search` | string | e.g. `CBC` |
| `raw_fields` | struct | Unmapped parser fields |

---

## Example (minimal good BNS alert)

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

### Simple filter (`lvk_filter_simple`)

Weighted score from classification probs; rejects high-FAR or missing-skymap alerts. See [[../+filters/README|+filters README]].

### Criteria filter (`lvk_filter_with_criteria`)

Thresholds in `LvkFilterCriteria`:

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

Parsed alert schema should stay aligned with Python parser output:

- `python/prj/nova/soc/alert_parsers/`
- Bridge passes file path only: `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/README.md`

---

_Last updated: 2026-06_
