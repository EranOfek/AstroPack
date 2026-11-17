# LVK Filter Functions

Concrete filter implementations for LVK alerts. Called from the service via `lvk_filter.m` — do not call the service layer from here.

Parent: [[../README|+lvk README]]

---

## Entry point — `lvk_filter.m`

```matlab
result = ultrasat.alerts_filters.lvk.filters.lvk_filter(Input, logger);
```

**Input requirements:**

- `Input.alert` — `LvkParsedAlert` object (loaded by service)
- `Input.filter` — filter name (optional, default `"simple"`)
- `Input.criteria` or `Input.criteria_file` — for criteria-based filter

**Returns:** `LvkFilterResult`

### Dispatch table

| `Input.filter` | Handler |
|----------------|---------|
| `simple` | `lvk_filter_simple` |
| `with_criteria`, `criteria` | `lvk_filter_with_criteria` |

Unknown filter names raise an error.

---

## `lvk_filter_simple.m`

Default filter. Computes a weighted score from classification probabilities:

| Signal | Weight |
|--------|--------|
| `prob_bns` | +2.0 × value |
| `prob_nsbh` | +1.5 × value |
| `prob_bbh` | +0.2 × value |
| `prob_terrestrial` | −2.0 × value (penalty) |

Score is floored at 0. Each contribution is recorded in `result.reasons`.

**TOO decision** (`applyTooDecisionSimple`, end of file): `is_too=true` when score ≥ 1.0, `far_per_year` ≤ 10, skymap present, and `prob_terrestrial` &lt; 0.5. Priority: high ≥ 1.6, medium ≥ 1.3, else low.

---

## `lvk_filter_with_criteria.m`

Uses `LvkFilterCriteria` for configurable thresholds:

- Max FAR
- Min BNS probability
- Max terrestrial probability
- Localization area limits
- Custom reason strings on pass/fail

Criteria from:

```matlab
criteria = ultrasat.alerts_filters.lvk.models.LvkFilterCriteria();
% or loadFromJsonFile, or Input.criteria struct
```

`LvkFilterCriteria.score_min` (default 1.0) gates `is_too` after FAR/terrestrial rejection flags. Skymap required.

---

## Adding a new filter

1. Add `lvk_filter_myname.m` in this folder
2. Add a `case` in `lvk_filter.m` switch
3. Add debug script in `+debug/+ultrasat/+alerts_filters/+lvk/debug_lvk_filter_myname.m`
4. Add sample alert JSON if needed
5. Do **not** change `+services/+alerts_filter/` unless IPC contract changes

---

## Debug

```matlab
debug.ultrasat.alerts_filters.lvk.debug_lvk_filter()
debug.ultrasat.alerts_filters.lvk.debug_lvk_filter_simple()
debug.ultrasat.alerts_filters.lvk.debug_lvk_filter_with_criteria()
```

Fixtures: `+debug/+ultrasat/+alerts_filters/+lvk/sample_alerts/`

---

_Service writes `alert_filter/result_<filter>.json`; see `+services/+alerts_filter/README.md`._

_Last updated: 2026-11_
