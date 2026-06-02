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

Additional rules may reject alerts with high FAR or missing skymap (see source).

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

---

## Adding a new filter

1. Add `lvk_filter_myname.m` in this folder
2. Add a `case` in `lvk_filter.m` switch
3. Add debug script in `+debug/debug_lvk_filter_myname.m`
4. Add sample alert JSON if needed
5. Do **not** change `+services/+alerts_filter/` unless IPC contract changes

---

## Debug

```matlab
ultrasat.alerts_filters.lvk.debug.debug_lvk_filter()
ultrasat.alerts_filters.lvk.debug.debug_lvk_filter_simple()
ultrasat.alerts_filters.lvk.debug.debug_lvk_filter_with_criteria()
```

Fixtures: `+debug/sample_alerts/`

---

_Last updated: 2026-06_
