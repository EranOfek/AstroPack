# Slew Calc Service — Debug Scripts

Headless tests for slew calculation IPC and wrappers.

Parent: [[../README|+slew_calc README]] · Examples: `../examples/`

---

## Environment

| Variable | Required |
|----------|----------|
| `SOC_PATH` | Yes — for `debug_processRequest` (uses exchange folders or local test paths) |

---

## Scripts

| Script | Tests |
|--------|-------|
| `debug_processRequest` | Full dispatcher — `health`, `slew`, `slew_batch`, `power_limits` |
| `debug_calcSlewWrapper` | Direct `calcSlewWrapper` — RA/Dec slew time |
| `debug_powerLimitsWrapper` | `powerLimitsWrapper` — sun angle limits from ISO times/coords |

---

## Example JSON

Sample requests in `../examples/`:

| File | Action |
|------|--------|
| `01_health.json` | `health` |
| `02_slew.json` | Single slew |
| `03_slew_batch.json` | Batch slew |
| `04_power_limits.json` | Power limits check |

Pre-computed outputs: `*.json.out` files alongside examples.

---

## How to run

```matlab
ultrasat.services.slew_calc.debug.debug_processRequest()
ultrasat.services.slew_calc.debug.debug_calcSlewWrapper()
ultrasat.services.slew_calc.debug.debug_powerLimitsWrapper()
```

---

## Related

- Slew tool: [[../../../+tools/README|+tools/calcSlew]]
- Python bridge: `python/prj/nova/soc/matlab_bridges/slew_calc_bridge/`

---

_Last updated: 2026-06_
