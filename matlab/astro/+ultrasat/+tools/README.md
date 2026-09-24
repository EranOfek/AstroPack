# ULTRASAT Planning Tools

Small focused utilities used by the observation planner (`+planner/uplanner.m`) and MATLAB services (notably slew calculation).

---

## Function reference

| Function | Signature (summary) | Purpose |
|----------|---------------------|---------|
| `calcSlew` | `[T_sec, DirectSlewBool] = calcSlew(RA_1, Dec_1, RA_2, Dec_2, Args)` | Slew time [s] between two RA/Dec pointings |
| `expectedRoll` | `Roll = expectedRoll(RA, Dec, JD, Args)` | Expected spacecraft roll angle [deg] |
| `getFOVcorners` | `[FOV, Tiles] = getFOVcorners(RA0, Dec0, Args)` | FOV corner coordinates + tile corners |
| `getFOVcircle` | `Circle = getFOVcircle(RA, Dec, Args)` | FOV as circular region |
| `getFOVcatalog` | `Cat = getFOVcatalog(RA, Dec, Args)` | Astrometric catalog sources in FOV |
| `getFOVcatalogWD` | `Cat = getFOVcatalogWD(RA, Dec, Args)` | White dwarf catalog in FOV |
| `coverProbMap` | `[RA, Dec, Stat] = coverProbMap(SkyMap, Args)` | Pointings to cover a probability skymap |
| `extinction` | `A_U = extinction(RA, Dec, Args)` | UV extinction A_U at coordinates |
| `ditherGrid` | `[Grid, GroupNum] = ditherGrid(Grid0, Args)` | Generate dither offset grid |
| `distributeAllSS` | `[DailyTab, ...] = distributeAllSS(...)` | Distribute AllSS targets over daily windows |
| `mergeAllSSTargetList` | `[UniqTargets, Nexp] = mergeAllSSTargetList(UTargets, Args)` | Merge AllSS unique target lists |

All functions use `arguments` blocks or `Args` name-value pairs for optional parameters.

---

## Usage from planner

- **`uplanner`** calls `calcSlew`, FOV helpers, and `extinction` during scheduling and property updates
- **`coverProbMap`** supports TOO probability-map planning
- **`distributeAllSS`** / **`mergeAllSSTargetList`** support AllSS plan building

---

## Usage from services

| Service | Tool |
|---------|------|
| `+services/+slew_calc/calcSlewWrapper.m` | Wraps `calcSlew` for IPC |
| `+services/+slew_calc/powerLimitsWrapper.m` | Uses `PowerLimits.m` (package root) |

Slew service docs: [[../+services/+slew_calc/README|+slew_calc]]

---

## Package root related utilities

Some mission physics live at `+ultrasat/` root (not in `+tools/`):

- `PowerLimits.m` — sun angle soft/hard limits vs time and DOD
- `ULTRASAT_restricted_visibility.m` — visibility constraints
- `getULTRASAT_PSF.m`, `weightedPSF.m` — PSF models

---

## Debug

Slew wrappers:

```matlab
debug.ultrasat.services.slew_calc.debug_calcSlewWrapper()
debug.ultrasat.services.slew_calc.debug_processRequest()
```

---

_Last updated: 2026-06_
