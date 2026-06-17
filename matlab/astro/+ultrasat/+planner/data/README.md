# Planner Grid and Field Data

Static data files used by `uplanner` and debug scripts. At runtime, copies may also exist under `$ASTROPACK_DATA_PATH/ULTRASAT/` — set that environment variable before running planner or debug scripts.

See [[../+debug/README|+debug README]] for test invocation.

---

## Files

| File | Format | Used by |
|------|--------|---------|
| `HCS_fields.csv` | CSV: `Name, RA, Dec` — HCS survey fields | New HCS plan auto-load, HCS build |
| `LCS_fields.csv` | CSV: `Field, RA, Dec, ...` — LCS field grid | New LCS plan auto-load, LCS scheduling (`LcsHelper_v4`, etc.) |
| `AllSS_fields.csv` | CSV: `Name, RA, Dec` — AllSS pointings | AllSS grid construction |
| `obsolete/` | Legacy grids and scripts | Reference only (`LCS_grid*.txt`, `fix_lcs.py`) |

---

## `HCS_fields.csv`

High Cadence Survey field list. Loaded automatically when creating a new HCS plan in the GUI.

Example:

```
Name,RA,Dec
"HCS_S1",67,-59
```

---

## `LCS_fields.csv`

Long Cadence Survey field grid (formerly `LCS_nonoverlapping_grid_surveys.csv`). Loaded automatically when creating a new LCS plan in the GUI.

Example:

```
Field,RA,Dec,V180,V45,...
1,54.18,-86.81,1,1,...
```

---

## `AllSS_fields.csv`

All-sky survey pointing grid (361 fields).

---

## `obsolete/`

Historical LCS grid iterations (`LCS_grid.txt`, `LCS_grid0.txt`, `LCS_grid1.txt`) and maintenance scripts. Not used by the current planner runtime.

---

## Runtime data path

Planner and debug scripts call `debug_ensureDataPath()` or equivalent, which expects:

```
$ASTROPACK_DATA_PATH/ULTRASAT/
```

to contain ULTRASAT-specific catalogs and grids (e.g. `LCS_fields.csv`, `HCS_fields.csv`).

Preflight (PowerShell):

```powershell
Test-Path (Join-Path $env:ASTROPACK_DATA_PATH "ULTRASAT")
```

Details: [[../../docs/run_matlab_cli|run_matlab_cli]]

---

## Related

- [[../README|+planner core]]
- TOO probability map fixture: `+debug/input_data/lvc_2024_04_01_00_40_58_000000.csv`

---

_Last updated: 2026-06_
