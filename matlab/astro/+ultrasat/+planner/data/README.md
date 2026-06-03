# Planner Grid and Field Data

Static data files used by `uplanner` and debug scripts. At runtime, copies may also exist under `$ASTROPACK_DATA_PATH/ULTRASAT/` — set that environment variable before running planner or debug scripts.

See [[../+debug/README|+debug README]] for test invocation.

---

## Files

| File | Format | Used by |
|------|--------|---------|
| `AllSS_grid_361.txt` | CSV: `Name, RA, Dec` — 361 AllSS pointings | AllSS grid construction, `constructAllSSgrid` |
| `HCS_fields.txt` | HCS field list | HCS plan building, `debug_Hcs` |
| `LCS_grid.txt` | LCS pointing grid (primary) | LCS scheduling |
| `LCS_grid0.txt` | LCS grid variant 0 | Alternate / historical grid |
| `LCS_grid1.txt` | LCS grid variant 1 | Alternate / historical grid |
| `fix_lcs.py` | Python one-off script | Generated or corrected LCS grid data |

---

## `AllSS_grid_361.txt`

Example rows:

```
Name, RA, Dec
AllSS_1, 2.974459e+02, 8.599501e+01
AllSS_2, 9.221792e+01, 8.225064e+01
...
```

361 named pointings covering the all-sky survey grid.

---

## `HCS_fields.txt`

List of HCS survey fields (coordinates / names) used when building High Cadence Survey plans.

---

## `LCS_grid*.txt`

LCS (Long Cadence Survey) pointing grids. Multiple versions exist for iteration during grid design. `LCS_grid.txt` is the primary file referenced by planner code.

`fix_lcs.py` — utility script used during grid maintenance; not part of the runtime planner loop.

---

## Runtime data path

Planner and debug scripts call `debug_ensureDataPath()` or equivalent, which expects:

```
$ASTROPACK_DATA_PATH/ULTRASAT/
```

to contain ULTRASAT-specific catalogs, grids, and probability maps (e.g. TOO CSV fixtures in `+debug/input_data/`).

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
