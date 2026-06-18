# Planner Debug Scripts

Headless step-by-step tests for `uplanner` plan types. Each script mirrors code paths in `unitTest.m` and is safe to run without the GUI.

Parent: [[../README|+planner core]] · CLI guide: [[../../docs/run_matlab_cli|run_matlab_cli]]

---

## Environment

| Variable | Required |
|----------|----------|
| `ASTROPACK_DATA_PATH` | Yes — must contain `ULTRASAT/` subfolder with grids and catalogs |

Scripts call `debug_ensureDataPath()` and may set a fallback if the variable is empty; prefer setting it explicitly.

---

## Scripts

| Script | Covers | Scenarios |
|--------|--------|-----------|
| `debug_Hcs` | HCS plan build | basic, inspect, custom exptime |
| `debug_Lcs` | LCS scheduling | LCS window and target scheduling |
| `debug_Ddt` | DDT plan | add DDT targets to plan |
| `debug_Too` | TOO plan | probability-map TOO build |
| `debug_AllSs` | AllSS distribution | All-sky survey distribution |
| `debug_TooPlannerRunner` | Batch TOO from JSON | full runner + artifacts |
| `debug_prepareForSave` | Plan serialization | `prepareForSave` / restore round-trip |

---

## Folders

| Folder | Purpose |
|--------|---------|
| `input_data/` | Fixtures (e.g. LVC probability map CSV). See [[input_data/README|input_data README]] |
| `working_dir/` | Generated logs, JSON, MAT, CSV during debug runs. See [[working_dir/README|working_dir README]] |

---

## How to run

From MATLAB:

```matlab
debug.ultrasat.planner.debug_Hcs()
debug.ultrasat.planner.debug_TooPlannerRunner()
```

Headless (PowerShell):

```powershell
matlab -batch "debug.ultrasat.planner.debug_Hcs()" 2>&1
```

---

## Data files

Grid and field lists: [[../data/README|+planner/data]]

---

_Last updated: 2026-06_
