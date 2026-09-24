# ULTRASAT Observation Planner — Core

Core planning logic for the ULTRASAT Observation Planner. No GUI or HTTP code here — used by the GUI (`+guiutils/MainModule`), debug scripts (`+debug/+ultrasat/+planner/`; see [debug/CLAUDE.md](../../../../debug/CLAUDE.md)), and the TOO service (`TooPlannerRunner`).

Architecture: [[diagrams/planner_layers.mmd]]

---

## Main classes and files

| File | Role |
|------|------|
| `uplanner.m` | Central planning class — HCS, LCS, AllSS, DDT, TOO |
| `TooPlannerRunner.m` | Batch TOO runs from one JSON config (service + debug) |
| `LcsHelper.m` | LCS scheduling helper used by `uplanner` |
| `AllSSHelper.m` | All-sky survey helper used by `uplanner` |
| `unitTest.m` | Planner unit test |
| `analyzePlannerSize.m` | Memory/size analysis for planner objects |
| `collectAlertStatistics.m` | Alert statistics utility |
| `plannerToO.m` | Planner → O-format conversion |

---

## `uplanner` — plan types

Allowed types: **HCS**, **LCS**, **AllSS**, **DDT**, **TOO**

| Method | Purpose |
|--------|---------|
| `buildHCS` | High Cadence Survey — single field, fixed window |
| `buildLCS` | Long Cadence Survey — daily windows over target list |
| `buildTOO` | Target of Opportunity — probability map coverage |
| `addDDT2Plan` | Add DDT targets to existing plan |
| `buildAllSS` | All-sky survey (in development) |
| `addUniqTargets` | Add RA/Dec to unique target list |
| `scheduleTargets` | Schedule a group with slew times |
| `validate` | Send plan to validator |
| `submit` | Submit plan to mission C&C |
| `prepareForSave` / `restoreAfterLoad` | Serialization for DB/file |

### Status lifecycle

```
draft → (schedule) → draft
      → (validate) → validated
      → (submit)   → submitted
```

Key properties: `Plan` (target table), `UniqTargList`, `Vis` (visibility), `MissionApprovedPlan`, `Status`, `Scheduled`, `Validated`, `Submitted`.

---

## `TooPlannerRunner`

Runs one or more TOO plans from a single JSON configuration. Used by:

- `+services/+too_planner/processRequest.m`
- `+debug/+ultrasat/+planner/debug_TooPlannerRunner.m`

Design: **fail-safe** — one failed plan does not stop others. Outputs per plan: JSON, MAT, PNG/FIG maps, plus `summary.json`.

See [[../+services/+too_planner/README|TOO planner service docs]].

---

## Data files

Grid and field lists in [[data/README|data/]]:

- `AllSS_grid_361.txt`, `HCS_fields.txt`, `LCS_grid*.txt`

Loaded via `ASTROPACK_DATA_PATH` (see `data/README.md`).

---

## Unit test

```matlab
ultrasat.planner.unitTest()
```

---

## Debug scripts

Headless step-by-step tests for each plan type: [[../../../../debug/+debug/+ultrasat/+planner/README|planner debug README]]

```matlab
debug.ultrasat.planner.debug_Hcs()
debug.ultrasat.planner.debug_Lcs()
debug.ultrasat.planner.debug_Too()
debug.ultrasat.planner.debug_TooPlannerRunner()
```

Requires `ASTROPACK_DATA_PATH`. CLI patterns: [[../docs/run_matlab_cli|run_matlab_cli]].

---

## Related packages

| Package | Role |
|---------|------|
| [[+guiutils/README|+guiutils]] | `MainModule`, GUI helpers |
| [[+gui/README|+gui]] | App Designer apps |
| [[+webpage/README|+webpage]] | HTML plan export |
| [[../+tools/README|+tools]] | Slew, FOV, extinction utilities |
| [[../+api/README|+api]] | REST clients for save/validate/submit |

---

## Python cross-references

| Topic | Path |
|-------|------|
| TOO manager | `python/prj/nova/soc/mission/too_manager/` |
| Plans manager | `python/prj/nova/soc/mission/plans_manager/` |
| Validator | `python/prj/nova/soc/mission/validator_manager/` |
| TOO alert workflow | `python/prj/nova/soc/mission/too_manager/too_alert_workflow_architecture_design.md` |

---

_Last updated: 2026-06_
