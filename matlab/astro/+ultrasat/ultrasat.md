# +ultrasat — ULTRASAT Mission MATLAB Package

Mission-specific MATLAB code for the ULTRASAT Science Operations Center (SOC). This package sits under AstroPack at `matlab/astro/+ultrasat/` and covers observation planning, REST API clients, long-running MATLAB workers, alert filtering, and mission science utilities.

Python SOC services live in a separate repo: `python/prj/nova/soc/`.

See also: [[diagrams/architecture_overview.mmd]] · [[diagrams/ipc_flow.mmd]]

---

## Overview

| Area | Path | Role |
|------|------|------|
| Observation planner | `+planner/` | Core planning (`uplanner`), GUI, helpers, HTML export |
| REST API clients | `+api/` | MATLAB mirrors of Python SOC HTTP clients |
| MATLAB workers | `+services/` | Long-running file-IPC services (slew, SNR, TOO, alerts) |
| Alert filters | `+alerts_filters/` | LVK alert models and filter logic (science side) |
| Mission tools | `+tools/` | Slew, FOV, extinction, AllSS helpers |
| Package root | `*.m` | PSF, visibility, simulation, zodiac, scheduling utilities |

Architecture diagram: `matlab/astro/+ultrasat/diagrams/architecture_overview.mmd`

---

## List of Subpackages

### `+planner/` — Observation Planner

- **`uplanner.m`** — Core planning class (HCS, LCS, AllSS, DDT, TOO)
- **`+gui/`** — App Designer `.mlapp` shells (edit `.mlapp`, not `mlapp_source/`)
- **`+guiutils/`** — `MainModule` (DataModule) and `PlannerMain*` helpers
- **`+webpage/`** — HTML plan/target export from templates
- **`+debug/+ultrasat/` (see [debug/CLAUDE.md](../../../debug/CLAUDE.md))** — Headless debug scripts for planner modes

Docs: [[+planner/+guiutils/README|+guiutils README]] · [[+planner/+gui/README|+gui README]] · [[+planner/+webpage/README|+webpage README]]

### `+api/` — SOC REST Clients

HTTP clients for plans, schedule, namespace, user, validator, virtual time. Python is the source of truth: `python/prj/nova/soc/common/clients/`.

Docs: [[+api/README|+api README]] · [[+api/docs/testing_namespace_manager_client|Namespace Manager testing]]

### `+services/` — MATLAB Workers

Four long-running services using file-based IPC with Python FastAPI bridges:

| Service | MATLAB entry | Python bridge |
|---------|--------------|---------------|
| Slew calc | `+slew_calc/slew_calc_service.m` | `python/prj/nova/soc/matlab_bridges/slew_calc_bridge/` |
| SNR calc | `+snr_calc/snr_calc_service.m` | `python/prj/nova/soc/matlab_bridges/snr_bridge/` |
| TOO planner | `+too_planner/too_planner_service.m` | `python/prj/nova/soc/matlab_bridges/too_planner_bridge/` |
| Alerts filter | `+alerts_filter/alerts_filter_service.m` | `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/` |

Shared IPC: `+common/JsonFileIpc.m`. Diagram: [[+services/diagrams/services_overview.mmd]]

Docs: [[+services/README|+services README]]

### `+alerts_filters/` — Alert Filter Logic

Science-side LVK parsing and filtering. The service wrapper is in `+services/+alerts_filter/`; scientists edit only `+alerts_filters/+lvk/+filters/`.

Docs: [[+alerts_filters/README|+alerts_filters README]]

### `+tools/` — Planning Utilities

`calcSlew`, FOV catalog/corners, extinction, dither grid, AllSS distribution — used by planner and slew service.

### Package root — Mission Science

PSF (`getULTRASAT_PSF`, `weightedPSF`), visibility maps, image simulation (`usim`, `simulateN3`), zodiac background, power limits, scheduling helpers.

---

## Usage

### Environment variables

| Variable | Required for | Purpose |
|----------|--------------|---------|
| `ASTROPACK_DATA_PATH` | Planner, grids, TOO maps | Root for ULTRASAT data files (Windows) |
| `SOC_PATH` | API clients, services | SOC deployment root (`config/services.json`, `runtime/exchange/`) |
| `SOC_API_KEY` | API clients | Sent in `api-key` header by `ClientBase` |

Details: [[docs/run_matlab_cli|Running MATLAB from CLI]]

### Unit tests

```matlab
ultrasat.unitTest              % package root (PSF, usim, merge)
ultrasat.planner.unitTest      % planner core
```

### Debug scripts (headless)

```matlab
debug.ultrasat.planner.debug_Hcs()
debug.ultrasat.planner.debug_TooPlannerRunner()
debug.ultrasat.api.clients.debug_ClientFactory()
debug.ultrasat.services.alerts_filter.debug_processRequest()
```

CLI patterns: [[docs/run_matlab_cli]]

### Launch planner GUI

Open and run `matlab/astro/+ultrasat/+planner/+gui/PlannerMain.mlapp` in App Designer (requires `SOC_PATH`, `SOC_API_KEY`, and usually `ASTROPACK_DATA_PATH`).

---

## Notes

- **Layering:** GUI → `MainModule` → `uplanner` / API clients. GUI never calls planner or API directly. See [[+planner/diagrams/planner_layers.mmd]].
- **IPC:** MATLAB workers and Python communicate via JSON files under `$SOC_PATH/runtime/exchange/<service>/`. See [[diagrams/ipc_flow.mmd]].
- **Cursor rules:** `.cursor/rules/` — domain-specific guidance for IPC, planner, API, services, GUI.
- **Obsidian:** This tree is an Obsidian vault; Mermaid diagrams live in `diagrams/` subfolders.

---

## Known Issues

- `+api/README.md` folder tree was outdated (legacy MissionClient files moved to `obsolete/`).
- SNR service can run as deployed MCR EXE with a separate folder layout (`$SOC_PATH/snr/input/`); see `+services/+snr_calc/README.md`.
- `+planner/+webpage/` is still POC — see ChatGPT link in `WebPageExporter.m`.

---

## See Also

- AstroPack repo root: `CLAUDE.md`
- Python SOC: `python/prj/nova/CLAUDE.md`
- MATLAB bridges overview: `python/prj/nova/soc/matlab_bridges/README.md`
- Integration env conventions: `python/prj/nova/integration/soc_env_conventions.md`
- Cursor rules index: `.cursor/rules/README.md`
