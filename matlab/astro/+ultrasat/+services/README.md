# ULTRASAT SOC — MATLAB Workers

Long-running MATLAB processes that serve the ULTRASAT Science Operations Center via **file-based IPC** with Python FastAPI bridges. MATLAB and Python never talk over sockets directly in this pattern.

Diagram: [[diagrams/services_overview.mmd]] · IPC sequence: [[../diagrams/ipc_flow.mmd]]

---

## Architecture

```
Python client  →  FastAPI bridge  →  JSON file  →  MATLAB service  →  .out.json  →  bridge  →  client
```

Exchange folders live under:

```
$SOC_PATH/runtime/exchange/<service_name>/
├── input/
├── processed/
└── <service>.watchdog
```

Shared infrastructure: `+common/JsonFileIpc.m` — polls `input/`, calls a callback, writes `*.out.json`, archives to `processed/yyyy/MM/dd/`.

---

## Services

| Service | MATLAB entry | Dispatcher | Python bridge |
|---------|--------------|------------|---------------|
| Slew calc | `+slew_calc/slew_calc_service.m` | `processRequest.m` | `python/prj/nova/soc/matlab_bridges/slew_calc_bridge/` |
| SNR calc | `+snr_calc/snr_calc_service.m` | `processSnrJson.m` | `python/prj/nova/soc/matlab_bridges/snr_bridge/` |
| TOO planner | `+too_planner/too_planner_service.m` | `processRequest.m` | `python/prj/nova/soc/matlab_bridges/too_planner_bridge/` |
| Alerts filter | `+alerts_filter/alerts_filter_service.m` | `processRequest.m` | `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/` |

Bridge overview: `python/prj/nova/soc/matlab_bridges/README.md`

---

## Common (`+common/`)

| File | Role |
|------|------|
| `JsonFileIpc.m` | Generic file-IPC loop (poll, callback, response, archive, watchdog) |
| `setLogFile.m` | Set log file path for deployed workers |
| `runDeployedBootstrap.m` | Path bootstrap for MCR-deployed executables |
| `+debug/debug_JsonFileIpc.m` | IPC unit debug |

---

## Service details

### Slew calculator — [[+slew_calc/README|+slew_calc]]

Actions: `health`, `slew`, `slew_batch`, `power_limits`. Wraps `ultrasat.tools.calcSlew` and `PowerLimits`.

### SNR calculator — [[+snr_calc/README|+snr_calc]]

SNR and limiting magnitude for web SNR calculator. Can run as source (`snr_calc_service.m`) or deployed MCR EXE. Legacy folder layout: `$SOC_PATH/snr/input/`.

### TOO planner — [[+too_planner/README|+too_planner]]

Runs `TooPlannerRunner` from JSON config; generates plan artifacts and `summary.json`. Comprehensive docs in that README.

### Alerts filter — [[+alerts_filter/README|+alerts_filter]]

Action `filter_lvk`: loads LVK alert from file, dispatches to `+alerts_filters/+lvk/+filters/`. Science logic stays in `+alerts_filters/`.

---

## Environment

| Variable | Purpose |
|----------|---------|
| `SOC_PATH` | Runtime exchange folders, logs, config |
| `ASTROPACK_DATA_PATH` | Required for TOO planner (grids, probability maps) |
| `ASTROPACK_CONFIG_PATH` | Required for deployed SNR/slew MCR builds |

---

## Running services

### Development (MATLAB source)

```matlab
ultrasat.services.slew_calc.slew_calc_service()
ultrasat.services.snr_calc.snr_calc_service()
ultrasat.services.too_planner.too_planner_service()
ultrasat.services.alerts_filter.alerts_filter_service()
```

### Production

systemd unit files and deploy scripts:

- `+slew_calc/scripts/README.md`
- `+too_planner/scripts/README.md`
- `python/prj/nova/deploy/socsrv/systemd/`

---

## Design rules

- Workers are **thin loops** — file I/O separate from computation
- Never crash on bad input — write error response JSON
- No GUI code in services
- Core algorithms must be callable without the file system (for unit tests)

Cursor rules: `.cursor/rules/01_matlab_file_based_ipc_with_python.mdc`, `06_matlab_services_workers_and_long_running_processes.mdc`

---

## Python cross-references

| Topic | Path |
|-------|------|
| Bridge architecture | `python/prj/nova/soc/matlab_bridges/README.md` |
| Slew models | `python/prj/nova/soc/common/models/matlab_bridges/api/slew_calc.py` |
| TOO models | `python/prj/nova/soc/common/models/matlab_bridges/api/too_planner.py` |
| Alerts filter | `python/prj/nova/soc/matlab_bridges/alerts_filter_bridge/README.md` |
| Deploy | `python/prj/nova/deploy/socsrv/README.md` |

---

_Last updated: 2026-06_
