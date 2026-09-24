# SNR Calculator Service

MATLAB backend for the ULTRASAT web SNR calculator. Computes signal-to-noise ratio and limiting magnitude for a given observational configuration using `UltrasatPerf2GUI`.

Python bridge: `python/prj/nova/soc/matlab_bridges/snr_bridge/`

---

## Overview

The SNR service receives JSON-encoded requests (via file IPC), runs the performance calculator, and returns JSON-encoded responses. It can run as:

1. **MATLAB source** — `snr_calc_service.m` (development)
2. **Deployed MCR EXE** — built with `_build.bat` (production)

---

## Main files

| File | Role |
|------|------|
| `snr_calc_service.m` | Service entry — `mainLoop()`, IPC setup |
| `processRequest.m` | Thin wrapper calling `processSnrJson` |
| `processSnrJson.m` | Decode JSON, call `UltrasatPerf2GUI`, encode response |
| `watchdog_monitor.py` | External watchdog for deployed process |
| `_build.bat` | Build MCR standalone EXE |
| `_run.bat` / `_run.sh` | Run deployed or source service |
| `examples/` | Sample request JSON files |

---

## Request flow

```
Python bridge  →  $SOC_PATH/snr/input/*.json  →  snr_calc_service  →  *.out.json  →  bridge
```

In source mode, `mainLoop()` uses:

- Input: `$SOC_PATH/snr/input/`
- Output: same folder with `.out` suffix

Deployed mode uses `FileMap` for path resolution (MCR cannot use `addpath()`).

---

## Input JSON fields

Typical request fields (see `processSnrJson.m` header):

- `ExpTime`, `NumImages`, `R`
- `Source`, `PicklesModels`
- `SnrMagnitude`, `LimitingMagnitude`
- `CalibFilterFamily`, `CalibFilter`, `MagnitudeSystem`

---

## Output envelope

```json
{
  "message": "...",
  "result": 0,
  "json_result": "{ ... escaped inner JSON ... }"
}
```

`result`: `0` = success, negative = error.

---

## Environment

| Variable | Purpose |
|----------|---------|
| `SOC_PATH` | Input/output folders, FileMap storage |
| `ASTROPACK_CONFIG_PATH` | Required for deployed MCR build |

Deployed FileMap: `$SOC_PATH/snr/snr_matlab/AstroPackFileMap_1.mat`

---

## Build and deploy

MATLAB R2023a+ required for MCR build.

```batch
_build.bat    REM from +snr_calc folder
_run.bat      REM Windows
_run.sh       REM Linux
```

Notes from source header:

- MCC generates a self-extracting ZIP with all required files
- `snakeyaml-1.9.jar` must be in the EXE folder
- `UltrasatPerf('Init', false)` is called at startup to force linker inclusion

Backup of older deployed layout: `backup-deployed-2023/`

---

## Debug

Run from MATLAB with `SOC_PATH` set:

```matlab
ultrasat.services.snr_calc.snr_calc_service()
```

---

## Python cross-references

| Topic | Path |
|-------|------|
| SNR bridge README | `python/prj/nova/soc/matlab_bridges/snr_bridge/README.md` |
| Bridge API | `python/prj/nova/soc/matlab_bridges/snr_bridge/api.py` |
| SNR models | `python/prj/nova/soc/common/models/matlab_bridges/api/snr.py` |

---

_Last updated: 2026-06_
