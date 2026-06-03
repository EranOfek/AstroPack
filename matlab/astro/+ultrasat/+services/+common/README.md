# SOC Services — Common Infrastructure

Shared code for all four MATLAB SOC workers: slew, SNR, TOO planner, alerts filter.

IPC overview: [[../diagrams/services_overview.mmd]] · [[../../diagrams/ipc_flow.mmd]]

---

## Files

| File | Role |
|------|------|
| `JsonFileIpc.m` | Generic JSON file IPC loop |
| `setLogFile.m` | Configure log file for deployed workers |
| `runDeployedBootstrap.m` | Path bootstrap for MCR-deployed executables |
| `+debug/debug_JsonFileIpc.m` | IPC unit debug |

---

## `JsonFileIpc`

Monitors an input folder, processes JSON files via a callback, writes response files, archives processed requests, and updates a watchdog file.

### Constructor properties

| Property | Default | Description |
|----------|---------|-------------|
| `InputPath` | `''` | Folder to poll for incoming files |
| `InputMask` | `'*.json'` | File filter |
| `ProcessedPath` | `''` | Archive folder for processed inputs |
| `KeepProcessedFilesDays` | `7` | Retention for archived files |
| `Callback` | `[]` | Function handle — receives decoded JSON struct |
| `WatchdogFileName` | `[]` | Path to watchdog file (touched every interval) |
| `WatchdogInterval` | `10` | Watchdog update interval [seconds] |
| `MaxRunTime` | `[]` | Optional max runtime for the loop |

### Typical construction (slew service example)

```matlab
jsonIpc = ultrasat.services.common.JsonFileIpc( ...
    'InputPath', InputPath, ...
    'ProcessedPath', ProcessedPath, ...
    'Callback', @ultrasat.services.slew_calc.processRequest, ...
    'WatchdogFileName', WatchdogFileName);
jsonIpc.processLoop();
```

### Request / response contract

| Step | File |
|------|------|
| Input | `request_001.json` |
| Output | `request_001.out.json` (same folder) |
| Archive | `processed/yyyy/MM/dd/request_001.json` |

Rules:

- Never modify request files in place
- Callback must return a struct (written as JSON response)
- Bad input → error response JSON, worker continues

### Loop API

| Method | Role |
|--------|------|
| `processLoop()` | Blocking main loop (used by all services) |
| `tick()` | Process one polling cycle (testing) |
| `processSingleInputFile()` | Process one file by path |

Debug: [[+debug/README|+debug README]]

---

## `setLogFile`

```matlab
ultrasat.services.common.setLogFile('slew_calc_service', 'matlab_services/slew_calc/');
```

Sets log file name and subfolder under `$SOC_PATH/log/`.

---

## `runDeployedBootstrap`

```matlab
ultrasat.services.common.runDeployedBootstrap('relative/path/to/service');
```

Adjusts paths when running as MCR-deployed EXE (no `addpath()` in deployed mode).

---

## Used by

| Service | Input folder | Callback |
|---------|--------------|----------|
| [[../+slew_calc/README\|slew_calc]] | `$SOC_PATH/runtime/exchange/slew_calc/input/` | `@processRequest` |
| [[../+too_planner/README\|too_planner]] | `$SOC_PATH/runtime/exchange/too_planner/input/` | `@processRequest` |
| [[../+alerts_filter/README\|alerts_filter]] | `$SOC_PATH/runtime/exchange/alerts_filter/input/` | `@processRequest` |
| [[../+snr_calc/README\|snr_calc]] | `$SOC_PATH/snr/input/` (legacy layout) | custom loop in `snr_calc_service.m` |

SNR service predates the unified exchange layout but uses the same JSON-in / JSON-out pattern.

---

_Last updated: 2026-06_
