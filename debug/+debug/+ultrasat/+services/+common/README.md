# JsonFileIpc Debug

Unit debug for the shared file-IPC framework used by all SOC MATLAB workers.

Parent: [[../README|+common README]]

---

## Environment

| Variable | Required |
|----------|----------|
| `SOC_PATH` | Yes — test folders and logs under SOC deployment root |

---

## Script

| Script | Tests |
|--------|-------|
| `debug_JsonFileIpc` | IPC loop — poll, callback, response write, archive |

Exercises `JsonFileIpc` with a temporary input folder and test callback.

---

## How to run

```matlab
debug.ultrasat.services.common.debug_JsonFileIpc()
```

Headless:

```powershell
matlab -batch "debug.ultrasat.services.common.debug_JsonFileIpc()" 2>&1
```

---

## What it validates

- Input file detection (`*.json` mask)
- Callback invocation with decoded struct
- Output file naming (`*.out.json`)
- Processed file archival
- Watchdog file update

For service-level tests, use each service's `+debug/debug_processRequest.m` instead.

---

_Last updated: 2026-06_
