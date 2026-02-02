# 04 — Data Flow and IPC

## MATLAB–Python File-Based IPC

Communication between MATLAB workers and Python services uses file-based IPC, not sockets or REST.

### Pattern

1. Python writes a JSON or MAT request file to a watched folder
2. MATLAB worker polls the folder, detects the new file
3. MATLAB processes the request, writes a response file
4. Python reads the response file

### Services Using This Pattern

- `snr_service` — Signal-to-noise ratio calculation
- `slew_service` — Slew time calculation
- `too_service` — Target of Opportunity planning
- `incoming_alerts_filter` — Alert filtering

## ULTRASAT Planner Data Flow

```
GUI (user interaction)
  ↕
DataModule (state, validation, serialization)
  ↕
uplanner (core logic, deterministic computation)
  ↕
API client (backend communication)
```

DataModule owns application state and mediates all data movement. GUI never calls API or planner core directly.

## Image Processing Pipeline Flow

```
Raw FITS files
  → @AstroImage (load + header parsing)
  → +imProc (calibration, source finding, photometry, astrometry)
  → +pipeline (orchestration of processing steps)
  → Database / output files
```
