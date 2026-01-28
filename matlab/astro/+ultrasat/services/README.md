# ULTRASAT SOC – MATLAB Workers

This document describes the structure and roles of the Slew Calculation 
subsystem used by ULTRASAT Mission Control.  
It covers the MATLAB worker, the Python API service, and the client 
libraries in both Python and Delphi WebCore.

---

## 1. MATLAB Worker

**File:** `soc_slew_calc_worker_matlab.m`  
**Location:**  
`AstroPack/matlab/astro/+ultrasat/services/slew_service/`

### Role
- Computes ULTRASAT slew time using the official MATLAB implementation.
- Listens for JSON input files under:  
  `$SOC_PATH/slew/input/`
- Writes JSON output files with `.out` suffix.
- Designed to run continuously as a background process:
  - **tmux** for development.
  - **systemd** for production.

---

## 2. Python API (FastAPI)

**File:** `soc_slew_calc_api.py`  
**Location:**  
`Ultrasat.git/python/prj/src/soc/matlab_bridge/slew_bridge/`

### Role
- Provides a REST interface for Mission Control.
- Receives HTTP requests and writes JSON files in the MATLAB worker input folder.
- Waits for MATLAB to produce output, then returns decoded results.
- Supports:
  - **Single requests**
  - **Batch requests** (much faster)

---

## 3. Client Libraries

### Python Mission Control Backend
**File:** `soc/mission/utils/slew_calc_client.py`

#### Role
- Wraps all communication with the Python API.
- Provides:
  - `calc_slew()` for single requests  
  - `calc_slew_batch()` for batch mode  
- Returns clean Python dictionaries and tuples.

---

### Delphi WebCore Frontend
**Unit / Class:** `Mission.SlewCalcClient`

#### Role
- Used by the Mission Control web frontend (Delphi WebCore).
- Sends JSON requests to the Python API.
- Parses JSON responses into Delphi records (`TSlewResult`).
- Supports single and batch operations.

---

## 4. Naming Convention

To make all SOC processes easy to locate and understand:

- MATLAB workers end with:  
  `*_worker_matlab.m`
- FastAPI services end with:  
  `*_api.py`
- Client libraries end with:  
  `*_client.py` or appropriate Delphi class names.

This ensures clarity when searching process lists, logs, and monitoring dashboards.

---

## 5. Summary

The Slew Calculator subsystem includes:

- A **MATLAB worker** that performs the physics-based calculations.
- A **Python FastAPI service** acting as a stable bridge for Mission Control.
- **Python and Delphi clients** that allow easy integration in backend and frontend systems.
- Unified naming and structure to support systemd in production and tmux during development.

This architecture maintains compatibility with existing MATLAB code while providing high-performance access from Mission Control, including batch-mode acceleration.

