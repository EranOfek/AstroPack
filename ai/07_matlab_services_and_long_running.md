# 07 — Services and Long-Running Processes

## Nature

Services are long-running MATLAB processes, not scripts. They run continuously, polling for work.

Location: `matlab/apps/services/`

## Active Services

- `snr_service` — Signal-to-noise ratio computation
- `slew_service` — Slew time computation
- `too_service` — Target of Opportunity planning
- `incoming_alerts_filter` — Incoming alert filtering

## Resilience Rules

- Must survive bad inputs without crashing
- Must recover from individual request failures and continue processing
- Errors in one request must not affect subsequent requests
- Defensive input validation at service boundaries
- Detailed logging of all failures

## Design Constraints

- No interactive prompts or GUI dependencies
- State is per-request; no accumulated state across requests
- File-based IPC for communication with Python (see 04_matlab_data_flow_and_ipc.md)
