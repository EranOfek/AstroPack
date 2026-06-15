# LCS CP-SAT Solver

Python OR-Tools CP-SAT scheduler for the ULTRASAT Low Cadence Survey (LCS).

The solver uses visibility and field data exported from `LcsHelper_v3.m` and
builds an optimization model that follows the same scheduling rules validated by
`+debug/validate_LcsHelper_v3.m`.

For the detailed model explanation, read:

- `docs/lcs_solver_v3_alignment.md` - full solver design and how it maps to v3.
- `docs/lcs_cpsat_model.md` - short pointer to the current design document.

## Directory Layout

```text
lcs_solver/
  lcs_cpsat/
    cli.py              command-line entry point
    io.py               CSV/JSON input and output
    models.py           dataclasses and solver configuration
    feasibility.py      visibility-to-feasible-window preprocessing
    v3_rules.py         v3 window/cadence/capacity helper rules
    solver.py           CP-SAT model and Set D placement
    validation.py       post-solve validation report
    scan_cli.py         command-line date scanner
    scanner.py          repeated start-date solves
  docs/
    lcs_solver_v3_alignment.md
  tests/
```

## Setup

```powershell
cd C:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+planner\lcs_solver
.venv\Scripts\pip install -r requirements.txt
```

## Prepare Inputs From MATLAB

Generate solver inputs from the same v3 visibility pipeline:

```matlab
ultrasat.planner.prepareLcsSolverInputs( ...
    'StartDate', datetime(2029,1,5), ...
    'LoadCache', false, ...
    'SaveCache', true);
```

This writes the input bundle to:

```text
../data/lcs_solver_inputs/
```

Important defaults:

- `StartDate = 2029-01-05`, matching `validate_LcsHelper_v3.m`.
- Visibility horizon is 420 days.
- Scheduled LCS plan is 360 days, equal to 8 fixed 45-day windows.
- Set B uses the v3 division table (`use_set_b_division = true`).

## Run One Plan

```powershell
.venv\Scripts\python.exe -m lcs_cpsat.cli --out output_fixed --time-limit 30
```

Main outputs:

- `schedule.csv` - schedule table with `category, group, ind, start, end, Field`.
- `full_windows.csv` - the eight 45-day windows with calendar dates.
- `daily_schedule.csv` - day x slot matrix (`day, date, slot_1 .. slot_11`). This is
  the canonical matrix consumed by `validate_LcsHelper_v4.py` and `compare_lcs_outputs.py`.
- `schedule_windows.csv` - normalized internal assignment table.
- `daily_observations.csv` - normalized one-row-per-observation debug view.
- `validation_report.csv` - pass/fail checks for the schedule rules.
- `solver_summary.json` - status, timing, counts, and validation summary.

Expected fixed-result shape for the bundled inputs:

```text
status: OPTIMAL
A: 48
B: 16
C: 16
D: 4
max_daily_load: 11
validation_passed: 1
```

The exact field choices can differ from `LcsHelper_v3` because CP-SAT optimizes
over all valid choices rather than replaying the v3 greedy/matching sequence.
The required outcome is rule-equivalence: same counts, window geometry, capacity
model, cadence rules, and validation success.

## Scan Start Dates

Try every candidate start date in a range:

```powershell
.venv\Scripts\python.exe -m lcs_cpsat.scan_cli `
  --scan-start 2029-01-05 `
  --scan-end   2029-03-05 `
  --time-limit 60 `
  --out        output\scan
```

The scanner writes:

- `lcs_plan_index.csv` - status per candidate start date.
- `lcs_plan_YYYYMMDD.csv` - one feasible plan per start date.

The visibility export contains a 420-day buffer, while a plan consumes 360 days.
For a scan, the chosen start offset must still leave enough visibility horizon
for day 360.

## Tests

```powershell
.venv\Scripts\python.exe -m pytest tests -q
```

## Validation Reference

The MATLAB reference validator is:

```matlab
ultrasat.planner.debug.validate_LcsHelper_v3()
```

The Python validation mirrors the same major checks:

- pipeline counts for Sets A/B/C/D
- no duplicate field use across sets
- v3 window-index capacity: `filled(k) = nA(k) + nB45(k) + (nB90(k)+nC(k))/4`
- divisibility of the 4-day cadence pool
- visibility coverage for every assigned field/window
- Set B 1x daily + 2x sparse cadence structure

