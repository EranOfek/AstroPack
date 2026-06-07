# LCS CP-SAT Solver

Python OR-Tools CP-SAT scheduler for ULTRASAT Low Cadence Survey (LCS).

## Setup

```powershell
cd matlab\astro\+ultrasat\+planner\lcs_solver
.venv\Scripts\pip install -r requirements.txt
```

## Prepare inputs (MATLAB)

Regenerate visibility inputs anchored to your scan range start (example: 2029-01-01):

```matlab
ultrasat.planner.prepareLcsSolverInputs( ...
    'StartDate', datetime('2029-01-01'), ...
    'SaveCache', false);
```

Writes CSV/JSON to `../data/lcs_solver_inputs/`.

## Run solver (single start day)

```powershell
.venv\Scripts\python.exe -m lcs_cpsat.cli --out output --time-limit 300
```

Outputs in `output/`:
- `schedule_windows.csv` — field window assignments (360-day plan)
- `daily_schedule.csv` — day × slot observations (days 1–360)
- `validation_report.csv`
- `solver_summary.json`

## Scan plan start dates

Try every candidate start date in a range (daily step) and write one plan CSV per feasible result:

```powershell
.venv\Scripts\python.exe -m lcs_cpsat.scan_cli `
  --scan-start 2029-01-01 `
  --scan-end   2029-03-02 `
  --time-limit 60 `
  --out        output/scan/
```

Outputs in `output/scan/`:
- `lcs_plan_index.csv` — all scanned start dates with status and plan file name
- `lcs_plan_YYYYMMDD.csv` — one file per feasible plan with columns:
  - `obs_datetime` — ISO UTC observation time
  - `field_id` — target index from `lcs_fields.csv`

Valid scan range with 420-day visibility buffer: plan start day `k` must satisfy `k + 359 <= 420`.
If MATLAB `start_date` is 2029-01-01, valid starts are roughly **2029-01-01 .. 2029-03-02** (~61 days).

## Tests

```powershell
.venv\Scripts\python.exe -m pytest tests/ -v
```

## Plan length

The **LCS plan is 360 days** (8 × 45-day windows). MATLAB visibility inputs cover 420 days as buffer; scheduling and daily capacity apply to days 1–360 only (`capacity_last_day` in `lcs_params.json`).

## Notes

The full problem (48 A + 16 B + 16 C + 4 D at 11 fields/day) may be reported **INFEASIBLE** if daily capacity cannot be satisfied simultaneously. Sub-problems (e.g. A+C) can be feasible. Tune `--time-limit` or adjust config counts for exploration.
