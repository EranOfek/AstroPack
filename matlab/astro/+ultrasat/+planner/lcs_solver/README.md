# LCS CP-SAT Solver

Python OR-Tools CP-SAT scheduler for ULTRASAT Low Cadence Survey (LCS).

## Setup

```powershell
cd matlab\astro\+ultrasat\+planner\lcs_solver
.venv\Scripts\pip install -r requirements.txt
```

## Prepare inputs (MATLAB)

```matlab
ultrasat.planner.prepareLcsSolverInputs();
```

Writes CSV/JSON to `../data/lcs_solver_inputs/`.

## Run solver

```powershell
.venv\Scripts\python.exe -m lcs_cpsat.cli --out output --time-limit 300
```

Outputs in `output/`:
- `schedule_windows.csv` — field window assignments (360-day plan)
- `daily_schedule.csv` — day × slot observations (days 1–360)
- `validation_report.csv`
- `solver_summary.json`

## Tests

```powershell
.venv\Scripts\python.exe -m pytest tests/ -v
```

## Plan length

The **LCS plan is 360 days** (8 × 45-day windows). MATLAB visibility inputs cover 420 days as buffer; scheduling and daily capacity apply to days 1–360 only (`capacity_last_day` in `lcs_params.json`).

## Notes

The full problem (48 A + 16 B + 16 C + 4 D at 11 fields/day) may be reported **INFEASIBLE** if daily capacity cannot be satisfied simultaneously. Sub-problems (e.g. A+C) can be feasible. Tune `--time-limit` or adjust config counts for exploration.
