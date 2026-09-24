# LCS CP-SAT Package Guide

## General Idea

This package is a Python OR-Tools CP-SAT scheduler for the ULTRASAT Low Cadence Survey. It is aligned with the MATLAB `LcsHelper_v3` scheduling rules.

CP-SAT is a constraint solver. In this code, the solver asks:

1. Which field-window assignments should be selected?
2. Do those selections satisfy all hard LCS rules?
3. Among legal schedules, which one has the best objective score?

The important CP-SAT concepts in this package are:

- Boolean variables: a variable is `1` when a candidate assignment is selected and `0` otherwise.
- Constraints: equations or inequalities that every valid solution must satisfy.
- Objective: a score used to rank valid solutions. It does not make an invalid plan valid.
- Status: `OPTIMAL` means the best solution was proven; `FEASIBLE` means a legal solution was found before proof of optimality; `INFEASIBLE` means no legal solution was found for that branch.

## Data Flow

The solver starts from MATLAB-exported input files under `data/lcs_solver_inputs`:

- `lcs_fields.csv`: field catalog and extinction values.
- `lcs_visibility_windows.csv`: strict visibility windows.
- `lcs_visibility_windows_1dgap.csv`: visibility windows after the one-day-gap merge.
- `lcs_field_eligibility.csv`: field eligibility flags.
- `lcs_params.json`: solver and campaign constants.

The package flow is:

1. `io.py` loads CSV/JSON inputs and normalizes column names.
2. `feasibility.py` converts visibility windows into feasible field/window maps.
3. `solver.py` creates CP-SAT Boolean variables only for feasible choices.
4. `solver.py` adds hard constraints for Sets A, B, C, field uniqueness, capacity, and sparse-cadence divisibility.
5. OR-Tools solves the A/B/C model.
6. `solver.py` extracts selected variables into `WindowAssignment` rows.
7. `solver.py` places Set D into open slack to mirror the MATLAB v3 post-processing step.
8. `solver.py` expands window assignments into per-day `DailyObservation` rows.
9. `validation.py` re-checks the result.
10. `io.py` writes normalized solver CSVs and MATLAB/v3-compatible CSVs.

For yearly scans, `scanner.py` repeats the same solve for many candidate start dates and writes an `lcs_plan_index.csv` plus per-date outputs.

## Sets A/B/C/D in Plain Language

Set A uses 48 daily fields arranged as 6 groups x 8 slots. Each slot is a 45-day window. The solver creates variables like "field 33 is in group 2, slot 5" only when that field is visible for that calendar interval.

Set B uses 16 fields. Each Set B field has one daily 45-day row (`B_45`) and two sparse 45-day rows (`B_90`). In division-table mode, the allowed B window triples come from the v3 helper table.

Set C uses 16 fields in 135-day windows. These are sparse every 4 days, so they share capacity using the same cadence pool as `B_90`.

Set D is placed after A/B/C. It uses up to 4 high-priority fields from `d_ranked_fields`, trying to fit them into open window-index slack. If needed, it may move a Set A row into an open compatible window, matching the v3 rescue behavior.

The core capacity rule is:

```text
filled(k) = nA(k) + nB45(k) + (nB90(k) + nC(k)) / 4
```

The solver enforces this as integer math:

```text
4*nA(k) + 4*nB45(k) + nB90(k) + nC(k) <= 4*daily_capacity
```

It also enforces that `nB90(k) + nC(k)` is divisible by 4, because sparse rows must interleave cleanly into 4-day cadence phases.

## How To Read The Code

Recommended reading order:

1. `models.py`
2. `io.py`
3. `feasibility.py`
4. `v3_rules.py`
5. `solver.py`
6. `validation.py`
7. `cli.py`
8. `scanner.py`
9. `scan_cli.py`
10. `../scripts/scan_cpsat_year.py`
11. `../scripts/compare_lcs_outputs.py`

Start with the data shapes, then learn how visibility becomes feasibility, then read the CP-SAT model.

## Per-File Guide

### `models.py`

Defines the package data structures:

- `SolverConfig`: constants and solver options.
- `FeasibilityMaps`: precomputed feasible choices.
- `WindowAssignment`: compact selected schedule row.
- `DailyObservation`: expanded per-day observation row.
- `SolverResult`: complete solve result.

Read this first so the function signatures in other files are easier to follow.

### `io.py`

Owns input loading and output writing. It normalizes MATLAB column names, validates required columns, loads `SolverConfig`, and writes both solver-native outputs and v3-compatible CSV outputs.

The important idea is that solver internals use normalized rows, while comparison and validator tools often need MATLAB-style `schedule.csv`, `full_windows.csv`, and daily schedule files.

### `feasibility.py`

Turns visibility windows into feasible assignment maps. This is where impossible field/window choices are removed before CP-SAT model construction.

This file answers questions like:

- Can field 18 cover 45-day window 3?
- Can field 54 cover a 135-day Set C super-window?
- Is this Set A field feasible for this exact group/slot calendar interval?

Pruning here keeps the CP-SAT model smaller and easier to solve.

### `v3_rules.py`

Contains Python versions of the MATLAB v3 scheduling arithmetic:

- Set B division table.
- Set A group anchors and shifted-group rescue geometry.
- Set C super-window anchors.
- Sparse 4-day cadence rule.
- `filled(k)` occupancy and open-slot computation.

This file is shared by solver and validation, so both use the same rule math.

### `solver.py`

Builds and solves the CP-SAT model.

Important sections:

- `_build_abc_model`: creates Boolean variables and hard constraints for Sets A/B/C.
- `_add_window_index_capacity`: enforces the v3 capacity formula.
- objective construction: gives small rewards for slack/extinction and large penalties for overflow.
- `build_and_solve_with_branching`: tries Set C anchor branches and Set A shift branches.
- `_extract_solution`: converts selected Boolean variables into `WindowAssignment` rows.
- `_place_set_d`: post-processes Set D into open slack.
- `build_daily_observations`: expands compact windows into per-day observations.

When learning CP-SAT, focus first on `model.NewBoolVar(...)`, `model.Add(...)`, and `model.Maximize(...)`.

### `validation.py`

Runs post-solve checks. This is separate from CP-SAT constraints on purpose: validation catches mistakes in model construction, solution extraction, or future edits.

It checks counts, duplicate fields, window-index capacity, sparse divisibility, visibility, and Set B cadence structure.

### `cli.py`

Command-line entry point for one solve. It loads inputs, calls `build_and_solve_with_branching`, writes outputs, validates, writes summaries, and returns success only for feasible solver statuses.

### `scanner.py`

Scans a date range. It converts each candidate calendar start into campaign-day coordinates, recomputes per-date eligibility, runs the solver, and writes an index plus optional full per-date output folders.

### `scan_cli.py`

Package-level CLI for date-range scans. It is useful when you want explicit `--scan-start` and `--scan-end` arguments.

### `../scripts/scan_cpsat_year.py`

Standalone yearly scan wrapper. It defaults to year `2029`, uses the default MATLAB-exported input directory, and writes full per-date output folders suitable for comparison with v3/v4 helper scans.

### `../scripts/compare_lcs_outputs.py`

Compares helper scan outputs to solver scan outputs. It first compares feasible date folders/index rows, then optionally compares detailed per-date content. It writes CSV, JSON, and text reports designed to be easy for humans and LLMs to inspect during fixing iterations.

## Typical Commands

Run one solve:

```powershell
cd C:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+planner\lcs_solver
.venv\Scripts\python.exe -m lcs_cpsat.cli --out output\single --time-limit 30
```

Run a date-range scan:

```powershell
.venv\Scripts\python.exe -m lcs_cpsat.scan_cli --scan-start 2029-01-01 --scan-end 2029-01-31 --out output\scan_jan
```

Run a full-year scan:

```powershell
.venv\Scripts\python.exe scripts\scan_cpsat_year.py --year 2029
```

Compare helper and solver outputs:

```powershell
.venv\Scripts\python.exe scripts\compare_lcs_outputs.py <helper_scan_folder> <solver_scan_folder>
```
