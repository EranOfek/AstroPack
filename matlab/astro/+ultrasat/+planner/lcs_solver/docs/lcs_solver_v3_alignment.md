# LCS Solver V3 Alignment Guide

This document explains how the Python CP-SAT solver works and how it maps to
`LcsHelper_v3.m`.

The solver is not a line-by-line port of v3. It uses the same input products and
formal scheduling rules, but it lets OR-Tools CP-SAT choose a valid optimized
assignment. Therefore the final field IDs may differ from v3 while still
matching the v3 validator rules.

## Reference Files

MATLAB reference:

- `../LcsHelper_v3.m`
- `../prepareLcsSolverInputs.m`
- `+debug/+ultrasat/+planner/+lcs_v3/validate_LcsHelper_v3.m`
- `+debug/+ultrasat/+planner/+lcs_v3/output/schedule.csv`
- `+debug/+ultrasat/+planner/+lcs_v3/output/full_windows.csv`
- `+debug/+ultrasat/+planner/+lcs_v3/output/daily_schedule.csv`

Python solver:

- `../lcs_cpsat/cli.py`
- `../lcs_cpsat/io.py`
- `../lcs_cpsat/models.py`
- `../lcs_cpsat/feasibility.py`
- `../lcs_cpsat/v3_rules.py`
- `../lcs_cpsat/solver.py`
- `../lcs_cpsat/validation.py`

Generated input bundle:

- `../../data/lcs_solver_inputs/lcs_fields.csv`
- `../../data/lcs_solver_inputs/lcs_params.json`
- `../../data/lcs_solver_inputs/lcs_daily_visibility.csv`
- `../../data/lcs_solver_inputs/lcs_visibility_windows.csv`
- `../../data/lcs_solver_inputs/lcs_visibility_windows_1dgap.csv`
- `../../data/lcs_solver_inputs/lcs_field_eligibility.csv`

## High-Level Flow

The full workflow has four stages.

1. MATLAB computes visibility.
2. MATLAB exports solver inputs.
3. Python converts visibility windows into feasible decision variables.
4. CP-SAT solves Sets A/B/C, then Python places Set D using the v3 slack rule.

The reason astronomy stays in MATLAB is practical: `LcsHelper_v3` already owns
the visibility calculations. Python should not duplicate or approximate those
calculations. Python only consumes the exported daily visibility and continuous
visibility windows.

## Input Export From V3

`prepareLcsSolverInputs.m` constructs `LcsHelper_v3` and runs:

- `calc_vis_matrix()`
- `calc_cont_vis_windows_v2()`

It then writes CSV/JSON files under `data/lcs_solver_inputs`.

The important exported settings are:

```text
start_date = 2029-01-05
num_days = 420
capacity_last_day = 360
min_window_days = 45
max_window_cut_days = 135
daily_lcs_slots = 11
set_a_total = 48
set_b_count = 16
set_c_count = 16
set_d_count = 4
set_c_start_ind = 3
use_window_index_capacity = true
use_set_b_division = true
solve_set_d_separately = true
```

These match the bundled v3 validator run. In particular, `2029-01-05` is the
reference start date used by `validate_LcsHelper_v3.m`.

## Time Model

The visibility horizon is 420 days, but the LCS schedule is 360 days.

The 360-day plan is split into eight fixed 45-day windows:

```text
W1 = day   1 ..  45
W2 = day  46 ..  90
W3 = day  91 .. 135
W4 = day 136 .. 180
W5 = day 181 .. 225
W6 = day 226 .. 270
W7 = day 271 .. 315
W8 = day 316 .. 360
```

With the reference start date, those map to:

```text
W1 = 2029-01-05 .. 2029-02-18
W2 = 2029-02-19 .. 2029-04-04
W3 = 2029-04-05 .. 2029-05-19
W4 = 2029-05-20 .. 2029-07-03
W5 = 2029-07-04 .. 2029-08-17
W6 = 2029-08-18 .. 2029-10-01
W7 = 2029-10-02 .. 2029-11-15
W8 = 2029-11-16 .. 2029-12-30
```

Set C uses two fixed 135-day super-windows. With `set_c_start_ind = 3`:

```text
C group 11 = W3..W5 = day  91 .. 225
C group 12 = W6..W8 = day 226 .. 360
```

## Feasibility Preprocessing

`feasibility.py` translates continuous visibility windows into candidate
assignments.

A field can cover a required interval when some exported visibility window fully
contains that interval:

```text
vis_start_day <= required_start
vis_end_day   >= required_end
```

The solver computes:

- `feasible_a`: field -> feasible 45-day window indices.
- `feasible_a_gs`: field/group/slot feasibility for normal Set A rows.
- `feasible_b`: field -> feasible 45-day window indices.
- `feasible_c`: field -> feasible 135-day super-window indices.
- `feasible_d`: ranked Set D field -> feasible 45-day window indices.
- `slack_45` and `slack_135`: extra visibility margin used in the objective.

The optional 1-day-gap visibility table is selected per field using the
`use1dgap` flag exported from v3 logic.

## Set A

V3 behavior:

- Set A has 48 fields.
- There are 6 normal groups and 8 slots per group.
- Each row occupies a 45-day daily-cadence block.
- V3 may shift one group by up to 30 days during its rescue phase.
- During Set D placement, v3 can move a Set A row to another full 45-day window.
  Moved Set A rows use `group >= 7`.

Python model:

- Normal Set A variables are `a[f,g,s]`.
- Moved Set A variables are `a_moved[f,w]`.
- A field can appear in at most one set.
- The total number of Set A rows is exactly 48:

```text
sum(a) + sum(a_moved) == 48
```

Moved rows are limited by the number of Set D rows:

```text
sum(a_moved) <= set_d_count
```

Normal Set A slots are allowed to be empty:

```text
sum_f a[f,g,s] <= 1
```

This is intentional. It lets the CP model represent the final v3 schedule after
Set D has bumped Set A rows into `group >= 7`.

## Set B

V3 behavior:

- Set B has 16 fields.
- Each Set B field has three 45-day rows:
  - one `B_45` daily-cadence row
  - two `B_90` sparse 4-day-cadence rows
- The three rows come from the v3 division table.
- The table depends on `SetC_start_ind`.
- The schedule encoding is:
  - `B_45`: `group = 100 + full_window_index`
  - `B_90`: `group = 200 + full_window_index`
  - `ind`: cadence counter inside that group

Python model:

- When `use_set_b_division = true`, the solver creates one row-choice variable
  per feasible `(field, division_row)` pair.
- Selecting a row activates exactly one `B_45` window and two `B_90` windows.
- All 16 division rows must be used once.
- Exactly 16 Set B fields are selected.

This is stricter and more v3-like than allowing any arbitrary three feasible
windows.

## Set C

V3 behavior:

- Set C has 16 fields.
- It uses two 135-day super-windows.
- There are 8 fields in group 11 and 8 fields in group 12.
- `ind = 1..8` is the cadence slot within the super-window.

Python model:

- Set C variables are `c[f, sw]`, where `sw` is one of the two v3 super-windows.
- Exactly 16 Set C fields are selected.
- Each valid super-window gets 8 fields.
- A Set C row contributes to the v3 capacity pool in each of the three covered
  45-day windows.

## V3 Window-Index Capacity

The key capacity rule is not ordinary per-calendar-day capacity. V3 first checks
capacity by 45-day window index:

```text
n4(k) = nB90(k) + nC(k)
filled(k) = nA(k) + nB45(k) + n4(k) / 4
filled(k) <= 11
n4(k) must be divisible by 4
```

Where:

- `nA(k)` counts Set A rows at window index `k`.
- `nB45(k)` counts daily Set B rows at window index `k`.
- `nB90(k)` counts sparse Set B rows at window index `k`.
- `nC(k)` counts Set C rows whose 135-day window covers index `k`.

The division by 4 is valid only when the 4-day cadence pool is divisible by 4.
That is why the solver enforces:

```text
4*nA(k) + 4*nB45(k) + n4(k) <= 4*11
n4(k) mod 4 == 0
```

This rule is implemented in `solver.py` by `_add_window_index_capacity()` and
mirrored in `v3_rules.py` by `compute_window_occupancy()`.

## Set D

V3 behavior:

- Set D is placed after Sets A/B/C.
- Candidate fields are tried in rank order:

```text
[79, 12, 48, 28, 16, 88, 55, 32, 213, 26]
```

- Set D uses strict 45-day visibility.
- Set D first tries direct placement in an open window-index slot.
- If no direct slot is available, v3 may bump a non-shifted Set A row from a
  Set D-feasible window into an open window.
- The Set D field takes the old Set A window.
- The moved Set A row becomes `group >= 7`.

Python behavior:

- The CP-SAT model solves A/B/C with moved Set A rows allowed.
- `_place_set_d()` then computes open slots using the v3 occupancy rule.
- It places Set D in rank order.
- It supports both direct placement and Set A bumping.
- It assigns Set D groups as `301..304`, matching v3.

## Cadence Expansion

After assignments are chosen, `build_daily_observations()` expands each window
assignment into day/slot observations.

Daily cadence rows observe every day in their interval:

- Set A
- Set B `B_45`
- Set D

Sparse cadence rows observe every fourth day:

- Set B `B_90`
- Set C

The sparse phase follows the v3 modulo rule:

```text
(day - start_day + 1) mod 4 == ind mod 4
```

This is implemented by `sparse_days_for_ind()` in `v3_rules.py`.

## Outputs

The solver writes both internal and v3-style outputs.

Internal outputs:

- `schedule_windows.csv`
- `daily_observations.csv`

MATLAB-style outputs:

- `schedule.csv`
- `full_windows.csv`
- `daily_schedule.csv` (day x slot matrix; read by the validators)

Diagnostics:

- `validation_report.csv`
- `solver_summary.json`

`schedule.csv` uses the v3 columns:

```text
category, group, ind, start, end, Field, start_date, end_date
```

## Validation

`validation.py` checks the solved result against v3-compatible rules:

- solver status is feasible or optimal
- Set A count is 48
- Set B count is 16 unique fields
- Set C count is 16
- Set D count is at most, and normally equal to, 4
- no field is used in more than one set
- window-index capacity passes
- `n4` divisibility passes
- every assigned field is visible for its assigned interval
- Set B fields have exactly one daily row and two sparse rows

For the bundled v3-aligned input bundle, expected summary is:

```text
status: OPTIMAL
counts:
  A: 48
  B: 16
  C: 16
  D: 4
max_daily_load: 11
validation_passed: 1
```

## Why CP-SAT Can Differ From V3

`LcsHelper_v3` is a constructive scheduler. It builds categories, matches or
places fields, shuffles on failure, and then places Set D.

The CP-SAT solver builds a mathematical model of the same constraints and asks
OR-Tools to optimize. That means:

- field choices may differ
- row order may differ
- Set D may use a different valid window
- objective score can prefer different visibility slack or extinction tradeoffs

The important requirement is that the final schedule satisfies the same formal
rules and passes validation.

## Current Objective

The objective prefers valid schedules with:

- more visibility slack
- lower extinction for normal survey sets
- fixed rank-order Set D placement after ABC

The objective is not intended to reproduce v3's exact greedy choices. To make
the solver row-for-row identical to v3, additional tie-break constraints or a
v3 replay mode would be needed.

## Development Checklist

After changing solver logic:

```powershell
cd C:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+planner\lcs_solver
.venv\Scripts\python.exe -m pytest tests -q
.venv\Scripts\python.exe -m lcs_cpsat.cli --out output_fixed --time-limit 30
```

Then inspect:

```text
output_fixed/solver_summary.json
output_fixed/validation_report.csv
output_fixed/schedule.csv
output_fixed/daily_schedule.csv
```

For a full input refresh:

```matlab
ultrasat.planner.prepareLcsSolverInputs( ...
    'StartDate', datetime(2029,1,5), ...
    'LoadCache', false, ...
    'SaveCache', true);
```

