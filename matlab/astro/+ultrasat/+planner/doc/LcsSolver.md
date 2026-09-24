Act as expert Python developer and Google OR-Tools CP-SAT optimization engineer.

Goal:
Create a real Python CP-SAT solver for ULTRASAT Low Cadence Survey scheduling.

Important:
Do NOT reimplement astronomy visibility calculations in Python.
Do NOT convert MATLAB functions PowerLimits, Eclipse_times, or ULTRASAT_restricted_visibility.
Assume visibility is already computed by MATLAB/AstroPack and exported to CSV.

The Python solver receives CSV inputs and produces schedule CSV outputs.

Background:
ULTRASAT LCS has 240 candidate sky fields. Only fields satisfying extinction and visibility constraints can be scheduled.
The survey is divided into 45-day windows.
A 135-day window means 3 consecutive 45-day windows.
Each day has capacity for 11 LCS targets.

Observation groups:

Set A:

* 48 fields
* each field observed for 45 consecutive days
* daily cadence
* uses good-extinction fields, A_U <= 1

Set B:

* 16 fields
* each field receives:

  * one 45-day daily-cadence block
  * two 45-day sparse blocks
* sparse blocks are every 4 days
* ordering is arbitrary; daily block can be first, middle, or last
* total logical coverage is 135 days
* uses good-extinction long-window fields, A_U <= 1

Set C:

* 16 fields
* each field observed in one 135-day window
* cadence every 4 days
* uses good-extinction long-window fields, A_U <= 1

Set D:

* 4 fields
* high-extinction special fields, A_U > 1
* selected from ranked candidate list
* each observed for 45 consecutive days
* daily cadence

Input files:

1. fields.csv
   Required columns:

* field_id
* ra
* dec
* A_U

2. visibility_windows.csv
   Required columns:

* field_id
* vis_start_day
* vis_end_day
* window_length

This file is produced by MATLAB. A field can appear multiple times if it has multiple continuous visibility windows.

3. optional config JSON/YAML
   Parameters:

* first_day default 1
* last_day default 420
* min_window_days default 45
* long_window_days default 135
* daily_capacity default 11
* set_a_count default 48
* set_b_count default 16
* set_c_count default 16
* set_d_count default 4
* max_extinction default 1.0
* d_ranked_fields default [79, 12, 48, 28, 16, 88, 55, 32, 213, 26]
* time_limit_seconds default 300

Core model:
Create 8 main 45-day windows:
W1 = days 1-45
W2 = days 46-90
...
W8 = days 316-360

Allow future extension where window starts may shift, but first implementation can use fixed 45-day windows.

A field is feasible for a 45-day window if one of its visibility windows fully covers that 45-day interval.

A field is feasible for a 135-day window if one of its visibility windows fully covers the 135-day interval, i.e. 3 consecutive 45-day windows.

Decision variables:

* assign_a[field, window45] boolean
* assign_b_daily[field, window45] boolean
* assign_b_sparse[field, window45] boolean
* assign_c[field, start_window_135] boolean
* assign_d[field, window45] boolean

Where start_window_135 can be 1..6, representing:

* W1-W3
* W2-W4
* ...
* W6-W8

Constraints:

1. Each field can be used at most once across A/B/C/D.
2. Exactly 48 A assignments.
3. Exactly 16 B fields.
4. Exactly 16 C assignments.
5. Exactly 4 D assignments.
6. A assignment allowed only if A_U <= max_extinction and field visible for the selected 45-day window.
7. B assignment allowed only if A_U <= max_extinction and field visible for all three selected 45-day blocks.
8. For each B field:

   * exactly one daily 45-day window if selected for B
   * exactly two sparse 45-day windows if selected for B
   * the three B windows must be distinct
   * ordering does not matter
   * optionally require the selected three 45-day windows to form a 135-day span if this is needed by config
9. C assignment allowed only if A_U <= max_extinction and field visible for the selected 135-day window.
10. D assignment allowed only if A_U > max_extinction, field_id is in d_ranked_fields, and field visible for selected 45-day window.
11. Daily capacity:
    For every day, number of scheduled observations <= daily_capacity.
    A and D count every day in their 45-day blocks.
    B daily block counts every day in its 45-day block.
    B sparse blocks count only every 4th day.
    C counts only every 4th day over the 135-day block.
12. For sparse cadence, define phase using window index or configurable phase:
    day is observed if (day - block_start_day) % 4 == phase.
    First version may use phase = window_index % 4, but make this explicit and configurable later.

Objective:
First make feasibility primary.
Then maximize:

* total number of higher-ranked D fields selected
* total visibility slack, where slack is how many extra days the field visibility window has beyond the required observation window
* optionally minimize maximum daily load
* optionally prefer lower A_U for A/B/C

Implement objective as weighted sum:

* very high weight for D rank priority
* medium weight for visibility slack
* small weight for lower extinction

Required outputs:

1. schedule_windows.csv
   Columns:

* category
* field_id
* cadence
* start_day
* end_day
* window_index
* group_id
* notes

2. daily_schedule.csv
   Columns:

* day
* slot_index
* field_id
* category
* cadence

The daily schedule should allocate observations into slot_index 1..11.
For now, slot order can be arbitrary. Later it may be optimized using per-slot visibility.

3. validation_report.csv
   Include checks:

* number of A fields
* number of B fields
* number of C fields
* number of D fields
* duplicate fields
* daily capacity violations
* visibility violations
* cadence violations
* unscheduled required fields
* solver status
* objective value

4. solver_summary.json
   Include:

* status
* wall_time
* objective
* counts per category
* max daily load
* average daily load
* config used

Python structure:
Create package:

lcs_cpsat/
**init**.py
models.py
io.py
feasibility.py
solver.py
validation.py
cli.py
tests/
test_tiny_case.py
test_validation.py

Implementation style:

* Use pandas for CSV I/O.
* Use dataclasses or Pydantic models for config.
* Use OR-Tools CP-SAT: from ortools.sat.python import cp_model
* Keep code readable, with docstrings.
* Do not hide important logic in clever one-liners.
* Add comments explaining each constraint.
* Raise clear errors if input files are missing required columns.
* Write tests using pytest.

CLI:
Implement command:

python -m lcs_cpsat.cli 
--fields fields.csv 
--visibility visibility_windows.csv 
--out output_folder 
--config config.json

Testing:
Create tiny artificial test data:

* 12 fields
* 90 days
* 2 or 3 windows
* reduced counts for A/B/C/D
* verify the solver creates a valid schedule.

Important design instruction:
Do not duplicate Yossi's greedy MATLAB algorithm.
This must be a clean CP-SAT formulation.
The goal is to solve the scheduling problem declaratively using constraints.

Before implementing, first create a short markdown file:
docs/lcs_cpsat_model.md

This document should describe:

* inputs
* variables
* constraints
* objective
* outputs
* known assumptions
* open questions

Open questions to leave clearly marked:

1. Should B's three 45-day blocks be required to be consecutive, or only any three visible 45-day windows?
2. Should sparse cadence phase be fixed by window index, field, or optimized?
3. Should Set A windows be fixed or allowed to shift?
4. Should Set C 135-day windows be fixed or selectable from all consecutive triples?
5. Should daily slot order use per-slot visibility in the first version?
6. Should the solver select A/B/C categories automatically, or use precomputed category candidate lists from MATLAB?

Implement with the current assumptions, but make the code easy to change.
