# LcsHelper v4 Validators

## General

The v4 validation folder contains two validators:

- `validate_LcsHelper_v4.m` validates a live `ultrasat.planner.LcsHelper_v4` MATLAB object.
- `validate_LcsHelper_v4.py` validates CSV files already written by the MATLAB validator or by another generator that emits the same CSV schema.

Both validators are meant to reject false plans. A plan should fail if it has invalid timing, wrong set composition, duplicate fields, illegal slot occupancy, or a daily schedule that does not match the schedule rows. The Python validator is useful for iterative solver comparisons because it can be run directly on a folder of CSV outputs without calling MATLAB.

Warnings are separate from hard failures. Warnings report ranking quality issues that should be reviewed but are not currently used as reject conditions.

## Detailed Operation

### MATLAB validator build and outputs

`validate_LcsHelper_v4.m` constructs `LcsHelper_v4`, runs the v4 build flow, validates the resulting object, and writes CSV output under:

`+debug/+lcs_v4/output/validation/`

The main CSV files are:

- `schedule.csv`
- `full_windows.csv`
- `daily_schedule.csv`
- set-specific debug tables when available

### Python validator inputs and outputs

`validate_LcsHelper_v4.py` reads a CSV output folder. By default it reads:

`+debug/+lcs_v4/output/validation/`

It accepts `--output-dir <folder>` to validate another folder, such as a solver output folder. It writes:

`validate_LcsHelper_v4_py_report.txt`

in the same folder and exits with a nonzero status when any hard check fails.

The Python validator requires the same CSV schema:

- `schedule.csv`: `category, group, ind, start, end, Field`
- `full_windows.csv`: `start, end`
- `daily_schedule.csv`: `day, slot_1, ..., slot_11`

### Set A hard checks

Set A must contain exactly 48 placed fields. Every Set A row must have a 45-day window. Original groups 1 through 6 may each contain at most 8 fields.

Moved Set A rows are checked explicitly. A moved row must align to the start and end day of its assigned full-window index, and moved `(group, ind)` slots must be unique. Group accounting includes moved groups so shifted fields cannot disappear from the group-count checks.

### Set B hard checks

Set B must contain 16 unique fields and 48 rows total. Each field must have one `B_45` row and two `B_90` rows. Every row must be 45 days long and must align to a full-window boundary.

The validator checks that each Set B field spans exactly three full-window indices over 135 days and that the group encoding matches the expected `B_45` and `B_90` conventions.

### Set C hard checks

Set C must contain exactly 16 unique fields. Every row must be 135 days long, start on a full-window boundary, and use a valid v4 block group. In v4, Set C groups are the block groups `11..16`, and each row must have a local cadence index in `1..8`.

### Set D hard checks

Set D may contain at most 4 fields. If present, Set D fields must be unique, must use 45-day windows, and must use group slots `301..304` without duplicates.

### Slot budget hard checks

Both validators recompute occupancy from `schedule.csv` or the MATLAB `Schedule` table:

- `nA`: Set A rows assigned to each full-window index
- `nB45`: Set B 45-day rows assigned to each index
- `nB90`: Set B cadence rows assigned to each index
- `nC`: Set C rows covering each index
- `nD`: Set D rows assigned to each index
- `nCadence4 = nB90 + nC`
- `filledABC = nA + nB45 + nCadence4 / 4`
- `filled+D = filledABC + nD`

Hard checks require `nCadence4` to be divisible by 4, `filledABC <= 11`, and `filled+D <= 11`. The final `filled+D` check is important because Set D is added after A/B/C balancing and must still fit in the 11-slot daily budget.

### Window bounds hard checks

All placed rows must lie inside the planning horizon. In MATLAB this is checked against `First_day` and `Last_day`. In Python this is checked against the day range present in `daily_schedule.csv`.

### Duplicate hard checks

Field IDs must not overlap across logical sets. The validators check A/B, A/C, A/D, B/C, B/D, and C/D overlaps.

### Daily schedule hard checks

The daily schedule must contain rows, exactly 11 slot columns, and at least one observation.

The validators reconstruct the expected field multiset for every day from the schedule rows. Normal rows appear every day in their `[start, end]` interval. `B_90` and `C` rows appear only on the 4-day cadence:

`mod(curr_day - start + 1, 4) == mod(ind, 4)`

For every day, the reconstructed multiset is compared to the multiset in the daily schedule slots. This catches missing observations, extra observations, duplicate daily fields, incorrect cadence, wrong dates, and plans where `daily_schedule.csv` no longer matches `schedule.csv`.

### Python-specific hard checks

The Python validator also checks required CSV columns, valid category names, and integer-like numeric fields. Malformed CSV data therefore fails validation instead of being silently accepted.

### Warning checks

The warning checks do not reject the plan:

- Long-field extinction ranking: MATLAB can inspect object-side field tables and warn about Set B, Set C, and long Set A ranking relationships. Python cannot fully reconstruct this from CSV output, so it reports that the check is not available from CSV.
- Set D ranking: MATLAB uses `SetD_ranked_fields`; Python uses the default rank list embedded in the script. The warning checks that selected Set D fields appear in rank order and that earlier ranked fields were not unexpectedly skipped.
