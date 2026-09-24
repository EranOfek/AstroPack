# LCS CP-SAT Tests Guide

## General Idea

These tests protect the Python CP-SAT solver from rule drift and accidental behavior changes. They are small on purpose: most tests use synthetic data so failures are easy to understand and do not require the full mission input bundle.

The tests cover three levels:

- Pure rule arithmetic, independent of OR-Tools.
- Validation behavior on hand-built solver results.
- A tiny end-to-end CP-SAT solve.
- Scanner date/index behavior.

Run them from the planner directory with:

```powershell
.\lcs_solver\.venv\Scripts\python.exe -m pytest lcs_solver\tests -q
```

Or from inside `lcs_solver`:

```powershell
.\.venv\Scripts\python.exe -m pytest tests -q
```

## File Guide

### `test_v3_rules.py`

Tests the Python translation of MATLAB v3 scheduling arithmetic.

It checks:

- Set A shifted-group calendar anchors.
- Set B division-table mirroring when `SetC_start_ind` changes.
- Sparse 4-day cadence day selection.
- The v3 `filled(k)` occupancy formula and `n4` divisibility rule.

If this file fails, the problem is usually in `lcs_cpsat/v3_rules.py` or in a changed interpretation of MATLAB v3 rules.

### `test_validation.py`

Tests `lcs_cpsat/validation.py` without running CP-SAT.

The tests build small `SolverResult` objects manually. This makes it easy to verify that validation catches:

- Duplicate field use across sets.
- Daily capacity violations.
- Correctly formed schedules.

If this file fails, inspect validation logic before investigating the solver model.

### `test_tiny_case.py`

Runs a small artificial CP-SAT solve.

The test creates:

- 12 synthetic fields.
- Full-horizon visibility for all fields.
- A small config with reduced Set A/B/D counts.

It then runs:

1. `compute_feasibility`
2. `build_and_solve`
3. `validate_schedule`

This is the main smoke test that CP-SAT model construction, solving, extraction, and validation still work together.

### `test_scanner.py`

Tests scanner behavior.

It checks:

- Calendar date to campaign-day conversion.
- Absolute observation timestamp construction.
- `scan_lcs_plans` writes `lcs_plan_index.csv` with one row per scanned date.

The scanner test reuses the tiny artificial inputs from `test_tiny_case.py` so it stays fast.

### `__init__.py`

Marks the folder as a test package. This allows tests to import helpers from each other, such as `_make_tiny_inputs`.

## How To Read Failures

Start with the failing file:

- `test_v3_rules.py`: likely a pure rule/math issue.
- `test_validation.py`: likely a report/checking issue.
- `test_tiny_case.py`: likely model construction, feasibility, solve, extraction, or validation integration.
- `test_scanner.py`: likely date conversion, scan indexing, or scan output behavior.

For CP-SAT failures, first check the solver status. `OPTIMAL` and `FEASIBLE` are both acceptable in these tests. `INFEASIBLE` usually means the tiny input/config no longer matches the constraints.

## Test Design Notes

The tests intentionally avoid full 2029 data. Full-year scans are useful for performance and operational validation, but they are too large for fast unit tests.

The tiny case does not prove that every real mission plan is feasible. It proves that the solver pipeline can create a legal schedule when given a simple feasible problem.
