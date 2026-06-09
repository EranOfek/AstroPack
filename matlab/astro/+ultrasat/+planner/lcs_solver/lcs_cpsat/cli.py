# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/cli.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Command-line interface for the LCS CP-SAT solver
# ***************************************************************************

"""Command-line interface for running one LCS CP-SAT solve.

This module is intentionally thin: it parses file paths and options, loads the
MATLAB-exported input bundle, calls the solver, validates the result, and writes
all output artifacts.
"""

from __future__ import annotations

import argparse
from pathlib import Path

from .io import (
    default_input_paths,
    load_inputs,
    write_daily_schedule,
    write_schedule_windows,
    write_solver_summary,
    write_validation_report,
    write_v3_outputs,
)
from .solver import build_and_solve_with_branching
from .validation import build_solver_summary, validate_schedule


def parse_args(argv: list | None = None) -> argparse.Namespace:
    """Parse command-line arguments for a single solver run."""
    base_dir = Path(__file__).resolve().parent.parent
    defaults = default_input_paths(base_dir)

    parser = argparse.ArgumentParser(description="ULTRASAT LCS CP-SAT scheduler")
    parser.add_argument("--fields", type=Path, default=defaults["fields"])
    parser.add_argument("--windows", type=Path, default=defaults["windows"])
    parser.add_argument("--windows-1dgap", type=Path, default=defaults["windows_1dgap"])
    parser.add_argument("--elig", type=Path, default=defaults["eligibility"])
    parser.add_argument("--config", type=Path, default=defaults["config"])
    parser.add_argument("--daily-vis", type=Path, default=defaults["daily_visibility"])
    parser.add_argument("--out", type=Path, default=base_dir / "output")
    parser.add_argument("--time-limit", type=int, default=None)
    return parser.parse_args(argv)


def main(argv: list | None = None) -> int:
    """Run one CP-SAT solve from CLI arguments and write result files."""
    args = parse_args(argv)
    args.out.mkdir(parents=True, exist_ok=True)

    # Loading returns both the tabular inputs and SolverConfig.  The optional
    # daily visibility table is not needed by the current CP-SAT model.
    fields_df, windows_df, eligibility_df, config, _daily_df, windows_1dgap_df = load_inputs(
        args.fields,
        args.windows,
        args.elig,
        args.config,
        args.daily_vis,
        args.windows_1dgap,
    )
    if args.time_limit is not None:
        config.time_limit_seconds = args.time_limit

    # build_and_solve_with_branching tries the v3-compatible Set C anchors and
    # Set A rescue shifts before returning the first feasible schedule.
    result = build_and_solve_with_branching(
        fields_df, windows_df, eligibility_df, config, windows_1dgap_df
    )

    # Write both Python-native and MATLAB/v3-compatible outputs so the same run
    # can be inspected by humans and compared against helper-generated plans.
    write_schedule_windows(result, args.out)
    write_daily_schedule(result, args.out)
    write_v3_outputs(result, args.out)
    report_df = validate_schedule(result)
    write_validation_report(report_df, args.out)
    summary = build_solver_summary(result, report_df)
    write_solver_summary(summary, args.out)

    print(f"Solver status: {result.status}")
    print(f"Objective: {result.objective_value}")
    print(f"SetC_start_ind: {result.config.set_c_start_ind}")
    print(f"Output written to: {args.out}")
    return 0 if result.status in ("OPTIMAL", "FEASIBLE") else 1


if __name__ == "__main__":
    raise SystemExit(main())
