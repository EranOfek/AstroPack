# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/cli.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Command-line interface for the LCS CP-SAT solver
# ***************************************************************************

"""Command-line interface for the LCS CP-SAT solver."""

from __future__ import annotations

import argparse
from pathlib import Path

from .feasibility import compute_feasibility
from .io import (
    default_input_paths,
    load_inputs,
    write_daily_schedule,
    write_schedule_windows,
    write_solver_summary,
    write_validation_report,
)
from .solver import build_and_solve
from .validation import build_solver_summary, validate_schedule


def parse_args(argv: list | None = None) -> argparse.Namespace:
    """
    Parse command-line arguments for a single solver run.

    :param argv: optional argument list (defaults to sys.argv)
    :return: parsed namespace with input/output paths
    """
    base_dir = Path(__file__).resolve().parent.parent
    defaults = default_input_paths(base_dir)

    parser = argparse.ArgumentParser(
        description="ULTRASAT LCS CP-SAT scheduler"
    )
    parser.add_argument(
        "--fields",
        type=Path,
        default=defaults["fields"],
        help="Path to lcs_fields.csv",
    )
    parser.add_argument(
        "--windows",
        type=Path,
        default=defaults["windows"],
        help="Path to lcs_visibility_windows.csv",
    )
    parser.add_argument(
        "--elig",
        type=Path,
        default=defaults["eligibility"],
        help="Path to lcs_field_eligibility.csv",
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=defaults["config"],
        help="Path to lcs_params.json",
    )
    parser.add_argument(
        "--daily-vis",
        type=Path,
        default=defaults["daily_visibility"],
        help="Optional lcs_daily_visibility.csv",
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=base_dir / "output",
        help="Output directory",
    )
    parser.add_argument(
        "--time-limit",
        type=int,
        default=None,
        help="Solver time limit in seconds",
    )
    return parser.parse_args(argv)


def main(argv: list | None = None) -> int:
    """
    Run one LCS CP-SAT solve: load inputs, solve, write outputs.

    :param argv: optional argument list
    :return: 0 on OPTIMAL/FEASIBLE, 1 otherwise
    """
    args = parse_args(argv)
    args.out.mkdir(parents=True, exist_ok=True)

    # Load MATLAB-exported CSV/JSON inputs
    fields_df, windows_df, eligibility_df, config, _daily_df = load_inputs(
        args.fields,
        args.windows,
        args.elig,
        args.config,
        args.daily_vis,
    )
    if args.time_limit is not None:
        config.time_limit_seconds = args.time_limit

    # Precompute feasibility, then build and solve CP-SAT model
    feasibility = compute_feasibility(fields_df, windows_df, eligibility_df, config)
    result = build_and_solve(fields_df, feasibility, config)

    # Write all output artifacts
    write_schedule_windows(result, args.out)
    write_daily_schedule(result, args.out)
    report_df = validate_schedule(result)
    write_validation_report(report_df, args.out)
    summary = build_solver_summary(result, report_df)
    write_solver_summary(summary, args.out)

    print(f"Solver status: {result.status}")
    print(f"Objective: {result.objective_value}")
    print(f"Output written to: {args.out}")
    return 0 if result.status in ("OPTIMAL", "FEASIBLE") else 1


if __name__ == "__main__":
    raise SystemExit(main())
