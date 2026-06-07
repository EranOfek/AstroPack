# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/scan_cli.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : CLI for scanning LCS plan start dates over a date range
# ***************************************************************************

"""CLI for scanning LCS plan start dates over a date range."""

from __future__ import annotations

import argparse
from pathlib import Path

from .io import default_input_paths, load_inputs
from .scanner import scan_lcs_plans


def parse_args(argv: list | None = None) -> argparse.Namespace:
    """
    Parse command-line arguments for the plan date scanner.

    :param argv: optional argument list (defaults to sys.argv)
    :return: parsed namespace with scan range and paths
    """
    base_dir = Path(__file__).resolve().parent.parent
    defaults = default_input_paths(base_dir)

    parser = argparse.ArgumentParser(
        description="Scan LCS plan start dates and write feasible plan CSVs"
    )
    parser.add_argument(
        "--scan-start",
        required=True,
        help="First candidate plan start date (ISO, e.g. 2029-01-01)",
    )
    parser.add_argument(
        "--scan-end",
        required=True,
        help="Last candidate plan start date (ISO, e.g. 2029-03-02)",
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
        "--out",
        type=Path,
        default=base_dir / "output" / "scan",
        help="Output directory for index and per-plan CSV files",
    )
    parser.add_argument(
        "--time-limit",
        type=int,
        default=60,
        help="CP-SAT time limit per candidate start date (seconds)",
    )
    return parser.parse_args(argv)


def main(argv: list | None = None) -> int:
    """
    Load inputs and scan all candidate plan start dates in the given range.

    :param argv: optional argument list
    :return: 0 on success
    """
    args = parse_args(argv)

    fields_df, windows_df, eligibility_df, config, _ = load_inputs(
        args.fields,
        args.windows,
        args.elig,
        args.config,
    )

    index_df = scan_lcs_plans(
        fields_df,
        windows_df,
        eligibility_df,
        config,
        scan_start_date=args.scan_start,
        scan_end_date=args.scan_end,
        out_dir=args.out,
        time_limit_seconds=args.time_limit,
    )

    feasible = index_df[index_df["status"].isin(["OPTIMAL", "FEASIBLE"])]
    print(f"Scanned {len(index_df)} start dates")
    print(f"Feasible plans: {len(feasible)}")
    print(f"Index written to: {args.out / 'lcs_plan_index.csv'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
