# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/scan_cli.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : CLI for scanning LCS plan start dates over a date range
# ***************************************************************************

"""CLI for scanning LCS plan start dates over a date range.

This entry point is a package-level scanner.  The standalone script in
scripts/scan_cpsat_year.py wraps the same scanner with year-oriented defaults.
"""

from __future__ import annotations

import argparse
from pathlib import Path

from .io import default_input_paths, load_inputs
from .scanner import scan_lcs_plans


def parse_args(argv: list | None = None) -> argparse.Namespace:
    """Parse command-line arguments for a date-range scan."""
    base_dir = Path(__file__).resolve().parent.parent
    defaults = default_input_paths(base_dir)

    parser = argparse.ArgumentParser(
        description="Scan LCS plan start dates and write feasible plan CSVs"
    )
    parser.add_argument("--scan-start", required=True)
    parser.add_argument("--scan-end", required=True)
    parser.add_argument("--fields", type=Path, default=defaults["fields"])
    parser.add_argument("--windows", type=Path, default=defaults["windows"])
    parser.add_argument("--windows-1dgap", type=Path, default=defaults["windows_1dgap"])
    parser.add_argument("--elig", type=Path, default=defaults["eligibility"])
    parser.add_argument("--config", type=Path, default=defaults["config"])
    parser.add_argument("--out", type=Path, default=base_dir / "output" / "scan")
    parser.add_argument("--time-limit", type=int, default=60)
    parser.add_argument(
        "--write-full-outputs",
        action="store_true",
        help="write full per-plan output folders for feasible plans",
    )
    return parser.parse_args(argv)


def main(argv: list | None = None) -> int:
    """Load inputs, run the date scan, print a short summary, and exit."""
    args = parse_args(argv)

    # The scanner recomputes per-date eligibility, but it still needs the broad
    # input tables exported by MATLAB as its source of visibility truth.
    fields_df, windows_df, eligibility_df, config, _, windows_1dgap_df = load_inputs(
        args.fields,
        args.windows,
        args.elig,
        args.config,
        windows_1dgap_path=args.windows_1dgap,
    )

    # scan_lcs_plans writes lcs_plan_index.csv and optional per-date outputs.
    index_df = scan_lcs_plans(
        fields_df,
        windows_df,
        eligibility_df,
        config,
        scan_start_date=args.scan_start,
        scan_end_date=args.scan_end,
        out_dir=args.out,
        time_limit_seconds=args.time_limit,
        windows_1dgap_df=windows_1dgap_df,
        write_full_outputs=args.write_full_outputs,
    )

    feasible = index_df[index_df["status"].isin(["OPTIMAL", "FEASIBLE"])]
    print(f"Scanned {len(index_df)} start dates")
    print(f"Feasible plans: {len(feasible)}")
    print(f"Index written to: {args.out / 'lcs_plan_index.csv'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
