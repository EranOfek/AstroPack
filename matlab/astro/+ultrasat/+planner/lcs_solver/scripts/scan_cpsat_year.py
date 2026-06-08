#!/usr/bin/env python3
# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/scripts/scan_cpsat_year.py
# Author      : Chen Tishler
# Created     : 08/06/2026
# Updated     : 08/06/2026
# Description : Standalone CP-SAT yearly scan writer
# ***************************************************************************

"""Scan a full calendar year with the CP-SAT LCS solver.

This is a convenience wrapper around lcs_cpsat.scanner.scan_lcs_plans.  It sets
year-based defaults so users can run a full 2029 scan without remembering the
lower-level scan-start/scan-end paths.
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import replace
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from lcs_cpsat.io import load_inputs
from lcs_cpsat.scanner import FEASIBLE_STATUSES, scan_lcs_plans


def _solver_dir() -> Path:
    """Return the lcs_solver root directory."""
    return Path(__file__).resolve().parents[1]


def _planner_dir() -> Path:
    """Return the planner package directory that owns data/lcs_solver_inputs."""
    return Path(__file__).resolve().parents[2]


def _default_input_dir() -> Path:
    """Return the default MATLAB-exported solver input directory."""
    return _planner_dir() / "data" / "lcs_solver_inputs"


def _default_output_dir(year: int) -> Path:
    """Return the default output folder for a yearly CP-SAT scan."""
    return _solver_dir() / "output" / "scans" / str(year)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse yearly scan command-line arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--year", type=int, default=2029)
    parser.add_argument("--scan-start", default=None)
    parser.add_argument("--scan-end", default=None)
    parser.add_argument("--input-dir", type=Path, default=None)
    parser.add_argument("--out", type=Path, default=None)
    parser.add_argument("--time-limit", type=int, default=60)
    parser.add_argument("--max-set-a-shift-days", type=int, default=1)
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Run a CP-SAT scan for a year or explicit date range."""
    args = parse_args(argv)
    input_dir = args.input_dir or _default_input_dir()
    out_dir = args.out or _default_output_dir(args.year)
    # The default scan range is the complete calendar year, but explicit dates
    # are allowed for smoke tests or partial reruns.
    scan_start = args.scan_start or f"{args.year:04d}-01-01"
    scan_end = args.scan_end or f"{args.year:04d}-12-31"

    fields = input_dir / "lcs_fields.csv"
    windows = input_dir / "lcs_visibility_windows.csv"
    windows_1dgap = input_dir / "lcs_visibility_windows_1dgap.csv"
    elig = input_dir / "lcs_field_eligibility.csv"
    config = input_dir / "lcs_params.json"

    missing = [p for p in [fields, windows, windows_1dgap, elig, config] if not p.exists()]
    if missing:
        # The solver cannot derive visibility itself; MATLAB must export these
        # tables first.
        raise FileNotFoundError(
            "Missing CP-SAT input files; prepare solver inputs first: "
            + ", ".join(str(p) for p in missing)
        )

    fields_df, windows_df, eligibility_df, solver_config, _, windows_1dgap_df = load_inputs(
        fields,
        windows,
        elig,
        config,
        windows_1dgap_path=windows_1dgap,
    )
    solver_config = replace(
        solver_config,
        max_set_a_shift_days=args.max_set_a_shift_days,
    )

    # write_full_outputs=True creates per-date success folders comparable to
    # helper scan outputs, which makes later v3/v4/solver comparisons possible.
    index_df = scan_lcs_plans(
        fields_df,
        windows_df,
        eligibility_df,
        solver_config,
        scan_start_date=scan_start,
        scan_end_date=scan_end,
        out_dir=out_dir,
        time_limit_seconds=args.time_limit,
        windows_1dgap_df=windows_1dgap_df,
        write_full_outputs=True,
    )
    feasible = index_df[index_df["status"].isin(FEASIBLE_STATUSES)]
    print(f"CP-SAT year       : {args.year}")
    print(f"Scan range        : {scan_start} .. {scan_end}")
    print(f"Input dir         : {input_dir}")
    print(f"Output dir        : {out_dir}")
    print(f"Scanned dates     : {len(index_df)}")
    print(f"Feasible plans    : {len(feasible)}")
    print(f"Index             : {out_dir / 'lcs_plan_index.csv'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
