# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/scripts/scan_2029_cpsat.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Updated     : 07/06/2026
# Description : Standalone CP-SAT 2029 scan and V3 comparison writer
# ***************************************************************************

"""Scan all 2029 start dates with CP-SAT and compare with saved V3 results."""

from __future__ import annotations

import argparse
import sys
from dataclasses import replace
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from lcs_cpsat.io import load_inputs
from lcs_cpsat.scanner import FEASIBLE_STATUSES, scan_lcs_plans


def _repo_planner_dir() -> Path:
    return Path(__file__).resolve().parents[2]


def _default_scan_root() -> Path:
    return _repo_planner_dir() / "+debug" / "lcs_2029_scan"


def _is_feasible(status: object) -> bool:
    return str(status).upper() in FEASIBLE_STATUSES


def compare_indices(v3_index_path: Path, cpsat_index_path: Path, out_path: Path) -> pd.DataFrame:
    v3 = pd.read_csv(v3_index_path, dtype={"plan_start_date": str})
    cpsat = pd.read_csv(cpsat_index_path, dtype={"plan_start_date": str})

    dates = sorted(set(v3["plan_start_date"]).union(set(cpsat["plan_start_date"])))
    rows = []
    for plan_date in dates:
        v3_row = v3[v3["plan_start_date"] == plan_date]
        cp_row = cpsat[cpsat["plan_start_date"] == plan_date]

        v3_status = "" if v3_row.empty else str(v3_row.iloc[0]["status"])
        cp_status = "" if cp_row.empty else str(cp_row.iloc[0]["status"])
        v3_feasible = _is_feasible(v3_status)
        cp_feasible = _is_feasible(cp_status)
        v3_obs = 0 if v3_row.empty else int(v3_row.iloc[0].get("num_observations", 0))
        cp_obs = 0 if cp_row.empty else int(cp_row.iloc[0].get("num_observations", 0))
        compatible = v3_feasible == cp_feasible
        if compatible and v3_feasible:
            compatible = v3_obs == cp_obs

        if v3_feasible and cp_feasible:
            relation = "match_feasible" if compatible else "both_feasible_count_mismatch"
        elif not v3_feasible and not cp_feasible:
            relation = "match_infeasible"
        elif v3_feasible:
            relation = "v3_only"
        else:
            relation = "cpsat_only"

        rows.append(
            {
                "plan_start_date": plan_date,
                "v3_status": v3_status,
                "cpsat_status": cp_status,
                "v3_feasible": int(v3_feasible),
                "cpsat_feasible": int(cp_feasible),
                "v3_num_observations": v3_obs,
                "cpsat_num_observations": cp_obs,
                "compatible": int(compatible),
                "relation": relation,
                "v3_detail": "" if v3_row.empty else str(v3_row.iloc[0].get("detail", "")),
                "cpsat_detail": "" if cp_row.empty else str(cp_row.iloc[0].get("detail", "")),
            }
        )

    out_path.parent.mkdir(parents=True, exist_ok=True)
    df = pd.DataFrame(rows)
    df.to_csv(out_path, index=False)
    return df


def parse_args() -> argparse.Namespace:
    scan_root = _default_scan_root()
    input_dir = scan_root / "cpsat_inputs"
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scan-start", default="2029-01-01")
    parser.add_argument("--scan-end", default="2029-12-31")
    parser.add_argument("--input-dir", type=Path, default=input_dir)
    parser.add_argument("--out", type=Path, default=scan_root / "cpsat")
    parser.add_argument("--v3-index", type=Path, default=scan_root / "v3" / "lcs_plan_index.csv")
    parser.add_argument("--comparison", type=Path, default=scan_root / "v3_vs_cpsat_comparison.csv")
    parser.add_argument("--time-limit", type=int, default=30)
    parser.add_argument("--max-set-a-shift-days", type=int, default=1)
    parser.add_argument("--no-compare", action="store_true")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    fields = args.input_dir / "lcs_fields.csv"
    windows = args.input_dir / "lcs_visibility_windows.csv"
    windows_1dgap = args.input_dir / "lcs_visibility_windows_1dgap.csv"
    elig = args.input_dir / "lcs_field_eligibility.csv"
    config = args.input_dir / "lcs_params.json"

    missing = [p for p in [fields, windows, windows_1dgap, elig, config] if not p.exists()]
    if missing:
        raise FileNotFoundError(
            "Missing CP-SAT input files. Prepare a broad visibility export first: "
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

    index_df = scan_lcs_plans(
        fields_df,
        windows_df,
        eligibility_df,
        solver_config,
        scan_start_date=args.scan_start,
        scan_end_date=args.scan_end,
        out_dir=args.out,
        time_limit_seconds=args.time_limit,
        windows_1dgap_df=windows_1dgap_df,
        write_full_outputs=True,
    )
    feasible = index_df[index_df["status"].isin(FEASIBLE_STATUSES)]
    print(f"CP-SAT scanned {len(index_df)} start dates")
    print(f"CP-SAT feasible plans: {len(feasible)}")
    print(f"CP-SAT index: {args.out / 'lcs_plan_index.csv'}")

    if not args.no_compare:
        if not args.v3_index.exists():
            raise FileNotFoundError(f"V3 index not found: {args.v3_index}")
        comparison = compare_indices(
            args.v3_index,
            args.out / "lcs_plan_index.csv",
            args.comparison,
        )
        mismatches = comparison[comparison["compatible"] == 0]
        print(f"Comparison: {args.comparison}")
        print(f"Mismatches: {len(mismatches)}")
        if not mismatches.empty:
            print(mismatches["relation"].value_counts().to_string())

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
