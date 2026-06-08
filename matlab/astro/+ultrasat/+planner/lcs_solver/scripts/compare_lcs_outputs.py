#!/usr/bin/env python3
# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/scripts/compare_lcs_outputs.py
# Author      : Chen Tishler
# Created     : 08/06/2026
# Updated     : 08/06/2026
# Description : Compare helper scan outputs with CP-SAT solver outputs
# ***************************************************************************

"""Compare two LCS scan output folders and write LLM-friendly reports."""

from __future__ import annotations

import argparse
import csv
import json
import re
from pathlib import Path
from typing import Any

DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
FEASIBLE = {"FEASIBLE", "OPTIMAL"}


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("helper_folder", type=Path)
    parser.add_argument("solver_folder", type=Path)
    parser.add_argument("--out", type=Path, default=None)
    parser.add_argument("--skip-details", action="store_true")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    helper = args.helper_folder.resolve()
    solver = args.solver_folder.resolve()
    out_dir = (args.out or helper / "comparison_to_solver").resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    helper_index = read_index(helper)
    solver_index = read_index(solver)
    helper_dates = discover_success_dates(helper, helper_index)
    solver_dates = discover_success_dates(solver, solver_index)

    common_dates = sorted(helper_dates & solver_dates)
    helper_only = sorted(helper_dates - solver_dates)
    solver_only = sorted(solver_dates - helper_dates)

    rows: list[dict[str, Any]] = []
    all_dates = sorted(set(helper_index) | set(solver_index) | helper_dates | solver_dates)
    for plan_date in all_dates:
        h = helper_index.get(plan_date, {})
        s = solver_index.get(plan_date, {})
        row: dict[str, Any] = {
            "plan_start_date": plan_date,
            "helper_status": h.get("status", ""),
            "solver_status": s.get("status", ""),
            "helper_feasible": int(is_feasible(h.get("status", "")) or plan_date in helper_dates),
            "solver_feasible": int(is_feasible(s.get("status", "")) or plan_date in solver_dates),
            "helper_num_observations": int_or_zero(h.get("num_observations", 0)),
            "solver_num_observations": int_or_zero(s.get("num_observations", 0)),
            "fast_relation": fast_relation(plan_date, helper_dates, solver_dates),
            "detail_relation": "",
            "detail": "",
        }
        if not args.skip_details and plan_date in common_dates:
            detail = compare_date_outputs(helper, solver, plan_date)
            row.update(detail)
        rows.append(row)

    summary = {
        "helper_folder": str(helper),
        "solver_folder": str(solver),
        "helper_success_dates": len(helper_dates),
        "solver_success_dates": len(solver_dates),
        "matching_success_dates": len(common_dates),
        "helper_only_success_dates": helper_only,
        "solver_only_success_dates": solver_only,
        "index_dates": len(all_dates),
        "detail_mismatches": sum(1 for r in rows if r.get("detail_relation") not in ("", "match")),
    }

    write_csv(out_dir / "comparison_by_date.csv", rows)
    (out_dir / "comparison_summary.json").write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    write_text_report(out_dir / "comparison_report.txt", summary, rows)

    print(f"Comparison summary : {out_dir / 'comparison_summary.json'}")
    print(f"Comparison by date : {out_dir / 'comparison_by_date.csv'}")
    print(f"Comparison report  : {out_dir / 'comparison_report.txt'}")
    print(f"Matching success dates: {summary['matching_success_dates']}")
    print(f"Detail mismatches     : {summary['detail_mismatches']}")
    return 0


def read_index(folder: Path) -> dict[str, dict[str, str]]:
    path = folder / "lcs_plan_index.csv"
    if not path.exists():
        return {}
    with path.open(newline="", encoding="utf-8-sig") as f:
        return {row.get("plan_start_date", ""): row for row in csv.DictReader(f) if row.get("plan_start_date")}


def discover_success_dates(folder: Path, index: dict[str, dict[str, str]]) -> set[str]:
    dates = {
        date
        for date, row in index.items()
        if is_feasible(row.get("status", "")) or str(row.get("plan_dir", "")).endswith("success")
    }
    children = folder.iterdir() if folder.exists() else []
    for child in children:
        if child.is_dir() and DATE_RE.match(child.name) and (child / "success").is_dir():
            dates.add(child.name)
    return dates


def is_feasible(status: object) -> bool:
    return str(status).upper() in FEASIBLE


def int_or_zero(value: object) -> int:
    try:
        return int(float(str(value)))
    except (TypeError, ValueError):
        return 0


def fast_relation(plan_date: str, helper_dates: set[str], solver_dates: set[str]) -> str:
    if plan_date in helper_dates and plan_date in solver_dates:
        return "match_success_folder"
    if plan_date in helper_dates:
        return "helper_only_success_folder"
    if plan_date in solver_dates:
        return "solver_only_success_folder"
    return "no_success_folder"


def compare_date_outputs(helper: Path, solver: Path, plan_date: str) -> dict[str, str]:
    h_dir = helper / plan_date / "success"
    s_dir = solver / plan_date / "success"
    details: list[str] = []
    relation = "match"

    h_obs = read_observation_rows(find_plan_csv(h_dir, plan_date))
    s_obs = read_observation_rows(find_plan_csv(s_dir, plan_date))
    if h_obs != s_obs:
        relation = "content_mismatch"
        details.append(f"observation_rows helper={len(h_obs)} solver={len(s_obs)} intersection={len(h_obs & s_obs)}")

    h_daily = daily_shape_and_count(h_dir / "daily_schedule.csv")
    s_daily = daily_shape_and_count(s_dir / "daily_schedule.csv")
    if h_daily != s_daily:
        relation = "content_mismatch"
        details.append(f"daily_schedule helper={h_daily} solver={s_daily}")

    h_summary = read_json(h_dir / "summary.json")
    s_summary = read_json(s_dir / "summary.json")
    if h_summary and s_summary:
        for key in ["num_observations", "daily_schedule_rows", "daily_schedule_slots"]:
            if h_summary.get(key) != s_summary.get(key):
                relation = "content_mismatch"
                details.append(f"summary.{key} helper={h_summary.get(key)} solver={s_summary.get(key)}")

    return {
        "detail_relation": relation,
        "detail": "; ".join(details),
    }


def find_plan_csv(plan_dir: Path, plan_date: str) -> Path:
    stamp = plan_date.replace("-", "")
    preferred = plan_dir / f"lcs_plan_{stamp}.csv"
    if preferred.exists():
        return preferred
    matches = sorted(plan_dir.glob("lcs_plan_*.csv"))
    return matches[0] if matches else preferred


def read_observation_rows(path: Path) -> set[tuple[str, str]]:
    if not path.exists():
        return set()
    with path.open(newline="", encoding="utf-8-sig") as f:
        rows = csv.DictReader(f)
        return {
            (row.get("obs_datetime", ""), row.get("field_id", ""))
            for row in rows
        }


def daily_shape_and_count(path: Path) -> dict[str, int]:
    if not path.exists():
        return {"rows": 0, "slots": 0, "observed": 0}
    with path.open(newline="", encoding="utf-8-sig") as f:
        rows = list(csv.DictReader(f))
    slot_cols = [c for c in rows[0].keys() if c.startswith("slot_")] if rows else []
    observed = 0
    for row in rows:
        for col in slot_cols:
            if row.get(col) not in ("", "NaN", "nan"):
                observed += 1
    return {"rows": len(rows), "slots": len(slot_cols), "observed": observed}


def read_json(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {}
    return json.loads(path.read_text(encoding="utf-8"))


def write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        path.write_text("", encoding="utf-8")
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def write_text_report(path: Path, summary: dict[str, Any], rows: list[dict[str, Any]]) -> None:
    lines = [
        "LCS output comparison",
        f"helper_folder: {summary['helper_folder']}",
        f"solver_folder: {summary['solver_folder']}",
        f"helper_success_dates: {summary['helper_success_dates']}",
        f"solver_success_dates: {summary['solver_success_dates']}",
        f"matching_success_dates: {summary['matching_success_dates']}",
        f"helper_only_success_dates: {len(summary['helper_only_success_dates'])}",
        f"solver_only_success_dates: {len(summary['solver_only_success_dates'])}",
        f"detail_mismatches: {summary['detail_mismatches']}",
        "",
        "First mismatches:",
    ]
    mismatches = [
        r for r in rows
        if r["fast_relation"] not in ("match_success_folder", "no_success_folder")
        or r.get("detail_relation") not in ("", "match")
    ]
    for row in mismatches[:50]:
        lines.append(
            f"{row['plan_start_date']}: fast={row['fast_relation']} detail={row.get('detail_relation','')} {row.get('detail','')}"
        )
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


if __name__ == "__main__":
    raise SystemExit(main())
