#!/usr/bin/env python3
"""Compare the MATLAB and Python v4 validator yearly index files.

The two yearly indexes do not need to match on report-file paths, because each
validator writes its own report filename.  This script compares only the shared
validation result columns and tells you whether the Python validator matches the
MATLAB validator.
"""

from __future__ import annotations

import argparse
import csv
import json
from pathlib import Path
from typing import Any


COMPARE_COLUMNS = ["plan_start_date", "passed", "failed_count", "warning_count", "status", "detail"]
IGNORED_COLUMNS = {"report_file"}


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse MATLAB/Python index paths and the output directory."""
    script_dir = Path(__file__).resolve().parent
    default_root = script_dir / "output" / "scans" / "2029"
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--matlab-index",
        type=Path,
        default=default_root / "matlab_validation_index.csv",
        help="Path to matlab_validation_index.csv.",
    )
    parser.add_argument(
        "--python-index",
        type=Path,
        default=default_root / "python_validation_index.csv",
        help="Path to python_validation_index.csv.",
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=None,
        help="Directory for comparison outputs. Defaults to the MATLAB index folder.",
    )
    return parser.parse_args(argv)


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    """Read a CSV file into a list of dictionaries, preserving header order."""
    if not path.exists():
        raise FileNotFoundError(f"Missing index file: {path}")
    with path.open(newline="", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def normalize_value(value: Any) -> str:
    """Normalize CSV values so row comparison is stable across types."""
    if value is None:
        return ""
    return str(value).strip()


def index_by_date(rows: list[dict[str, str]]) -> dict[str, dict[str, str]]:
    """Key rows by plan_start_date, which is the stable join key."""
    keyed: dict[str, dict[str, str]] = {}
    for row in rows:
        plan_date = normalize_value(row.get("plan_start_date"))
        if not plan_date:
            continue
        if plan_date in keyed:
            raise ValueError(f"Duplicate plan_start_date in index: {plan_date}")
        keyed[plan_date] = row
    return keyed


def compare_rows(m_row: dict[str, str], p_row: dict[str, str]) -> tuple[bool, list[str], dict[str, str]]:
    """Compare the shared result columns for one date."""
    mismatches: list[str] = []
    detail: dict[str, str] = {}
    for col in COMPARE_COLUMNS:
        m_val = normalize_value(m_row.get(col))
        p_val = normalize_value(p_row.get(col))
        detail[f"matlab_{col}"] = m_val
        detail[f"python_{col}"] = p_val
        if m_val != p_val:
            mismatches.append(col)
    detail["mismatch_columns"] = ",".join(mismatches)
    return len(mismatches) == 0, mismatches, detail


def compare_indexes(matlab_rows: list[dict[str, str]], python_rows: list[dict[str, str]]) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    """Compare the two yearly index files and return summary plus per-date rows."""
    m_by_date = index_by_date(matlab_rows)
    p_by_date = index_by_date(python_rows)

    m_dates = set(m_by_date)
    p_dates = set(p_by_date)
    common_dates = sorted(m_dates & p_dates)
    matlab_only = sorted(m_dates - p_dates)
    python_only = sorted(p_dates - m_dates)

    by_date_rows: list[dict[str, Any]] = []
    mismatched_dates: list[str] = []
    for plan_date in common_dates:
        ok, mismatch_cols, detail = compare_rows(m_by_date[plan_date], p_by_date[plan_date])
        if not ok:
            mismatched_dates.append(plan_date)
        by_date_rows.append(
            {
                "plan_start_date": plan_date,
                "match": int(ok),
                **detail,
            }
        )

    summary = {
        "match": int(not matlab_only and not python_only and not mismatched_dates),
        "matlab_index_rows": len(matlab_rows),
        "python_index_rows": len(python_rows),
        "common_dates": len(common_dates),
        "matlab_only_dates": matlab_only,
        "python_only_dates": python_only,
        "mismatched_dates": mismatched_dates,
        "ignored_columns": sorted(IGNORED_COLUMNS),
        "compared_columns": COMPARE_COLUMNS,
    }
    return summary, by_date_rows


def write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    """Write comparison rows to CSV."""
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        path.write_text("", encoding="utf-8")
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def write_report(path: Path, summary: dict[str, Any], rows: list[dict[str, Any]]) -> None:
    """Write a concise human-readable comparison report."""
    lines = [
        "LCS v4 validation index comparison",
        f"match: {bool(summary['match'])}",
        f"matlab_index_rows: {summary['matlab_index_rows']}",
        f"python_index_rows: {summary['python_index_rows']}",
        f"common_dates: {summary['common_dates']}",
        f"matlab_only_dates: {len(summary['matlab_only_dates'])}",
        f"python_only_dates: {len(summary['python_only_dates'])}",
        f"mismatched_dates: {len(summary['mismatched_dates'])}",
        f"compared_columns: {', '.join(summary['compared_columns'])}",
        f"ignored_columns: {', '.join(summary['ignored_columns'])}",
        "",
    ]
    if summary["matlab_only_dates"]:
        lines.append("MATLAB-only dates:")
        lines.extend(f"  {d}" for d in summary["matlab_only_dates"][:50])
        lines.append("")
    if summary["python_only_dates"]:
        lines.append("Python-only dates:")
        lines.extend(f"  {d}" for d in summary["python_only_dates"][:50])
        lines.append("")
    if summary["mismatched_dates"]:
        lines.append("First mismatches:")
        for row in rows:
            if row["match"] == 0:
                lines.append(
                    f"  {row['plan_start_date']}: {row['mismatch_columns']} "
                    f"matlab={ {k: row[k] for k in row if k.startswith('matlab_')} } "
                    f"python={ {k: row[k] for k in row if k.startswith('python_')} }"
                )
        lines.append("")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    """Compare the yearly MATLAB and Python validation indexes."""
    args = parse_args(argv)
    matlab_index = args.matlab_index.resolve()
    python_index = args.python_index.resolve()
    out_dir = (args.out or matlab_index.parent).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    matlab_rows = read_csv_rows(matlab_index)
    python_rows = read_csv_rows(python_index)
    summary, by_date_rows = compare_indexes(matlab_rows, python_rows)

    summary_path = out_dir / "validation_index_comparison.json"
    by_date_path = out_dir / "validation_index_comparison_by_date.csv"
    report_path = out_dir / "validation_index_comparison_report.txt"

    summary_path.write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    write_csv(by_date_path, by_date_rows)
    write_report(report_path, summary, by_date_rows)

    print(f"MATLAB index : {matlab_index}")
    print(f"Python index : {python_index}")
    print(f"Comparison   : {report_path}")
    print(f"By-date CSV  : {by_date_path}")
    print(f"Summary JSON : {summary_path}")
    print(f"Match        : {bool(summary['match'])}")
    print(f"Mismatched   : {len(summary['mismatched_dates'])}")
    return 0 if summary["match"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
