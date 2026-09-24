#!/usr/bin/env python3
"""Run the v4 Python CSV validator over every feasible scan output folder.

The per-folder validator is validate_LcsHelper_v4.py.  This batch wrapper keeps
the command reusable: it finds the yearly scan index, validates each
yyyy-mm-dd/success folder, lets the per-folder validator write its report, and
writes a yearly python_validation_index.csv.
"""

from __future__ import annotations

import argparse
import csv
import subprocess
import sys
from pathlib import Path


FEASIBLE_STATUSES = {"FEASIBLE", "OPTIMAL"}


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse scan-folder options for a reusable yearly validation command."""
    script_dir = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--year", type=int, default=2029)
    parser.add_argument("--scan-dir", type=Path, default=None)
    parser.add_argument("--validator", type=Path, default=script_dir / "validate_LcsHelper_v4.py")
    parser.add_argument("--stop-on-failure", action="store_true")
    return parser.parse_args(argv)


def default_scan_dir(year: int) -> Path:
    """Return +debug/+lcs_v4/output/scans/<year> relative to this script."""
    return Path(__file__).resolve().parent / "output" / "scans" / str(year)


def read_scan_index(scan_dir: Path) -> list[dict[str, str]]:
    """Read lcs_plan_index.csv from the scan root."""
    index_path = scan_dir / "lcs_plan_index.csv"
    if not index_path.exists():
        raise FileNotFoundError(f"Missing scan index: {index_path}")
    with index_path.open(newline="", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def feasible_rows(index_rows: list[dict[str, str]]) -> list[dict[str, str]]:
    """Return rows whose status should have a yyyy-mm-dd/success folder."""
    return [
        row for row in index_rows
        if row.get("status", "").upper() in FEASIBLE_STATUSES
    ]


def run_one_validator(validator: Path, output_dir: Path) -> subprocess.CompletedProcess[str]:
    """Run validate_LcsHelper_v4.py for one output directory."""
    return subprocess.run(
        [sys.executable, str(validator), "--output-dir", str(output_dir)],
        text=True,
        capture_output=True,
    )


def parse_report_header(report_path: Path) -> tuple[str, str, str]:
    """Read checks passed/failed/warnings from a per-folder report."""
    passed = ""
    failed = ""
    warnings = ""
    if not report_path.exists():
        return passed, failed, warnings
    for line in report_path.read_text(encoding="utf-8").splitlines()[:8]:
        if line.startswith("checks passed:"):
            passed = line.split(":", 1)[1].strip()
        elif line.startswith("checks failed:"):
            failed = line.split(":", 1)[1].strip()
        elif line.startswith("warnings:"):
            warnings = line.split(":", 1)[1].strip()
    return passed, failed, warnings


def first_fail_detail(report_path: Path, proc: subprocess.CompletedProcess[str]) -> str:
    """Return the first FAIL line, or stderr if the report was not written."""
    if report_path.exists():
        for line in report_path.read_text(encoding="utf-8").splitlines():
            if line.startswith("[FAIL]"):
                return line
    return proc.stderr.strip().replace("\n", " | ")[:500]


def write_index(scan_dir: Path, rows: list[dict[str, object]]) -> Path:
    """Write the yearly Python validation summary CSV."""
    out_path = scan_dir / "python_validation_index.csv"
    fieldnames = [
        "plan_start_date",
        "passed",
        "failed_count",
        "warning_count",
        "status",
        "report_file",
        "detail",
    ]
    with out_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    return out_path


def main(argv: list[str] | None = None) -> int:
    """Validate all feasible scan folders and write a yearly summary index."""
    args = parse_args(argv)
    scan_dir = (args.scan_dir or default_scan_dir(args.year)).resolve()
    validator = args.validator.resolve()

    print(f"Python v4 scan validator")
    print(f"Scan dir : {scan_dir}")
    print(f"Validator: {validator}")

    index_rows = read_scan_index(scan_dir)
    rows = []
    failures = 0

    selected = feasible_rows(index_rows)
    for idx, row in enumerate(selected, start=1):
        plan_date = row["plan_start_date"]
        output_dir = scan_dir / plan_date / "success"
        print(f"[{idx:3d}/{len(selected):3d}] {plan_date}")

        proc = run_one_validator(validator, output_dir)
        report_path = output_dir / "validate_LcsHelper_v4_py_report.txt"
        checks_passed, checks_failed, warnings = parse_report_header(report_path)
        ok = proc.returncode == 0
        if not ok:
            failures += 1

        rows.append(
            {
                "plan_start_date": plan_date,
                "passed": int(ok),
                "failed_count": checks_failed,
                "warning_count": warnings,
                "status": row.get("status", ""),
                "report_file": str(report_path),
                "detail": "ok" if ok else first_fail_detail(report_path, proc),
            }
        )

        if not ok and args.stop_on_failure:
            break

    out_path = write_index(scan_dir, rows)
    print(f"Python validation index: {out_path}")
    print(f"Folders validated: {len(rows)}")
    print(f"Passed: {sum(int(row['passed']) for row in rows)}")
    print(f"Failed: {failures}")
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
