# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/+debug/plot_lcs_2029_v3_coverage.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Updated     : 07/06/2026
# Description : Plot LCS 2029 V3 scan date coverage from folder presence
# ***************************************************************************

"""Plot which 2029 dates have V3 scan output folders under lcs_2029_scan/v3."""

from __future__ import annotations

import argparse
import re
from datetime import date, datetime
from pathlib import Path

import matplotlib.dates as mdates
import matplotlib.pyplot as plt

DATE_DIR_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
YEAR_START = date(2029, 1, 1)
YEAR_END = date(2029, 12, 31)
DAYS_IN_YEAR = (YEAR_END - YEAR_START).days + 1


def _default_scan_dir() -> Path:
    return Path(__file__).resolve().parent / "lcs_2029_scan" / "v3"


def _default_output_path() -> Path:
    return Path(__file__).resolve().parent / "lcs_2029_v3_coverage.png"


def discover_scanned_dates(scan_dir: Path) -> list[date]:
    """Return sorted dates for immediate child folders named YYYY-MM-DD."""
    if not scan_dir.is_dir():
        return []

    dates: list[date] = []
    for child in scan_dir.iterdir():
        if not child.is_dir() or not DATE_DIR_RE.match(child.name):
            continue
        try:
            dates.append(date.fromisoformat(child.name))
        except ValueError:
            continue
    return sorted(dates)


def plot_coverage(scanned_dates: list[date], output_path: Path | None, show: bool) -> None:
    """Draw a full-2029 timeline with bars on dates that have scan folders."""
    fig, ax = plt.subplots(figsize=(14, 4))

    year_start_dt = datetime(YEAR_START.year, YEAR_START.month, YEAR_START.day)
    year_end_dt = datetime(YEAR_END.year, YEAR_END.month, YEAR_END.day)

    if scanned_dates:
        bar_x = mdates.date2num([datetime(d.year, d.month, d.day) for d in scanned_dates])
        ax.bar(
            bar_x,
            height=1.0,
            width=0.9,
            align="center",
            bottom=0.0,
            color="#1f77b4",
            edgecolor="#0d4a7a",
            linewidth=0.3,
            zorder=2,
        )

    ax.set_xlim(year_start_dt, year_end_dt)
    ax.set_ylim(0.0, 1.05)
    ax.set_yticks([0.0, 1.0])
    ax.set_yticklabels(["", "scanned"])
    ax.set_xlabel("Date")
    ax.set_title(
        "LCS 2029 V3 scan coverage\n"
        f"{len(scanned_dates)} / {DAYS_IN_YEAR} dates",
        fontsize=12,
    )

    ax.xaxis.set_major_locator(mdates.MonthLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
    ax.xaxis.set_minor_locator(mdates.DayLocator(interval=7))
    ax.grid(True, axis="x", which="major", linestyle="--", alpha=0.4)
    ax.grid(True, axis="y", which="major", linestyle=":", alpha=0.3)
    fig.autofmt_xdate()
    fig.tight_layout()

    if output_path is not None:
        output_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(output_path, dpi=150, bbox_inches="tight")

    if show:
        plt.show()
    else:
        plt.close(fig)


def print_summary(scan_dir: Path, scanned_dates: list[date], output_path: Path | None) -> None:
    """Print scan coverage stats to the console."""
    print(f"Scan dir : {scan_dir}")
    print(f"Scanned  : {len(scanned_dates)} / {DAYS_IN_YEAR} dates")

    if not scan_dir.is_dir():
        print("Warning  : scan directory does not exist")
        return

    if not scanned_dates:
        print("Warning  : no date folders found")
        return

    print(f"First    : {scanned_dates[0].isoformat()}")
    print(f"Last     : {scanned_dates[-1].isoformat()}")

    if output_path is not None:
        print(f"Saved    : {output_path}")


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Plot LCS 2029 V3 scan coverage from date-named output folders"
    )
    parser.add_argument(
        "--scan-dir",
        type=Path,
        default=_default_scan_dir(),
        help="Root directory containing YYYY-MM-DD subfolders (default: lcs_2029_scan/v3)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=_default_output_path(),
        help="PNG output path (default: lcs_2029_v3_coverage.png beside this script)",
    )
    parser.add_argument("--no-show", action="store_true", help="Do not open an interactive plot window")
    parser.add_argument("--no-save", action="store_true", help="Do not write a PNG file")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    scan_dir = args.scan_dir.resolve()
    output_path = None if args.no_save else args.output.resolve()
    scanned_dates = discover_scanned_dates(scan_dir)

    plot_coverage(scanned_dates, output_path=output_path, show=not args.no_show)
    print_summary(scan_dir, scanned_dates, output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
