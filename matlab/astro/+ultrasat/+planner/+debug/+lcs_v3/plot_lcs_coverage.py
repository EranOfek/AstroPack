#!/usr/bin/env python3
"""Plot LCS scan coverage from date-named output folders."""

from __future__ import annotations

import argparse
import re
from datetime import date, datetime
from pathlib import Path

import matplotlib.dates as mdates
import matplotlib.pyplot as plt

DATE_DIR_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")


def discover_scanned_dates(scan_dir: Path) -> list[date]:
    if not scan_dir.is_dir():
        return []
    dates: list[date] = []
    for child in scan_dir.iterdir():
        if child.is_dir() and DATE_DIR_RE.match(child.name):
            try:
                dates.append(date.fromisoformat(child.name))
            except ValueError:
                pass
    return sorted(dates)


def plot_coverage(scanned_dates: list[date], output_path: Path | None, show: bool) -> None:
    if scanned_dates:
        year = scanned_dates[0].year
    else:
        year = datetime.now().year
    year_start = date(year, 1, 1)
    year_end = date(year, 12, 31)
    days_in_year = (year_end - year_start).days + 1

    fig, ax = plt.subplots(figsize=(14, 4))
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

    ax.set_xlim(datetime(year, 1, 1), datetime(year, 12, 31))
    ax.set_ylim(0.0, 1.05)
    ax.set_yticks([0.0, 1.0])
    ax.set_yticklabels(["", "scanned"])
    ax.set_xlabel("Date")
    ax.set_title(f"LCS {year} scan coverage\n{len(scanned_dates)} / {days_in_year} dates", fontsize=12)
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


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--folder",
        type=Path,
        default=Path.cwd(),
        help="Folder containing YYYY-MM-DD scan subfolders; default is current working directory.",
    )
    parser.add_argument("--output", type=Path, default=None)
    parser.add_argument("--no-show", action="store_true")
    parser.add_argument("--no-save", action="store_true")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    scan_dir = args.folder.resolve()
    output_path = None if args.no_save else (args.output or scan_dir / "lcs_coverage.png").resolve()
    scanned_dates = discover_scanned_dates(scan_dir)
    plot_coverage(scanned_dates, output_path=output_path, show=not args.no_show)
    print(f"Scan dir : {scan_dir}")
    print(f"Scanned  : {len(scanned_dates)}")
    if scanned_dates:
        print(f"First    : {scanned_dates[0].isoformat()}")
        print(f"Last     : {scanned_dates[-1].isoformat()}")
    if output_path is not None:
        print(f"Saved    : {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
