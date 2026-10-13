#!/usr/bin/env python3
"""Summarize insertArchiveCatalogs2DB_perf visits from insertDB_perf.log (aligned stdout).

Usage:
  python3 parse_insertdb_perf_log.py ~/matlab/log/last/insertArchive/insertDB_perf.log
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

TS_PREFIX = re.compile(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3} ")
RE_CATALOG_ASTRO = re.compile(
    r"(?P<fn>\S+) catalogAstroPerf nCat=(?P<ncat>\d+) nRows=(?P<nrows>\d+) "
    r"catSec=[\d.]+ rowsPerSec=[\d.]+ (?P<datadir>\S+)$"
)
RE_VISIT_END = re.compile(
    r"(?P<fn>\S+) visitEnd (?P<outcome>\S+) (?P<datadir>\S+) (?P<sec>[\d.]+)s$"
)


@dataclass
class VisitRecord:
    datadir: str
    n_cat: int
    n_rows: int
    visit_sec: float | None = None
    order: int = 0
    missing_visit_end: bool = field(default=False)


def strip_timestamp(line: str) -> str:
    return TS_PREFIX.sub("", line.rstrip("\n\r"))


def parse_log(path: Path, fn_filter: str) -> list[VisitRecord]:
    by_dir: dict[str, VisitRecord] = {}
    order = 0

    text = path.read_text(encoding="utf-8", errors="replace")
    for line in text.splitlines():
        body = strip_timestamp(line)
        m = RE_CATALOG_ASTRO.match(body)
        if m and m.group("fn") == fn_filter:
            datadir = m.group("datadir")
            rec = VisitRecord(
                datadir=datadir,
                n_cat=int(m.group("ncat")),
                n_rows=int(m.group("nrows")),
                order=order,
            )
            order += 1
            by_dir[datadir] = rec
            continue

        m = RE_VISIT_END.match(body)
        if m and m.group("fn") == fn_filter:
            datadir = m.group("datadir")
            if datadir in by_dir:
                by_dir[datadir].visit_sec = float(m.group("sec"))

    rows = sorted(by_dir.values(), key=lambda r: r.order)
    for rec in rows:
        if rec.visit_sec is None:
            rec.missing_visit_end = True
    return rows


def visit_sec_str(rec: VisitRecord) -> str:
    if rec.visit_sec is None:
        return ""
    return f"{rec.visit_sec:.1f}"


def format_row(
    datadir: str,
    n_cat: str,
    n_rows: str,
    visit: str,
    w_datadir: int,
    w_ncat: int,
    w_nrows: int,
    w_visit: int,
) -> str:
    return (
        f"{datadir:<{w_datadir}},"
        f"{n_cat:>{w_ncat}},"
        f"{n_rows:>{w_nrows}},"
        f"{visit:>{w_visit}}"
    )


def format_table(rows: list[VisitRecord], header: bool) -> list[str]:
    if not rows:
        return []

    visit_strs = [visit_sec_str(r) for r in rows]
    w_datadir = max(len("DataDir"), max(len(r.datadir) for r in rows))
    w_ncat = max(len("nCat"), max(len(str(r.n_cat)) for r in rows))
    w_nrows = max(len("nRows"), max(len(str(r.n_rows)) for r in rows))
    w_visit = max(len("visitSec"), max((len(s) for s in visit_strs), default=0))

    out: list[str] = []
    if header:
        out.append(
            format_row("DataDir", "nCat", "nRows", "visitSec", w_datadir, w_ncat, w_nrows, w_visit)
        )
    for rec, vstr in zip(rows, visit_strs):
        out.append(
            format_row(
                rec.datadir,
                str(rec.n_cat),
                str(rec.n_rows),
                vstr,
                w_datadir,
                w_ncat,
                w_nrows,
                w_visit,
            )
        )
    return out


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Print one aligned line per catalogAstroPerf visit from insertDB_perf.log."
    )
    parser.add_argument(
        "logfile",
        type=Path,
        help="Path to insertDB_perf.log (or a copy)",
    )
    parser.add_argument(
        "--fn",
        default="insertArchiveCatalogs2DB_perf",
        help="Clone name filter (default: insertArchiveCatalogs2DB_perf)",
    )
    parser.add_argument(
        "--no-header",
        action="store_true",
        help="Omit the CSV header row (default: header included)",
    )
    args = parser.parse_args()

    if not args.logfile.is_file():
        print(f"error: log file not found: {args.logfile}", file=sys.stderr)
        return 1

    rows = parse_log(args.logfile, args.fn)
    if not rows:
        print(
            f"error: no catalogAstroPerf lines for --fn {args.fn!r} in {args.logfile}",
            file=sys.stderr,
        )
        return 1

    for rec in rows:
        if rec.missing_visit_end:
            print(
                f"warning: no visitEnd for {rec.datadir} ({args.logfile})",
                file=sys.stderr,
            )

    for line in format_table(rows, header=not args.no_header):
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main())
