#!/usr/bin/env python3
"""Validate CSV output from validate_LcsHelper_v4.m.

This script does not call MATLAB. It validates the CSV files written under
+debug/+lcs_v4/output/validation and writes a concise text report in the same directory.
"""

from __future__ import annotations

import argparse
import csv
from collections import Counter, defaultdict
from pathlib import Path


SET_A_NUM = 48
SET_B_NUM = 16
SET_C_NUM = 16
SET_D_MAX = 4
MIN_WINDOW = 45
DAILY_LCS_SLOTS = 11
DEFAULT_SETD_RANK = [79, 12, 48, 28, 16, 88, 55, 32, 213, 26]
SCHEDULE_COLUMNS = {"category", "group", "ind", "start", "end", "Field"}
WINDOW_COLUMNS = {"start", "end"}
DAILY_COLUMNS = {"day"}
VALID_CATEGORIES = {"A", "B_45", "B_90", "C", "D"}


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--output-dir",
        default=str(Path(__file__).resolve().parent / "output" / "validation"),
        help="Directory containing schedule.csv, full_windows.csv, and daily_schedule.csv.",
    )
    args = parser.parse_args()

    out_dir = Path(args.output_dir)
    checks: list[tuple[str, bool, str]] = []
    warnings: list[tuple[str, bool, str]] = []

    schedule = read_csv(out_dir / "schedule.csv")
    windows = read_csv(out_dir / "full_windows.csv")
    daily = read_csv(out_dir / "daily_schedule.csv")

    validate_schema(checks, schedule, windows, daily)

    placed = [r for r in schedule if to_int(r.get("Field")) > 0]
    rows_a = [r for r in placed if r.get("category") == "A"]
    rows_b = [r for r in placed if r.get("category") in {"B_45", "B_90"}]
    rows_b45 = [r for r in placed if r.get("category") == "B_45"]
    rows_b90 = [r for r in placed if r.get("category") == "B_90"]
    rows_c = [r for r in placed if r.get("category") == "C"]
    rows_d = [r for r in placed if r.get("category") == "D"]

    add(checks, "CSV files exist and have rows", bool(schedule and windows and daily),
        f"schedule={len(schedule)} full_windows={len(windows)} daily={len(daily)}")
    add(checks, "Schedule has placed rows", bool(placed), f"placed={len(placed)}")
    add(checks, "Schedule categories valid",
        all(r.get("category") in VALID_CATEGORIES for r in placed),
        invalid_category_msg(placed))
    add(checks, "Schedule placed numeric fields valid",
        all_numeric_fields_valid(placed),
        invalid_numeric_msg(placed))

    add(checks, "SetA field count", len(rows_a) == SET_A_NUM, f"got {len(rows_a)}")
    add(checks, "SetB row count", len(rows_b) == 3 * SET_B_NUM, f"got {len(rows_b)}")
    add(checks, "SetB unique field count", len(field_set(rows_b)) == SET_B_NUM,
        f"got {len(field_set(rows_b))}")
    add(checks, "SetC field count", len(rows_c) == SET_C_NUM, f"got {len(rows_c)}")
    add(checks, "SetD field count", len(rows_d) <= SET_D_MAX, f"got {len(rows_d)}")

    add(checks, "SetA unique fields", unique_fields(rows_a), duplicate_msg(rows_a))
    add(checks, "SetA 45-day windows", all(window_len(r) == MIN_WINDOW for r in rows_a),
        bad_count_msg(rows_a, lambda r: window_len(r) == MIN_WINDOW))
    add(checks, "SetA original groups use <= 8 fields",
        all(sum(to_int(r["group"]) == g for r in rows_a) <= 8 for g in range(1, 7)),
        group_count_msg(rows_a, range(1, 7)))

    add(checks, "SetB rows are 45 days", all(window_len(r) == MIN_WINDOW for r in rows_b),
        bad_count_msg(rows_b, lambda r: window_len(r) == MIN_WINDOW))
    setb_aligned = align_with_windows(rows_b, windows)
    add(checks, "SetB rows align with full windows", setb_aligned,
        "all rows align" if setb_aligned else "one or more SetB rows do not match full_windows boundaries")
    validate_setb_fields(checks, rows_b)

    add(checks, "SetC unique fields", unique_fields(rows_c), duplicate_msg(rows_c))
    add(checks, "SetC rows are 135 days", all(window_len(r) == 3 * MIN_WINDOW for r in rows_c),
        bad_count_msg(rows_c, lambda r: window_len(r) == 3 * MIN_WINDOW))
    add(checks, "SetC groups are v4 block groups",
        all(11 <= to_int(r["group"]) <= 16 for r in rows_c),
        bad_count_msg(rows_c, lambda r: 11 <= to_int(r["group"]) <= 16))
    setc_starts_align = all(to_int(r["start"]) in {to_int(w["start"]) for w in windows} for r in rows_c)
    add(checks, "SetC starts align with full windows", setc_starts_align,
        "all starts align" if setc_starts_align else "one or more SetC starts are not full window starts")
    add(checks, "SetC local ind range",
        all(1 <= to_int(r["ind"]) <= 8 for r in rows_c),
        bad_count_msg(rows_c, lambda r: 1 <= to_int(r["ind"]) <= 8))

    if rows_d:
        add(checks, "SetD unique fields", unique_fields(rows_d), duplicate_msg(rows_d))
        add(checks, "SetD rows are 45 days", all(window_len(r) == MIN_WINDOW for r in rows_d),
            bad_count_msg(rows_d, lambda r: window_len(r) == MIN_WINDOW))
        add(checks, "SetD group encoding", all(301 <= to_int(r["group"]) <= 304 for r in rows_d),
            bad_count_msg(rows_d, lambda r: 301 <= to_int(r["group"]) <= 304))
        setd_unique_slots = len({to_int(r["group"]) for r in rows_d}) == len(rows_d)
        add(checks, "SetD unique slots", setd_unique_slots,
            "all SetD group slots unique" if setd_unique_slots else "duplicate SetD group slot")

    validate_slot_budget(checks, placed, windows)
    validate_window_bounds(checks, placed, daily)
    validate_cross_set_duplicates(checks, rows_a, rows_b, rows_c, rows_d)
    validate_daily_schedule(checks, placed, daily)
    validate_warning_checks(warnings, rows_a, rows_b, rows_c, rows_d)

    passed = sum(ok for _, ok, _ in checks)
    failed = len(checks) - passed
    warning_count = sum(not ok for _, ok, _ in warnings)
    lines = [
        f"validate_LcsHelper_v4.py report",
        f"checks passed: {passed}",
        f"checks failed: {failed}",
        f"warnings: {warning_count}",
        "",
    ]
    for name, ok, detail in checks:
        status = "PASS" if ok else "FAIL"
        lines.append(f"[{status}] {name}: {detail}")
    if warnings:
        lines.append("")
        for name, ok, detail in warnings:
            status = "WARN-OK" if ok else "WARN"
            lines.append(f"[{status}] {name}: {detail}")

    report_path = out_dir / "validate_LcsHelper_v4_py_report.txt"
    out_dir.mkdir(parents=True, exist_ok=True)
    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print("\n".join(lines))
    print(f"\nreport: {report_path}")
    return 0 if failed == 0 else 1


def read_csv(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def add(checks: list[tuple[str, bool, str]], name: str, ok: bool, detail: str) -> None:
    checks.append((name, bool(ok), detail))


def to_int(value: object) -> int:
    if value in (None, ""):
        return 0
    try:
        return int(float(str(value)))
    except (TypeError, ValueError):
        return 0


def is_int_like(value: object) -> bool:
    if value in (None, ""):
        return False
    try:
        int(float(str(value)))
        return True
    except (TypeError, ValueError):
        return False


def window_len(row: dict[str, str]) -> int:
    return to_int(row["end"]) - to_int(row["start"]) + 1


def field_set(rows: list[dict[str, str]]) -> set[int]:
    return {to_int(r["Field"]) for r in rows if to_int(r.get("Field")) > 0}


def unique_fields(rows: list[dict[str, str]]) -> bool:
    values = [to_int(r["Field"]) for r in rows]
    return bool(values) and len(values) == len(set(values))


def duplicate_msg(rows: list[dict[str, str]]) -> str:
    values = [to_int(r["Field"]) for r in rows]
    dupes = [field for field, count in Counter(values).items() if count > 1]
    return "duplicates=" + repr(dupes)


def invalid_category_msg(rows: list[dict[str, str]]) -> str:
    bad = sorted({r.get("category", "") for r in rows if r.get("category") not in VALID_CATEGORIES})
    return "invalid_categories=" + repr(bad)


def all_numeric_fields_valid(rows: list[dict[str, str]]) -> bool:
    numeric_cols = ("Field", "group", "ind", "start", "end")
    return all(all(is_int_like(r.get(col)) for col in numeric_cols) for r in rows)


def invalid_numeric_msg(rows: list[dict[str, str]]) -> str:
    numeric_cols = ("Field", "group", "ind", "start", "end")
    bad = []
    for idx, row in enumerate(rows, start=1):
        bad_cols = [col for col in numeric_cols if not is_int_like(row.get(col))]
        if bad_cols:
            bad.append(f"row {idx}: {bad_cols}")
    return "; ".join(bad[:5]) if bad else "all placed numeric columns are integer-like"


def bad_count_msg(rows: list[dict[str, str]], pred) -> str:
    return f"bad_rows={sum(not pred(r) for r in rows)}"


def group_count_msg(rows: list[dict[str, str]], groups) -> str:
    counts = {g: sum(to_int(r["group"]) == g for r in rows) for g in groups}
    return f"counts={counts}"


def align_with_windows(rows: list[dict[str, str]], windows: list[dict[str, str]]) -> bool:
    pairs = {(to_int(w["start"]), to_int(w["end"])) for w in windows}
    return all((to_int(r["start"]), to_int(r["end"])) in pairs for r in rows)


def validate_setb_fields(checks: list[tuple[str, bool, str]], rows_b: list[dict[str, str]]) -> None:
    ok = True
    details = []
    by_field: dict[int, list[dict[str, str]]] = defaultdict(list)
    for row in rows_b:
        by_field[to_int(row["Field"])].append(row)
    for field, rows in by_field.items():
        n45 = sum(r["category"] == "B_45" for r in rows)
        n90 = sum(r["category"] == "B_90" for r in rows)
        fw_inds = [setb_fw_ind(r) for r in rows]
        span = max(to_int(r["start"]) for r in rows) - min(to_int(r["start"]) for r in rows) + MIN_WINDOW
        field_ok = n45 == 1 and n90 == 2 and len(set(fw_inds)) == 3 and span == 3 * MIN_WINDOW
        if not field_ok:
            ok = False
            details.append(f"field {field}: n45={n45} n90={n90} fw={fw_inds} span={span}")
    add(checks, "SetB per-field 1xB45 2xB90 135-day span", ok,
        "; ".join(details[:5]) if details else f"fields={len(by_field)}")


def setb_fw_ind(row: dict[str, str]) -> int:
    group = to_int(row["group"])
    return group - 100 if row["category"] == "B_45" else group - 200


def validate_slot_budget(checks: list[tuple[str, bool, str]], placed: list[dict[str, str]], windows: list[dict[str, str]]) -> None:
    ninds = len(windows)
    n_a = [0] * ninds
    n_b45 = [0] * ninds
    n_b90 = [0] * ninds
    n_c = [0] * ninds
    n_d = [0] * ninds
    start_to_ind = {to_int(w["start"]): i for i, w in enumerate(windows)}

    for row in placed:
        cat = row["category"]
        ind = to_int(row["ind"])
        group = to_int(row["group"])
        if cat == "A" and 1 <= ind <= ninds:
            n_a[ind - 1] += 1
        elif cat == "B_45" and 1 <= group - 100 <= ninds:
            n_b45[group - 101] += 1
        elif cat == "B_90" and 1 <= group - 200 <= ninds:
            n_b90[group - 201] += 1
        elif cat == "C":
            start_ind = start_to_ind.get(to_int(row["start"]))
            if start_ind is not None:
                for i in range(start_ind, min(start_ind + 3, ninds)):
                    n_c[i] += 1
        elif cat == "D" and 1 <= ind <= ninds:
            n_d[ind - 1] += 1

    n_cadence4 = [b90 + c for b90, c in zip(n_b90, n_c)]
    filled_abc = [a + b45 + x / 4 for a, b45, x in zip(n_a, n_b45, n_cadence4)]
    filled_with_d = [abc + d for abc, d in zip(filled_abc, n_d)]
    add(checks, "Slot budget nCadence4 divisible by 4", all(x % 4 == 0 for x in n_cadence4),
        f"nCadence4={n_cadence4}")
    add(checks, "Slot budget filledABC <= 11", all(x <= DAILY_LCS_SLOTS for x in filled_abc),
        f"filledABC={filled_abc} nD={n_d} filled+D={filled_with_d}")
    add(checks, "Final slot use filledABC+nD <= 11", all(x <= DAILY_LCS_SLOTS for x in filled_with_d),
        f"filledABC={filled_abc} nD={n_d} filled+D={filled_with_d}")


def validate_window_bounds(checks: list[tuple[str, bool, str]], placed: list[dict[str, str]], daily: list[dict[str, str]]) -> None:
    if not placed or not daily:
        add(checks, "Window bounds", False, "missing placed schedule or daily schedule")
        return
    first_day = min(to_int(r["day"]) for r in daily)
    last_day = max(to_int(r["day"]) for r in daily)
    add(checks, "Window bounds start", min(to_int(r["start"]) for r in placed) >= first_day, f"first_day={first_day}")
    add(checks, "Window bounds end", max(to_int(r["end"]) for r in placed) <= last_day, f"last_day={last_day}")


def validate_cross_set_duplicates(checks, rows_a, rows_b, rows_c, rows_d) -> None:
    sets = {"A": field_set(rows_a), "B": field_set(rows_b), "C": field_set(rows_c), "D": field_set(rows_d)}
    for left, right in [("A", "B"), ("A", "C"), ("A", "D"), ("B", "C"), ("B", "D"), ("C", "D")]:
        overlap = sets[left] & sets[right]
        add(checks, f"No cross-set duplicates {left} vs {right}", not overlap, f"overlap={sorted(overlap)}")


def validate_daily_schedule(checks: list[tuple[str, bool, str]], placed: list[dict[str, str]], daily: list[dict[str, str]]) -> None:
    slot_cols = [c for c in daily[0].keys() if c.startswith("slot_")] if daily else []
    observed = 0
    for row in daily:
        for col in slot_cols:
            if row.get(col) not in ("", "NaN", "nan"):
                observed += 1
    add(checks, "Daily schedule row count", len(daily) > 0, f"rows={len(daily)}")
    add(checks, "Daily schedule slot count", len(slot_cols) == DAILY_LCS_SLOTS, f"slots={len(slot_cols)}")
    add(checks, "Daily schedule contains observations", observed > 0, f"observed={observed}")
    n_bad, first_bad = daily_schedule_mismatch_count(placed, daily, slot_cols)
    add(checks, "Daily schedule matches schedule rows and cadence", n_bad == 0,
        f"bad_days={n_bad} first_bad_day={first_bad}")


def validate_schema(
    checks: list[tuple[str, bool, str]],
    schedule: list[dict[str, str]],
    windows: list[dict[str, str]],
    daily: list[dict[str, str]],
) -> None:
    schedule_cols = set(schedule[0].keys()) if schedule else set()
    window_cols = set(windows[0].keys()) if windows else set()
    daily_cols = set(daily[0].keys()) if daily else set()
    slot_cols = {f"slot_{i}" for i in range(1, DAILY_LCS_SLOTS + 1)}

    add(checks, "Schedule CSV required columns", SCHEDULE_COLUMNS <= schedule_cols,
        f"missing={sorted(SCHEDULE_COLUMNS - schedule_cols)}")
    add(checks, "Full_windows CSV required columns", WINDOW_COLUMNS <= window_cols,
        f"missing={sorted(WINDOW_COLUMNS - window_cols)}")
    add(checks, "Daily_schedule CSV required columns", (DAILY_COLUMNS | slot_cols) <= daily_cols,
        f"missing={sorted((DAILY_COLUMNS | slot_cols) - daily_cols)}")


def daily_schedule_mismatch_count(
    placed: list[dict[str, str]],
    daily: list[dict[str, str]],
    slot_cols: list[str],
) -> tuple[int, int | None]:
    if not placed or not daily or len(slot_cols) != DAILY_LCS_SLOTS:
        return 1, None

    days = [to_int(r["day"]) for r in daily]
    first_day = min(days)
    last_day = max(days)
    expected_by_day: dict[int, Counter[int]] = {day: Counter() for day in days}

    for row in placed:
        field = to_int(row.get("Field"))
        cat = row.get("category")
        start = to_int(row.get("start"))
        end = to_int(row.get("end"))
        ind = to_int(row.get("ind"))
        if field <= 0 or cat not in VALID_CATEGORIES:
            continue
        for curr_day in range(start, end + 1):
            if curr_day < first_day or curr_day > last_day:
                continue
            if cat in {"C", "B_90"} and ((curr_day - start + 1) % 4) != (ind % 4):
                continue
            expected_by_day.setdefault(curr_day, Counter())[field] += 1

    n_bad = 0
    first_bad: int | None = None
    for row in daily:
        day = to_int(row["day"])
        actual = Counter()
        for col in slot_cols:
            value = row.get(col)
            if value not in ("", "NaN", "nan", None):
                actual[to_int(value)] += 1
        expected = expected_by_day.get(day, Counter())
        if actual != expected:
            n_bad += 1
            if first_bad is None:
                first_bad = day
    return n_bad, first_bad


def validate_warning_checks(
    warnings: list[tuple[str, bool, str]],
    rows_a: list[dict[str, str]],
    rows_b: list[dict[str, str]],
    rows_c: list[dict[str, str]],
    rows_d: list[dict[str, str]],
) -> None:
    moved = [r for r in rows_a if to_int(r["group"]) > 6]
    if moved:
        slots = {(to_int(r["group"]), to_int(r["ind"])) for r in moved}
        add(warnings, "SetA moved group accounting", len(slots) == len(moved),
            f"moved_rows={len(moved)} moved_slots={sorted(slots)}")
    else:
        add(warnings, "SetA moved group accounting", True, "no moved SetA rows")

    add(warnings, "Long-field extinction ranking", False,
        "not available from CSV output; MATLAB validator has object field tables")

    if not rows_d:
        add(warnings, "SetD ranking", True, "no SetD rows")
        return
    rows_d_sorted = sorted(rows_d, key=lambda r: to_int(r["group"]))
    positions = []
    missing = []
    for row in rows_d_sorted:
        field = to_int(row["Field"])
        try:
            positions.append(DEFAULT_SETD_RANK.index(field) + 1)
        except ValueError:
            missing.append(field)
    add(warnings, "SetD ranking fields in default rank list", not missing,
        f"missing={missing} rank_positions={positions}")
    add(warnings, "SetD ranking selected order follows rank order",
        not missing and all(b >= a for a, b in zip(positions, positions[1:])),
        f"rank_positions={positions}")
    add(warnings, "SetD ranking earlier fields skipped",
        not missing and (max(positions, default=0) <= len(rows_d_sorted)),
        f"rank_positions={positions}")


if __name__ == "__main__":
    raise SystemExit(main())
