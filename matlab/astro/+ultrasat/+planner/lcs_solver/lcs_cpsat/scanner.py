# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/scanner.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Scan candidate LCS plan start dates and write per-plan CSV outputs
# ***************************************************************************

"""Scan candidate LCS plan start dates and write per-plan CSV outputs.

The single-plan solver works in campaign-day coordinates.  The scanner repeats
that solve for many calendar start dates by converting each date into the
corresponding campaign-day offset, then writes an index of feasible starts.
"""

from __future__ import annotations

from dataclasses import replace
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Dict, List, Optional

import pandas as pd

from .io import (
    write_daily_schedule,
    write_schedule_windows,
    write_solver_summary,
    write_validation_report,
    write_v3_outputs,
)
from .models import DailyObservation, SolverConfig, SolverResult
from .solver import build_and_solve_with_branching
from .validation import build_solver_summary, validate_schedule

PLAN_DURATION_DAYS = 360
VISIBILITY_HORIZON_DAYS = 420
FEASIBLE_STATUSES = {"OPTIMAL", "FEASIBLE"}


def _parse_iso_date(value: str) -> date:
    """Parse an ISO yyyy-mm-dd string into a date object."""
    return date.fromisoformat(value)


def _day_offset_from_ref(plan_start: date, ref_date: date) -> int:
    """Convert a calendar start date into the solver's 1-based day number."""
    return (plan_start - ref_date).days + 1


def _obs_datetime(
    anchor_date: date,
    obs: DailyObservation,
    config: SolverConfig,
) -> datetime:
    """Convert a daily observation into an absolute UTC observation timestamp."""
    day_offset = timedelta(days=obs.day - 1)
    slot_seconds = (obs.slot_index - 1) * config.slot_time_days * 86400.0
    window_seconds = config.daily_window_start_time_seconds
    base = datetime(
        anchor_date.year, anchor_date.month, anchor_date.day, tzinfo=timezone.utc
    )
    return base + day_offset + timedelta(seconds=window_seconds + slot_seconds)


def _plan_filename(plan_start: date) -> str:
    """Return the compact per-plan CSV filename used by yearly scans."""
    return f"lcs_plan_{plan_start.strftime('%Y%m%d')}.csv"


def _plan_dir_name(plan_start: date) -> str:
    """Return the helper-compatible success folder name for full outputs."""
    return f"{plan_start.isoformat()}/success"


def write_plan_csv(result: SolverResult, plan_start: date, out_path: Path) -> int:
    """Write the minimal observation-time CSV for one feasible scanned plan."""
    anchor = _parse_iso_date(result.config.start_date)
    rows = []
    for obs in result.daily_observations:
        rows.append(
            {
                "obs_datetime": _obs_datetime(anchor, obs, result.config).isoformat(),
                "field_id": obs.field_id,
            }
        )
    df = pd.DataFrame(rows)
    if not df.empty:
        df = df.sort_values("obs_datetime").reset_index(drop=True)
    df.to_csv(out_path, index=False)
    return len(df)


def _iter_scan_dates(scan_start: date, scan_end: date) -> List[date]:
    """Return every calendar date in the inclusive scan range."""
    dates = []
    current = scan_start
    while current <= scan_end:
        dates.append(current)
        current += timedelta(days=1)
    return dates


def _max_clipped_window_by_field(
    windows_df: pd.DataFrame,
    field_ids: List[int],
    first_day: int,
    last_day: int,
) -> Dict[int, int]:
    """Return each field's longest visibility window after horizon clipping."""
    max_by_field = {field_id: 0 for field_id in field_ids}
    for _, row in windows_df.iterrows():
        field_id = int(row["field_id"])
        start = max(int(row["vis_start_day"]), first_day)
        end = min(int(row["vis_end_day"]), last_day)
        if end >= start:
            max_by_field[field_id] = max(max_by_field.get(field_id, 0), end - start + 1)
    return max_by_field


def build_v3_category_eligibility(
    fields_df: pd.DataFrame,
    windows_df: pd.DataFrame,
    windows_1dgap_df: Optional[pd.DataFrame],
    config: SolverConfig,
) -> pd.DataFrame:
    """
    Recreate LcsHelper_v3.categorizeFields_v3 for one scanned absolute date.

    The broad MATLAB export may cover many scanned starts. V3, however,
    categorizes fields inside a fresh 420-day horizon for each start date.
    This function clips visibility windows to config.first_day..config.last_day
    and exports allowed_set_a/b/c pools for compute_feasibility().
    """
    field_ids = fields_df["field_id"].astype(int).tolist()
    au_by_field = {
        int(row["field_id"]): float(row["A_U"])
        for _, row in fields_df.iterrows()
    }
    # For a scan, visibility must be evaluated inside the shifted 420-day
    # horizon of the candidate plan start, not the original export horizon.
    max_strict = _max_clipped_window_by_field(
        windows_df, field_ids, config.first_day, config.last_day
    )
    if windows_1dgap_df is None:
        max_1dgap = dict(max_strict)
    else:
        max_1dgap = _max_clipped_window_by_field(
            windows_1dgap_df, field_ids, config.first_day, config.last_day
        )

    use1gap = {field_id: False for field_id in field_ids}
    # First build the low-extinction pool with strict visibility.  If there are
    # not enough fields, v3 permits one-day-gap merged windows as a rescue.
    low_ext = {
        field_id
        for field_id in field_ids
        if au_by_field[field_id] <= config.max_extinction
        and max_strict[field_id] >= config.min_window_days
    }
    if len(low_ext) < (
        config.set_a_count + config.set_b_count + config.set_c_count + 1
    ):
        low_ext = {
            field_id
            for field_id in field_ids
            if au_by_field[field_id] <= config.max_extinction
            and max_1dgap[field_id] >= config.min_window_days
        }
        for field_id in low_ext:
            if max_1dgap[field_id] >= config.min_window_days and max_strict[field_id] < config.min_window_days:
                use1gap[field_id] = True

    long_low_ext = {
        field_id
        for field_id in low_ext
        if max_strict[field_id] >= config.long_window_days
    }
    if len(long_low_ext) < (config.set_b_count + config.set_c_count):
        long_low_ext = {
            field_id
            for field_id in low_ext
            if max_1dgap[field_id] >= config.long_window_days
        }
        for field_id in long_low_ext:
            if max_1dgap[field_id] >= config.long_window_days and max_strict[field_id] < config.long_window_days:
                use1gap[field_id] = True

    long_sorted = sorted(long_low_ext, key=lambda fid: (au_by_field[fid], fid))
    # V3 partitions long low-extinction fields by extinction rank: early fields
    # go to B, next fields go to C, and the remaining long fields can feed A.
    set_b = set(long_sorted[: config.set_b_count])
    set_c = set(long_sorted[config.set_b_count : config.set_b_count + config.set_c_count])
    long_leftover = set(long_sorted[config.set_b_count + config.set_c_count :])
    short_fields = low_ext - long_low_ext
    set_a = short_fields | long_leftover

    rows = []
    for field_id in field_ids:
        rows.append(
            {
                "field_id": field_id,
                "eligible_abc": int(field_id in low_ext),
                "eligible_long_window": int(field_id in long_low_ext),
                "eligible_d": int(
                    au_by_field[field_id] > config.max_extinction
                    and max_strict[field_id] >= config.min_window_days
                ),
                "use1dgap": int(use1gap[field_id]),
                "allowed_set_a": int(field_id in set_a),
                "allowed_set_b": int(field_id in set_b),
                "allowed_set_c": int(field_id in set_c),
                "max_window_days": max_strict[field_id],
                "max_window_1dgap_days": max_1dgap[field_id],
            }
        )
    return pd.DataFrame(rows)


def build_v3_physical_eligibility(
    fields_df: pd.DataFrame,
    windows_df: pd.DataFrame,
    windows_1dgap_df: Optional[pd.DataFrame],
    config: SolverConfig,
) -> pd.DataFrame:
    """Build per-date v3 physical eligibility without freezing A/B/C pools."""
    elig = build_v3_category_eligibility(fields_df, windows_df, windows_1dgap_df, config)
    return elig.drop(columns=["allowed_set_a", "allowed_set_b", "allowed_set_c"])


def scan_lcs_plans(
    fields_df: pd.DataFrame,
    windows_df: pd.DataFrame,
    eligibility_df: pd.DataFrame,
    config: SolverConfig,
    scan_start_date: str,
    scan_end_date: str,
    out_dir: Path,
    time_limit_seconds: int | None = None,
    windows_1dgap_df: Optional[pd.DataFrame] = None,
    write_full_outputs: bool = False,
) -> pd.DataFrame:
    """Scan daily plan start dates and write an index plus feasible plan files."""
    out_dir.mkdir(parents=True, exist_ok=True)
    ref_date = _parse_iso_date(config.start_date)
    scan_start = _parse_iso_date(scan_start_date)
    scan_end = _parse_iso_date(scan_end_date)

    if scan_end < scan_start:
        raise ValueError("scan_end_date must be on or after scan_start_date")

    index_rows: List[dict] = []
    per_run_limit = (
        time_limit_seconds if time_limit_seconds is not None else config.time_limit_seconds
    )

    for plan_start in _iter_scan_dates(scan_start, scan_end):
        # Shift the solver horizon so day 1 is the candidate plan start.
        first_day = _day_offset_from_ref(plan_start, ref_date)
        last_plan_day = first_day + PLAN_DURATION_DAYS - 1

        if first_day < 1:
            index_rows.append(
                {
                    "plan_start_date": plan_start.isoformat(),
                    "status": "SKIPPED",
                    "plan_file": "",
                    "plan_dir": "",
                    "num_observations": 0,
                    "detail": "before visibility data start",
                }
            )
            continue

        if last_plan_day > config.last_day:
            index_rows.append(
                {
                    "plan_start_date": plan_start.isoformat(),
                    "status": "SKIPPED",
                    "plan_file": "",
                    "plan_dir": "",
                    "num_observations": 0,
                    "detail": "plan exceeds visibility horizon",
                }
            )
            continue

        run_config = replace(
            config,
            first_day=first_day,
            last_day=first_day + VISIBILITY_HORIZON_DAYS - 1,
            capacity_last_day=last_plan_day,
            time_limit_seconds=per_run_limit,
        )
        run_eligibility_df = build_v3_physical_eligibility(
            fields_df, windows_df, windows_1dgap_df, run_config
        )
        # Each date is an independent solve.  The solver handles Set C branch
        # attempts and Set A rescue shifts internally.
        result = build_and_solve_with_branching(
            fields_df,
            windows_df,
            run_eligibility_df,
            run_config,
            windows_1dgap_df,
        )

        if result.status in FEASIBLE_STATUSES:
            # Minimal output is a timestamp/field CSV plus the yearly index.
            # Full output additionally writes helper-style per-day folders.
            plan_file = _plan_filename(plan_start)
            plan_path = out_dir / plan_file
            num_obs = write_plan_csv(result, plan_start, plan_path)
            plan_dir_name = ""
            if write_full_outputs:
                plan_dir_name = _plan_dir_name(plan_start)
                plan_dir = out_dir / plan_dir_name
                plan_dir.mkdir(parents=True, exist_ok=True)
                write_schedule_windows(result, plan_dir)
                write_daily_schedule(result, plan_dir)
                write_v3_outputs(result, plan_dir)
                report_df = validate_schedule(result)
                write_validation_report(report_df, plan_dir)
                write_solver_summary(
                    build_solver_summary(result, report_df),
                    plan_dir,
                )
            index_rows.append(
                {
                    "plan_start_date": plan_start.isoformat(),
                    "status": result.status,
                    "plan_file": plan_file,
                    "plan_dir": plan_dir_name,
                    "num_observations": num_obs,
                    "detail": f"SetC_start_ind={result.config.set_c_start_ind}",
                }
            )
        else:
            index_rows.append(
                {
                    "plan_start_date": plan_start.isoformat(),
                    "status": result.status,
                    "plan_file": "",
                    "plan_dir": "",
                    "num_observations": 0,
                    "detail": "",
                }
            )

    index_df = pd.DataFrame(index_rows)
    index_path = out_dir / "lcs_plan_index.csv"
    index_df.to_csv(index_path, index=False)
    return index_df
