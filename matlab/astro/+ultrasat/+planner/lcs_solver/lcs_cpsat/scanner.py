# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/scanner.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Scan candidate LCS plan start dates and write per-plan CSV outputs
# ***************************************************************************

"""Scan candidate LCS plan start dates and write per-plan CSV outputs."""

from __future__ import annotations

from dataclasses import replace
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import List, Tuple

import pandas as pd

from .feasibility import compute_feasibility
from .models import DailyObservation, SolverConfig, SolverResult
from .solver import build_and_solve

PLAN_DURATION_DAYS = 360
FEASIBLE_STATUSES = {"OPTIMAL", "FEASIBLE"}


def _parse_iso_date(value: str) -> date:
    return date.fromisoformat(value)


def _day_offset_from_ref(plan_start: date, ref_date: date) -> int:
    """Return campaign day index (1-based) for plan_start relative to ref_date."""
    return (plan_start - ref_date).days + 1


def _obs_datetime(
    anchor_date: date,
    obs: DailyObservation,
    config: SolverConfig,
) -> datetime:
    """Convert a daily observation to UTC ISO datetime."""
    day_offset = timedelta(days=obs.day - 1)
    slot_seconds = (obs.slot_index - 1) * config.slot_time_days * 86400.0
    window_seconds = config.daily_window_start_time_seconds
    base = datetime(
        anchor_date.year,
        anchor_date.month,
        anchor_date.day,
        tzinfo=timezone.utc,
    )
    return base + day_offset + timedelta(seconds=window_seconds + slot_seconds)


def _plan_filename(plan_start: date) -> str:
    return f"lcs_plan_{plan_start.strftime('%Y%m%d')}.csv"


def write_plan_csv(result: SolverResult, plan_start: date, out_path: Path) -> int:
    """Write one plan CSV with obs_datetime and field_id columns."""
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
    dates = []
    current = scan_start
    while current <= scan_end:
        dates.append(current)
        current += timedelta(days=1)
    return dates


def scan_lcs_plans(
    fields_df: pd.DataFrame,
    windows_df: pd.DataFrame,
    eligibility_df: pd.DataFrame,
    config: SolverConfig,
    scan_start_date: str,
    scan_end_date: str,
    out_dir: Path,
    time_limit_seconds: int | None = None,
) -> pd.DataFrame:
    """
    Scan daily plan start dates and write feasible plans to out_dir.

    Returns index DataFrame written to lcs_plan_index.csv.
    """
    out_dir.mkdir(parents=True, exist_ok=True)
    ref_date = _parse_iso_date(config.start_date)
    scan_start = _parse_iso_date(scan_start_date)
    scan_end = _parse_iso_date(scan_end_date)

    if scan_end < scan_start:
        raise ValueError("scan_end_date must be on or after scan_start_date")

    index_rows: List[dict] = []
    per_run_limit = time_limit_seconds if time_limit_seconds is not None else config.time_limit_seconds

    for plan_start in _iter_scan_dates(scan_start, scan_end):
        first_day = _day_offset_from_ref(plan_start, ref_date)
        last_plan_day = first_day + PLAN_DURATION_DAYS - 1

        if first_day < 1:
            index_rows.append(
                {
                    "plan_start_date": plan_start.isoformat(),
                    "status": "SKIPPED",
                    "plan_file": "",
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
                    "num_observations": 0,
                    "detail": "plan exceeds visibility horizon",
                }
            )
            continue

        run_config = replace(
            config,
            first_day=first_day,
            capacity_last_day=last_plan_day,
            time_limit_seconds=per_run_limit,
        )
        feasibility = compute_feasibility(fields_df, windows_df, eligibility_df, run_config)
        result = build_and_solve(fields_df, feasibility, run_config)

        if result.status in FEASIBLE_STATUSES:
            plan_file = _plan_filename(plan_start)
            plan_path = out_dir / plan_file
            num_obs = write_plan_csv(result, plan_start, plan_path)
            index_rows.append(
                {
                    "plan_start_date": plan_start.isoformat(),
                    "status": result.status,
                    "plan_file": plan_file,
                    "num_observations": num_obs,
                    "detail": "",
                }
            )
        else:
            index_rows.append(
                {
                    "plan_start_date": plan_start.isoformat(),
                    "status": result.status,
                    "plan_file": "",
                    "num_observations": 0,
                    "detail": "",
                }
            )

    index_df = pd.DataFrame(index_rows)
    index_path = out_dir / "lcs_plan_index.csv"
    index_df.to_csv(index_path, index=False)
    return index_df
