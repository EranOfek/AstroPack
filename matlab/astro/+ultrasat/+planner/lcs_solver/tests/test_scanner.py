# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/tests/test_scanner.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Tests for the LCS plan date scanner
# ***************************************************************************

"""Tests for the LCS plan date scanner."""

from __future__ import annotations

from datetime import date
from pathlib import Path

import pandas as pd

from lcs_cpsat.models import DailyObservation, SolverConfig, SolverResult, FeasibilityMaps, WindowDef
from lcs_cpsat.scanner import _obs_datetime, _day_offset_from_ref, scan_lcs_plans
from tests.test_tiny_case import _make_tiny_inputs


def _empty_feasibility() -> FeasibilityMaps:
    return FeasibilityMaps(
        windows_45=[WindowDef(1, 1, 45)],
        windows_135=[(1, 1, 135)],
        feasible_a={},
        feasible_b={},
        feasible_c={},
        feasible_d={},
        slack_45={},
        slack_135={},
        eligible_abc=set(),
        eligible_long=set(),
        eligible_d=set(),
    )


def test_day_offset_from_ref():
    ref = date(2029, 1, 1)
    assert _day_offset_from_ref(date(2029, 1, 1), ref) == 1
    assert _day_offset_from_ref(date(2029, 1, 15), ref) == 15


def test_obs_datetime_iso():
    config = SolverConfig(
        start_date="2029-01-01",
        daily_window_start_time_seconds=0.0,
        slot_time_days=900.0 / 86400.0,
    )
    obs = DailyObservation(day=1, slot_index=2, field_id=7, category="A", cadence="daily")
    dt = _obs_datetime(date(2029, 1, 1), obs, config)
    assert dt.isoformat().startswith("2029-01-01T00:15:00")


def test_scan_writes_index(tmp_path: Path):
    fields_df, windows_df, eligibility_df, config = _make_tiny_inputs()
    config.start_date = "2029-01-01"
    config.last_day = 135

    index_df = scan_lcs_plans(
        fields_df,
        windows_df,
        eligibility_df,
        config,
        scan_start_date="2029-01-01",
        scan_end_date="2029-01-03",
        out_dir=tmp_path,
        time_limit_seconds=30,
    )

    assert (tmp_path / "lcs_plan_index.csv").exists()
    assert len(index_df) == 3
    assert "plan_start_date" in index_df.columns
    assert "status" in index_df.columns
