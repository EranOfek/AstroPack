# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/tests/test_validation.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Validation report tests for LCS CP-SAT schedules
# ***************************************************************************

"""Validation report tests."""

from __future__ import annotations

import pandas as pd

from lcs_cpsat.models import (
    DailyObservation,
    FeasibilityMaps,
    SolverConfig,
    SolverResult,
    WindowAssignment,
    WindowDef,
)
from lcs_cpsat.validation import validate_schedule


def _empty_feasibility() -> FeasibilityMaps:
    """Minimal FeasibilityMaps stub — validation tests build assignments manually."""
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


def _make_result(
    assignments,
    observations,
    config=None,
) -> SolverResult:
    """
    Build a synthetic SolverResult for validation unit tests.

    :param assignments: list of WindowAssignment
    :param observations: list of DailyObservation
    :param config: optional SolverConfig override
    :return: SolverResult with OPTIMAL status
    """
    config = config or SolverConfig(
        set_a_count=1,
        set_b_count=1,
        set_c_count=1,
        set_d_count=1,
        use_window_index_capacity=False,
        solve_set_d_separately=False,
    )
    return SolverResult(
        status="OPTIMAL",
        objective_value=100.0,
        wall_time_seconds=1.0,
        window_assignments=assignments,
        daily_observations=observations,
        config=config,
        fields_df=pd.DataFrame({"field_id": [1], "ra": [0.0], "dec": [0.0], "A_U": [0.5]}),
        feasibility=_empty_feasibility(),
    )


def test_validation_passes_good_schedule():
    """Well-formed schedule with one field per set should pass all checks."""
    assignments = [
        WindowAssignment("A", 1, "daily", 1, 45, 1),
        WindowAssignment("B", 2, "daily", 1, 45, 1, group_id=2, notes="B_45"),
        WindowAssignment("B", 2, "sparse4", 46, 90, 2, group_id=2, notes="B_90"),
        WindowAssignment("B", 2, "sparse4", 91, 135, 3, group_id=2, notes="B_90"),
        WindowAssignment("C", 3, "sparse4", 1, 135, 1),
        WindowAssignment("D", 4, "daily", 1, 45, 1),
    ]
    observations = [
        DailyObservation(1, 1, 1, "A", "daily"),
        DailyObservation(1, 2, 2, "B", "daily"),
        DailyObservation(1, 3, 3, "C", "sparse4"),
        DailyObservation(1, 4, 4, "D", "daily"),
    ]
    result = _make_result(assignments, observations)
    report = validate_schedule(result)
    assert report.loc[report["check"] == "no_duplicate_fields", "passed"].iloc[0] == 1
    assert report.loc[report["check"] == "daily_capacity", "passed"].iloc[0] == 1


def test_validation_catches_capacity_violation():
    """13 observations on one day exceeds default daily_capacity of 11."""
    assignments = [
        WindowAssignment("A", 1, "daily", 1, 45, 1),
    ]
    observations = [
        DailyObservation(1, i, i, "A", "daily") for i in range(1, 14)
    ]
    result = _make_result(assignments, observations)
    report = validate_schedule(result)
    assert report.loc[report["check"] == "daily_capacity", "passed"].iloc[0] == 0


def test_validation_catches_duplicate_fields():
    """Same field_id in both Set A and Set C must fail no_duplicate_fields."""
    assignments = [
        WindowAssignment("A", 1, "daily", 1, 45, 1),
        WindowAssignment("C", 1, "sparse4", 1, 135, 1),
    ]
    observations = [DailyObservation(1, 1, 1, "A", "daily")]
    result = _make_result(assignments, observations)
    report = validate_schedule(result)
    assert report.loc[report["check"] == "no_duplicate_fields", "passed"].iloc[0] == 0
