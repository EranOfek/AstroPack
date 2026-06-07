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
    config = config or SolverConfig(
        set_a_count=1,
        set_b_count=1,
        set_c_count=1,
        set_d_count=1,
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
    assignments = [
        WindowAssignment("A", 1, "daily", 1, 45, 1),
        WindowAssignment("C", 1, "sparse4", 1, 135, 1),
    ]
    observations = [DailyObservation(1, 1, 1, "A", "daily")]
    result = _make_result(assignments, observations)
    report = validate_schedule(result)
    assert report.loc[report["check"] == "no_duplicate_fields", "passed"].iloc[0] == 0
