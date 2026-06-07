# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/validation.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Post-solve validation for LCS schedules
# ***************************************************************************

"""Post-solve validation for LCS schedules."""

from __future__ import annotations

from collections import Counter, defaultdict
from typing import List

import pandas as pd

from .feasibility import build_windows_45
from .models import DailyObservation, SolverConfig, SolverResult, WindowAssignment
from .solver import _daily_days_in_window, _sparse_days_in_window


def validate_schedule(result: SolverResult) -> pd.DataFrame:
    """Run validation checks and return a report DataFrame."""
    config = result.config
    checks: List[dict] = []

    def add(check_name: str, passed: bool, detail: str = "") -> None:
        checks.append(
            {
                "check": check_name,
                "passed": int(passed),
                "detail": detail,
            }
        )

    add("solver_status_optimal_or_feasible", result.status in ("OPTIMAL", "FEASIBLE"), result.status)
    add(
        "objective_recorded",
        result.objective_value is not None or result.status not in ("OPTIMAL", "FEASIBLE"),
        str(result.objective_value),
    )

    assignments = result.window_assignments
    categories = Counter(a.category for a in assignments)
    add("set_a_count", categories.get("A", 0) == config.set_a_count, str(categories.get("A", 0)))
    add("set_b_count", _count_b_fields(assignments) == config.set_b_count, str(_count_b_fields(assignments)))
    add("set_c_count", categories.get("C", 0) == config.set_c_count, str(categories.get("C", 0)))
    add("set_d_count", categories.get("D", 0) == config.set_d_count, str(categories.get("D", 0)))

    # Duplicates mean the same field_id appears in more than one set (A/B/C/D).
    categories_by_field: dict = defaultdict(set)
    for a in assignments:
        categories_by_field[a.field_id].add(a.category)
    cross_set_dupes = [fid for fid, cats in categories_by_field.items() if len(cats) > 1]
    add("no_duplicate_fields", len(cross_set_dupes) == 0, str(cross_set_dupes))

    capacity_violations = _check_daily_capacity(result.daily_observations, config)
    add("daily_capacity", len(capacity_violations) == 0, str(capacity_violations[:5]))

    vis_violations = _check_visibility(assignments, result)
    add("visibility", len(vis_violations) == 0, str(vis_violations[:5]))

    cadence_violations = _check_cadence(assignments, config)
    add("cadence", len(cadence_violations) == 0, str(cadence_violations[:5]))

    add("solver_status", True, result.status)
    add("objective_value", True, str(result.objective_value))

    return pd.DataFrame(checks)


def build_solver_summary(result: SolverResult, report_df: pd.DataFrame) -> dict:
    """Build solver_summary.json content."""
    daily_loads = Counter(obs.day for obs in result.daily_observations)
    loads = list(daily_loads.values()) if daily_loads else [0]
    categories = Counter(a.category for a in result.window_assignments)

    return {
        "status": result.status,
        "wall_time": result.wall_time_seconds,
        "objective": result.objective_value,
        "counts": {
            "A": categories.get("A", 0),
            "B": _count_b_fields(result.window_assignments),
            "C": categories.get("C", 0),
            "D": categories.get("D", 0),
        },
        "max_daily_load": max(loads),
        "average_daily_load": sum(loads) / len(loads),
        "validation_passed": int(report_df["passed"].min()) if not report_df.empty else 0,
        "config": {
            "set_a_count": result.config.set_a_count,
            "set_b_count": result.config.set_b_count,
            "set_c_count": result.config.set_c_count,
            "set_d_count": result.config.set_d_count,
            "daily_capacity": result.config.daily_capacity,
            "time_limit_seconds": result.config.time_limit_seconds,
        },
    }


def _count_b_fields(assignments: List[WindowAssignment]) -> int:
    return len({a.field_id for a in assignments if a.category == "B"})


def _check_daily_capacity(
    observations: List[DailyObservation], config: SolverConfig
) -> List[int]:
    loads = Counter(obs.day for obs in observations)
    return [day for day, load in loads.items() if load > config.daily_capacity]


def _check_visibility(assignments: List[WindowAssignment], result: SolverResult) -> List[str]:
    violations = []
    feasibility = result.feasibility
    windows_45 = build_windows_45(result.config)
    span_by_idx = {idx: (s, e) for idx, s, e in feasibility.windows_135}

    for item in assignments:
        if item.category in ("A", "D") or (item.category == "B" and item.cadence == "daily"):
            feasible = feasibility.feasible_a.get(item.field_id, set())
            if item.category == "D":
                feasible = feasibility.feasible_d.get(item.field_id, set())
            if item.window_index not in feasible:
                violations.append(f"field {item.field_id} window {item.window_index}")
        elif item.category == "B" and item.cadence == "sparse4":
            feasible = feasibility.feasible_b.get(item.field_id, set())
            if item.window_index not in feasible:
                violations.append(f"field {item.field_id} sparse window {item.window_index}")
        elif item.category == "C":
            feasible = feasibility.feasible_c.get(item.field_id, set())
            if item.window_index not in feasible:
                violations.append(f"field {item.field_id} span {item.window_index}")
    return violations


def _check_cadence(assignments: List[WindowAssignment], config: SolverConfig) -> List[str]:
    violations = []
    by_field: dict = defaultdict(list)
    for item in assignments:
        by_field[item.field_id].append(item)

    for field_id, items in by_field.items():
        if any(i.category == "B" for i in items):
            daily = [i for i in items if i.notes == "B_45" or (i.category == "B" and i.cadence == "daily")]
            sparse = [i for i in items if i.notes == "B_90" or (i.category == "B" and i.cadence == "sparse4")]
            if len(daily) != 1 or len(sparse) != 2:
                violations.append(f"B field {field_id} block counts daily={len(daily)} sparse={len(sparse)}")
    return violations
