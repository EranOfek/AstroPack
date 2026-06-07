# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/models.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Data models for the LCS CP-SAT solver
# ***************************************************************************

"""Data models for the LCS CP-SAT solver."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd


@dataclass
class WindowDef:
    """One fixed 45-day scheduling window on the campaign timeline."""

    index: int       # 1-based window index (W1, W2, ...)
    start_day: int   # first campaign day in window
    end_day: int     # last campaign day in window

    @property
    def length(self) -> int:
        """Number of days in this window."""
        return self.end_day - self.start_day + 1


@dataclass
class SolverConfig:
    """Campaign parameters and CP-SAT solver settings."""

    first_day: int = 1
    last_day: int = 420
    min_window_days: int = 45
    long_window_days: int = 135
    daily_capacity: int = 11
    set_a_count: int = 48
    set_a_n_groups: int = 6          # parallel Set A groups (v3 layout)
    set_a_fields_per_group: int = 8  # slots per group = number of 45-day windows
    set_b_count: int = 16
    set_c_count: int = 16
    set_d_count: int = 4
    max_extinction: float = 1.0
    d_ranked_fields: List[int] = field(
        default_factory=lambda: [79, 12, 48, 28, 16, 88, 55, 32, 213, 26]
    )
    require_b_consecutive: bool = False
    sparse_phase: int = 0
    sparse_cadence: int = 4
    capacity_last_day: int = 360     # last day where daily slot limit is enforced
    time_limit_seconds: int = 300
    start_date: str = "2029-01-01"   # calendar anchor for day 1 (ISO)
    daily_window_start_time_seconds: float = 0.0
    slot_time_days: float = 3 * 300 / 86400
    weight_d_rank: int = 1000
    weight_slack: int = 10
    weight_extinction: int = 1
    num_windows_45: int = 8

    @classmethod
    def from_dict(cls, data: dict) -> "SolverConfig":
        """
        Build config from JSON dict exported by MATLAB.

        :param data: parsed lcs_params.json content
        :return: SolverConfig with MATLAB key names mapped to Python fields
        """
        # Map Python field names to keys in lcs_params.json
        mapping = {
            "first_day": "first_day",
            "last_day": "num_days",
            "min_window_days": "min_window_days",
            "long_window_days": "max_window_cut_days",
            "daily_capacity": "daily_lcs_slots",
            "set_a_count": "set_a_total",
            "set_b_count": "set_b_count",
            "set_c_count": "set_c_count",
            "set_d_count": "set_d_count",
            "max_extinction": "max_extinction",
        }
        kwargs = {}
        for attr, key in mapping.items():
            if key in data:
                kwargs[attr] = data[key]
        # Optional keys not in the base mapping
        if "d_ranked_fields" in data:
            kwargs["d_ranked_fields"] = data["d_ranked_fields"]
        if "require_b_consecutive" in data:
            kwargs["require_b_consecutive"] = data["require_b_consecutive"]
        if "sparse_phase" in data:
            kwargs["sparse_phase"] = data["sparse_phase"]
        if "time_limit_seconds" in data:
            kwargs["time_limit_seconds"] = data["time_limit_seconds"]
        if "weight_d_rank" in data:
            kwargs["weight_d_rank"] = data["weight_d_rank"]
        if "weight_slack" in data:
            kwargs["weight_slack"] = data["weight_slack"]
        if "weight_extinction" in data:
            kwargs["weight_extinction"] = data["weight_extinction"]
        if "capacity_last_day" in data:
            kwargs["capacity_last_day"] = data["capacity_last_day"]
        if "start_date" in data:
            kwargs["start_date"] = data["start_date"]
        if "daily_window_start_time_seconds" in data:
            kwargs["daily_window_start_time_seconds"] = data[
                "daily_window_start_time_seconds"
            ]
        if "slot_time_days" in data:
            kwargs["slot_time_days"] = data["slot_time_days"]
        if "set_a_n_groups" in data:
            kwargs["set_a_n_groups"] = data["set_a_n_groups"]
        if "set_a_fields_per_group" in data:
            kwargs["set_a_fields_per_group"] = data["set_a_fields_per_group"]
            # slot count equals 45-day window count unless overridden
            if "num_windows_45" not in kwargs:
                kwargs["num_windows_45"] = data["set_a_fields_per_group"]
        return cls(**kwargs)


@dataclass
class FeasibilityMaps:
    """Precomputed (field, window) feasibility used to prune CP-SAT variables."""

    windows_45: List[WindowDef]
    windows_135: List[Tuple[int, int, int]]  # (span_idx, start_day, end_day)
    feasible_a: Dict[int, Set[int]]   # field_id -> feasible 45-day window indices
    feasible_b: Dict[int, Set[int]]
    feasible_c: Dict[int, Set[int]]   # field_id -> feasible 135-day span indices
    feasible_d: Dict[int, Set[int]]
    slack_45: Dict[Tuple[int, int], int]   # (field_id, window_idx) -> extra vis days
    slack_135: Dict[Tuple[int, int], int]
    eligible_abc: Set[int]
    eligible_long: Set[int]
    eligible_d: Set[int]


@dataclass
class WindowAssignment:
    """One field assigned to a time window in a specific set (A/B/C/D)."""

    category: str           # A, B, C, or D
    field_id: int
    cadence: str            # daily or sparse4
    start_day: int
    end_day: int
    window_index: int       # 45-day window or 135-day span index
    group_id: Optional[int] = None  # Set A group (1..6) or B field id
    notes: str = ""         # e.g. B_45, B_90


@dataclass
class DailyObservation:
    """One target observation on a campaign day and LCS slot."""

    day: int
    slot_index: int         # 1..11 within the daily LCS window
    field_id: int
    category: str
    cadence: str


@dataclass
class SolverResult:
    """Complete output from one CP-SAT solve run."""

    status: str             # OPTIMAL, FEASIBLE, INFEASIBLE, ...
    objective_value: Optional[float]
    wall_time_seconds: float
    window_assignments: List[WindowAssignment]
    daily_observations: List[DailyObservation]
    config: SolverConfig
    fields_df: pd.DataFrame
    feasibility: FeasibilityMaps
