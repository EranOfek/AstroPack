"""Data models for the LCS CP-SAT solver."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd


@dataclass
class WindowDef:
    """A fixed 45-day scheduling window."""

    index: int
    start_day: int
    end_day: int

    @property
    def length(self) -> int:
        return self.end_day - self.start_day + 1


@dataclass
class SolverConfig:
    """Configuration for the LCS CP-SAT solver."""

    first_day: int = 1
    last_day: int = 420
    min_window_days: int = 45
    long_window_days: int = 135
    daily_capacity: int = 11
    set_a_count: int = 48
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
    capacity_last_day: int = 360
    time_limit_seconds: int = 300
    start_date: str = "2029-01-01"
    daily_window_start_time_seconds: float = 0.0
    slot_time_days: float = 3 * 300 / 86400
    weight_d_rank: int = 1000
    weight_slack: int = 10
    weight_extinction: int = 1
    num_windows_45: int = 8

    @classmethod
    def from_dict(cls, data: dict) -> "SolverConfig":
        """Build config from a JSON/dict, mapping MATLAB export keys when present."""
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
        return cls(**kwargs)


@dataclass
class FeasibilityMaps:
    """Precomputed feasible assignments and slack values."""

    windows_45: List[WindowDef]
    windows_135: List[Tuple[int, int, int]]  # (start_idx, end_idx, start_day, end_day) simplified
    feasible_a: Dict[int, Set[int]]
    feasible_b: Dict[int, Set[int]]
    feasible_c: Dict[int, Set[int]]
    feasible_d: Dict[int, Set[int]]
    slack_45: Dict[Tuple[int, int], int]
    slack_135: Dict[Tuple[int, int], int]
    eligible_abc: Set[int]
    eligible_long: Set[int]
    eligible_d: Set[int]


@dataclass
class WindowAssignment:
    """One scheduled field window."""

    category: str
    field_id: int
    cadence: str
    start_day: int
    end_day: int
    window_index: int
    group_id: Optional[int] = None
    notes: str = ""


@dataclass
class DailyObservation:
    """One observation on a specific day and slot."""

    day: int
    slot_index: int
    field_id: int
    category: str
    cadence: str


@dataclass
class SolverResult:
    """Output of the CP-SAT solve step."""

    status: str
    objective_value: Optional[float]
    wall_time_seconds: float
    window_assignments: List[WindowAssignment]
    daily_observations: List[DailyObservation]
    config: SolverConfig
    fields_df: pd.DataFrame
    feasibility: FeasibilityMaps
