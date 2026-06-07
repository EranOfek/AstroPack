"""Precompute feasible field-window assignments."""

from __future__ import annotations

from typing import Dict, List, Set, Tuple

import pandas as pd

from .models import FeasibilityMaps, SolverConfig, WindowDef


def build_windows_45(config: SolverConfig) -> List[WindowDef]:
    """Build fixed 45-day windows starting at first_day."""
    windows = []
    for idx in range(1, config.num_windows_45 + 1):
        start = config.first_day + (idx - 1) * config.min_window_days
        end = start + config.min_window_days - 1
        windows.append(WindowDef(index=idx, start_day=start, end_day=end))
    return windows


def build_windows_135(
    windows_45: List[WindowDef],
) -> List[Tuple[int, int, int]]:
    """Build 135-day spans as (index, start_day, end_day)."""
    spans = []
    for i in range(len(windows_45) - 2):
        w0 = windows_45[i]
        w2 = windows_45[i + 2]
        spans.append((i + 1, w0.start_day, w2.end_day))
    return spans


def _covers_interval(
    vis_start: int, vis_end: int, req_start: int, req_end: int
) -> bool:
    return vis_start <= req_start and vis_end >= req_end


def _slack_for_interval(
    vis_start: int, vis_end: int, req_start: int, req_end: int
) -> int:
    """Extra visibility buffer beyond the required interval on both sides."""
    return (req_start - vis_start) + (vis_end - req_end)


def _feasible_windows_for_field(
    field_id: int,
    windows_df: pd.DataFrame,
    req_intervals: List[Tuple[int, int, int]],
) -> Tuple[Dict[int, int], Set[int]]:
    """
    Return slack per window index and set of feasible window indices.

    req_intervals: list of (index, req_start, req_end)
    """
    field_rows = windows_df[windows_df["field_id"] == field_id]
    slack: Dict[int, int] = {}
    feasible: Set[int] = set()

    for idx, req_start, req_end in req_intervals:
        best_slack = None
        for _, row in field_rows.iterrows():
            vs = int(row["vis_start_day"])
            ve = int(row["vis_end_day"])
            if _covers_interval(vs, ve, req_start, req_end):
                s = _slack_for_interval(vs, ve, req_start, req_end)
                if best_slack is None or s > best_slack:
                    best_slack = s
        if best_slack is not None:
            feasible.add(idx)
            slack[idx] = best_slack
    return slack, feasible


def compute_feasibility(
    fields_df: pd.DataFrame,
    windows_df: pd.DataFrame,
    eligibility_df: pd.DataFrame,
    config: SolverConfig,
) -> FeasibilityMaps:
    """Precompute all feasible (field, window) pairs and slack values."""
    windows_45 = build_windows_45(config)
    windows_135 = build_windows_135(windows_45)

    req_45 = [(w.index, w.start_day, w.end_day) for w in windows_45]
    req_135 = list(windows_135)

    eligible_abc = set(
        eligibility_df.loc[eligibility_df["eligible_abc"] == 1, "field_id"].astype(int)
    )
    eligible_long = set(
        eligibility_df.loc[
            eligibility_df["eligible_long_window"] == 1, "field_id"
        ].astype(int)
    )
    eligible_d = set(
        eligibility_df.loc[eligibility_df["eligible_d"] == 1, "field_id"].astype(int)
    )

    d_ranked = set(config.d_ranked_fields)

    feasible_a: Dict[int, Set[int]] = {}
    feasible_b: Dict[int, Set[int]] = {}
    feasible_c: Dict[int, Set[int]] = {}
    feasible_d: Dict[int, Set[int]] = {}
    slack_45: Dict[Tuple[int, int], int] = {}
    slack_135: Dict[Tuple[int, int], int] = {}

    field_ids = fields_df["field_id"].astype(int).tolist()

    for field_id in field_ids:
        s45, f45 = _feasible_windows_for_field(field_id, windows_df, req_45)
        s135, f135 = _feasible_windows_for_field(field_id, windows_df, req_135)

        if field_id in eligible_abc:
            feasible_a[field_id] = f45
            feasible_b[field_id] = f45
            for w_idx, val in s45.items():
                slack_45[(field_id, w_idx)] = val

        if field_id in eligible_abc and field_id in eligible_long:
            feasible_c[field_id] = f135
            for w_idx, val in s135.items():
                slack_135[(field_id, w_idx)] = val

        if field_id in eligible_d and field_id in d_ranked:
            feasible_d[field_id] = f45

    return FeasibilityMaps(
        windows_45=windows_45,
        windows_135=windows_135,
        feasible_a=feasible_a,
        feasible_b=feasible_b,
        feasible_c=feasible_c,
        feasible_d=feasible_d,
        slack_45=slack_45,
        slack_135=slack_135,
        eligible_abc=eligible_abc,
        eligible_long=eligible_long,
        eligible_d=eligible_d,
    )
