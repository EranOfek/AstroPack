# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/feasibility.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Precompute feasible field-window assignments for LCS CP-SAT
# ***************************************************************************

"""Precompute feasible field-window assignments."""

from __future__ import annotations

from typing import Dict, List, Optional, Set, Tuple

import pandas as pd

from .models import FeasibilityMaps, SolverConfig, WindowDef
from .v3_rules import set_a_slot_calendar, set_c_super_windows


def build_windows_45(config: SolverConfig) -> List[WindowDef]:
    """
    Build the fixed 45-day windows W1..Wn on the campaign timeline.

    :param config: solver config with first_day, min_window_days, num_windows_45
    :return: list of WindowDef, e.g. W1=[1,45], W2=[46,90], ...
    """
    windows = []
    for idx in range(1, config.num_windows_45 + 1):
        start = config.first_day + (idx - 1) * config.min_window_days
        end = start + config.min_window_days - 1
        windows.append(WindowDef(index=idx, start_day=start, end_day=end))
    return windows


def build_windows_135(
    windows_45: List[WindowDef],
) -> List[Tuple[int, int, int]]:
    """
    Build overlapping 135-day spans from three consecutive 45-day windows.

    :param windows_45: list of 45-day windows
    :return: list of (span_index, start_day, end_day), e.g. W1-W3, W2-W4, ...
    """
    spans = []
    for i in range(len(windows_45) - 2):
        w0 = windows_45[i]
        w2 = windows_45[i + 2]
        spans.append((i + 1, w0.start_day, w2.end_day))
    return spans


def _covers_interval(
    vis_start: int, vis_end: int, req_start: int, req_end: int
) -> bool:
    """True if visibility interval fully covers the required interval."""
    return vis_start <= req_start and vis_end >= req_end


def _slack_for_interval(
    vis_start: int, vis_end: int, req_start: int, req_end: int
) -> int:
    """Extra visibility days beyond the required interval (both sides)."""
    return (req_start - vis_start) + (vis_end - req_end)


def _feasible_windows_for_field(
    field_id: int,
    windows_df: pd.DataFrame,
    req_intervals: List[Tuple[int, int, int]],
) -> Tuple[Dict[int, int], Set[int]]:
    """
    Find which required intervals a field can cover.

    :param field_id: sky field index
    :param windows_df: visibility windows from MATLAB
    :param req_intervals: list of (index, req_start, req_end)
    :return: (slack_by_index, feasible_index_set)
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


def _windows_df_for_field(
    field_id: int,
    windows_strict: pd.DataFrame,
    windows_1dgap: Optional[pd.DataFrame],
    use1dgap: Dict[int, bool],
) -> pd.DataFrame:
    """Pick strict or 1-day-gap visibility table per field (v3 use1gap)."""
    if use1dgap.get(field_id, False) and windows_1dgap is not None:
        return windows_1dgap[windows_1dgap["field_id"] == field_id]
    return windows_strict[windows_strict["field_id"] == field_id]


def _feasible_calendar_interval(
    field_id: int,
    req_start: int,
    req_end: int,
    windows_strict: pd.DataFrame,
    windows_1dgap: Optional[pd.DataFrame],
    use1dgap: Dict[int, bool],
) -> Optional[int]:
    """Return slack if field covers [req_start, req_end], else None."""
    field_rows = _windows_df_for_field(
        field_id, windows_strict, windows_1dgap, use1dgap
    )
    best = None
    for _, row in field_rows.iterrows():
        vs = int(row["vis_start_day"])
        ve = int(row["vis_end_day"])
        if _covers_interval(vs, ve, req_start, req_end):
            s = _slack_for_interval(vs, ve, req_start, req_end)
            if best is None or s > best:
                best = s
    return best


def compute_feasibility(
    fields_df: pd.DataFrame,
    windows_df: pd.DataFrame,
    eligibility_df: pd.DataFrame,
    config: SolverConfig,
    windows_1dgap_df: Optional[pd.DataFrame] = None,
) -> FeasibilityMaps:
    """
    Precompute all feasible (field, window) pairs before building the CP-SAT model.

    :param fields_df: field catalog
    :param windows_df: strict continuous visibility windows
    :param eligibility_df: eligibility flags (+ optional use1dgap)
    :param config: solver configuration
    :param windows_1dgap_df: optional 1-day-gap merged windows
    :return: FeasibilityMaps used by solver.py
    """
    windows_45 = build_windows_45(config)
    windows_135 = build_windows_135(windows_45)
    super_windows = set_c_super_windows(config, windows_45)

    req_45 = [(w.index, w.start_day, w.end_day) for w in windows_45]
    req_135 = [
        (sw.index, sw.start_day, sw.end_day) for sw in super_windows
    ]

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

    use1dgap: Dict[int, bool] = {}
    if "use1dgap" in eligibility_df.columns:
        for _, row in eligibility_df.iterrows():
            fid = int(row["field_id"])
            use1dgap[fid] = bool(int(row["use1dgap"]))
    elif "max_window_1dgap_days" in eligibility_df.columns:
        for _, row in eligibility_df.iterrows():
            fid = int(row["field_id"])
            strict = int(row.get("max_window_days", 0))
            gap = int(row["max_window_1dgap_days"])
            use1dgap[fid] = gap > strict

    feasible_a: Dict[int, Set[int]] = {}
    feasible_b: Dict[int, Set[int]] = {}
    feasible_c: Dict[int, Set[int]] = {}
    feasible_d: Dict[int, Set[int]] = {}
    feasible_a_gs: Dict[Tuple[int, int, int], bool] = {}
    slack_a_gs: Dict[Tuple[int, int, int], int] = {}
    slack_45: Dict[Tuple[int, int], int] = {}
    slack_135: Dict[Tuple[int, int], int] = {}

    field_ids = fields_df["field_id"].astype(int).tolist()
    n_groups = config.set_a_n_groups
    n_slots = config.set_a_fields_per_group

    for field_id in field_ids:
        wdf = _windows_df_for_field(
            field_id, windows_df, windows_1dgap_df, use1dgap
        )
        s45, f45 = _feasible_windows_for_field(
            field_id,
            wdf if not wdf.empty else windows_df[windows_df["field_id"] == field_id],
            req_45,
        )
        s135, f135 = _feasible_windows_for_field(
            field_id,
            wdf if not wdf.empty else windows_df[windows_df["field_id"] == field_id],
            req_135,
        )

        if field_id in eligible_abc:
            feasible_a[field_id] = f45
            feasible_b[field_id] = f45
            for w_idx, val in s45.items():
                slack_45[(field_id, w_idx)] = val

            for g in range(1, n_groups + 1):
                for s in range(1, n_slots + 1):
                    start, end, _ = set_a_slot_calendar(
                        config, g, s, windows_45
                    )
                    slack = _feasible_calendar_interval(
                        field_id, start, end,
                        windows_df, windows_1dgap_df, use1dgap,
                    )
                    if slack is not None:
                        feasible_a_gs[(field_id, g, s)] = True
                        slack_a_gs[(field_id, g, s)] = slack

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
        use1dgap=use1dgap,
        feasible_a_gs=feasible_a_gs,
        slack_a_gs=slack_a_gs,
    )
