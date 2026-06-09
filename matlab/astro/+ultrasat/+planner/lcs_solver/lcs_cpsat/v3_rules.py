# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/v3_rules.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : LcsHelper_v3 scheduling rules (geometry, division table, capacity)
# ***************************************************************************

"""LcsHelper_v3 scheduling rules shared by solver and validation.

This file keeps the MATLAB v3 scheduling arithmetic in small Python helpers.
The solver uses these helpers while building constraints, and validation uses
the same helpers to re-check the result after solving.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple

from .models import SolverConfig, WindowAssignment, WindowDef


# Set B division table from LcsHelper_v3.local_setB_division (SetC_start_ind == 3)
# Each row says where one Set B field gets its one daily 45-day block and its
# two sparse 45-day blocks.  The table is mirrored when SetC_start_ind == 1.
SETB_W45_BASE = [1, 1, 1, 1, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8]
SETB_W90_1_BASE = [2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 7, 7, 6, 6, 6, 6]
SETB_W90_2_BASE = [3, 3, 3, 3, 4, 4, 2, 2, 4, 4, 8, 8, 8, 8, 7, 7]


@dataclass(frozen=True)
class SetBDivisionRow:
    """One preset Set B window triple (W45, W90_1, W90_2)."""

    row_index: int
    w45: int
    w90_1: int
    w90_2: int


@dataclass(frozen=True)
class SetCSuperWindow:
    """One fixed 135-day Set C super-window."""

    index: int
    start_day: int
    end_day: int


def set_b_division_table(set_c_start_ind: int) -> List[SetBDivisionRow]:
    """
    Return 16 Set B division rows for the given SetC_start_ind.

    :param set_c_start_ind: 3 (base) or 1 (mirrored via 9-x)
    :return: list of SetBDivisionRow
    """
    if set_c_start_ind == 3:
        w45, w90_1, w90_2 = SETB_W45_BASE, SETB_W90_1_BASE, SETB_W90_2_BASE
    elif set_c_start_ind == 1:
        # MATLAB mirrors the base table across the eight 45-day windows using
        # 9 - window_index, so W1 becomes W8, W2 becomes W7, and so on.
        w45 = [9 - x for x in SETB_W45_BASE]
        w90_1 = [9 - x for x in SETB_W90_1_BASE]
        w90_2 = [9 - x for x in SETB_W90_2_BASE]
    else:
        raise ValueError(f"unsupported set_c_start_ind={set_c_start_ind}")
    return [
        SetBDivisionRow(i + 1, w45[i], w90_1[i], w90_2[i])
        for i in range(16)
    ]


def set_a_group_anchors(config: SolverConfig) -> List[int]:
    """
    Per-group anchor days for Set A (LcsHelper_v3 schedule_SetA_v3 phase 2).

    All groups start at first_day except one optional shifted group.
    """
    ref = config.first_day
    anchors = [ref] * config.set_a_n_groups
    sg = config.set_a_shifted_group
    sh = config.set_a_shift_days
    if 1 <= sg <= config.set_a_n_groups and sh != 0:
        # Only one Set A group is shifted in a branch.  The outer solver loop
        # tries possible shifts when the no-shift geometry is infeasible.
        anchors[sg - 1] = ref + sh
    return anchors


def set_a_slot_calendar(
    config: SolverConfig,
    group: int,
    slot: int,
    windows_45: Optional[List] = None,
) -> Tuple[int, int, int]:
    """
    Calendar interval and slot index for Set A group g, slot s.

    Uses per-group anchors (v3 phase-2 rescue). Capacity counts by slot index.

    :return: (start_day, end_day, window_index)
    """
    del windows_45  # slot index is the v3 ind; calendar comes from anchors
    l_win = config.min_window_days
    anchors = set_a_group_anchors(config)
    start = anchors[group - 1] + (slot - 1) * l_win
    end = start + l_win - 1
    return start, end, slot


def set_c_super_windows(
    config: SolverConfig,
    windows_45: List[WindowDef],
) -> List[SetCSuperWindow]:
    """
    Two fixed 135-day super-windows anchored at Full_windows.start(sci).

    :param config: set_c_start_ind and long_window_days
    :param windows_45: fixed 45-day window grid
    :return: exactly two SetCSuperWindow records
    """
    sci = config.set_c_start_ind
    if sci < 1 or sci > len(windows_45):
        raise ValueError(f"set_c_start_ind={sci} out of range")
    l_super = config.long_window_days
    s1 = windows_45[sci - 1].start_day
    s2 = s1 + l_super
    return [
        SetCSuperWindow(1, s1, s1 + l_super - 1),
        SetCSuperWindow(2, s2, s2 + l_super - 1),
    ]


def sparse_days_for_ind(
    start_day: int, end_day: int, cadence_ind: int, cadence: int = 4
) -> Set[int]:
    """
    Sparse observation days using v3 mod(ind, cadence) phase rule.

    Day d is observed when (d - start_day + 1) % cadence == cadence_ind % cadence.
    """
    phase = cadence_ind % cadence
    days: Set[int] = set()
    for day in range(start_day, end_day + 1):
        # This is the exact v3 sparse cadence rule.  It is used for B_90 and C
        # when expanding compact window assignments into daily observations.
        if (day - start_day + 1) % cadence == phase:
            days.add(day)
    return days


def compute_window_occupancy(
    assignments: List[WindowAssignment],
    config: SolverConfig,
    windows_45: List[WindowDef],
    include_d: bool = False,
) -> Tuple[List[int], List[int], List[int], List[int], List[int], bool]:
    """
    Compute per-window-index occupancy (v3 local_compute_slot_occupancy).

    :return: (nA, nB45, nB90, nC, filled, divisibility_ok)
    """
    n_inds = len(windows_45)
    n_a = [0] * n_inds
    n_b45 = [0] * n_inds
    n_b90 = [0] * n_inds
    n_c = [0] * n_inds

    start_to_ind = {w.start_day: w.index for w in windows_45}

    for item in assignments:
        # Count each selected assignment in the 45-day index where it consumes
        # capacity.  Long Set C rows cover three consecutive 45-day indices.
        if item.category == "A":
            k = item.window_index
            if 1 <= k <= n_inds:
                n_a[k - 1] += 1
        elif item.category == "B":
            if item.notes == "B_45" or item.cadence == "daily":
                k = item.window_index
                if 1 <= k <= n_inds:
                    n_b45[k - 1] += 1
            else:
                k = item.window_index
                if 1 <= k <= n_inds:
                    n_b90[k - 1] += 1
        elif item.category == "C":
            start_ind = start_to_ind.get(item.start_day)
            if start_ind is not None:
                for kk in range(start_ind, min(start_ind + 3, n_inds + 1)):
                    n_c[kk - 1] += 1
        elif item.category == "D" and include_d:
            k = item.window_index
            if 1 <= k <= n_inds:
                n_b45[k - 1] += 1  # D counts as daily slot at ind k

    n4 = [n_b90[i] + n_c[i] for i in range(n_inds)]
    divisibility_ok = all(v % 4 == 0 for v in n4)
    # Sparse rows are interleaved across four cadence phases, so four sparse
    # rows consume one daily slot in the v3 filled(k) accounting.
    filled = [
        n_a[i] + n_b45[i] + n4[i] / 4.0
        for i in range(n_inds)
    ]
    return n_a, n_b45, n_b90, n_c, filled, divisibility_ok


def compute_inds_open(
    assignments: List[WindowAssignment],
    config: SolverConfig,
    windows_45: List[WindowDef],
) -> List[int]:
    """
    Window indices with spare daily capacity after ABC (v3 inds_open).

    Each open slot is one window index k where another daily field fits.
    Set D is excluded from occupancy (placed into open slack).
    """
    _, _, _, _, filled, ok = compute_window_occupancy(
        assignments, config, windows_45, include_d=False
    )
    if not ok:
        return []
    inds_open: List[int] = []
    cap = config.daily_capacity
    for k, load in enumerate(filled, start=1):
        if load < cap:
            inds_open.extend([k] * int(cap - load))
    return inds_open
