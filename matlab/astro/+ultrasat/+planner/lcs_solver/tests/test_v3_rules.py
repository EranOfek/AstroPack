# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/tests/test_v3_rules.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Unit tests for v3 scheduling rules
# ***************************************************************************

"""Tests for v3_rules helpers."""

from __future__ import annotations

from lcs_cpsat.feasibility import build_windows_45
from lcs_cpsat.models import SolverConfig, WindowAssignment
from lcs_cpsat.v3_rules import (
    compute_window_occupancy,
    set_a_slot_calendar,
    set_b_division_table,
    sparse_days_for_ind,
)


def test_set_a_shared_windows():
    config = SolverConfig(
        first_day=1, set_a_n_groups=6, set_a_fields_per_group=8,
        set_a_shifted_group=1, set_a_shift_days=1,
    )
    s1, _, _ = set_a_slot_calendar(config, 1, 1)
    s2, _, _ = set_a_slot_calendar(config, 2, 1)
    assert s1 == 2
    assert s2 == 1


def test_set_b_division_mirror():
    base = set_b_division_table(3)
    mirror = set_b_division_table(1)
    assert base[0].w45 == 1
    assert mirror[0].w45 == 8


def test_sparse_days_mod_ind():
    days = sparse_days_for_ind(1, 12, cadence_ind=2, cadence=4)
    assert 2 in days
    assert 6 in days
    assert 1 not in days


def test_window_occupancy_filled():
    config = SolverConfig(daily_capacity=11)
    windows = build_windows_45(config)
    assignments = [
        WindowAssignment("A", 1, "daily", 1, 45, 1, group_id=1),
        WindowAssignment("B", 2, "daily", 1, 45, 1, group_id=101, notes="B_45"),
        WindowAssignment("B", 2, "sparse4", 1, 45, 1, group_id=201, notes="B_90", cadence_ind=1),
        WindowAssignment("B", 2, "sparse4", 46, 90, 2, group_id=202, notes="B_90", cadence_ind=2),
        WindowAssignment("B", 2, "sparse4", 91, 135, 3, group_id=203, notes="B_90", cadence_ind=3),
    ]
    n_a, n_b45, n_b90, n_c, filled, ok = compute_window_occupancy(
        assignments, config, windows
    )
    assert n_a[0] == 1
    assert n_b45[0] == 1
    assert filled[0] == 2.25  # one B_90 at ind 1 -> n4/4 = 0.25
    assert not ok  # n4=1 at ind 1 is not divisible by 4
