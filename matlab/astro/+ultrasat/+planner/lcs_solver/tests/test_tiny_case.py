# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/tests/test_tiny_case.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : Tiny artificial case smoke test for the LCS CP-SAT solver
# ***************************************************************************

"""Tiny artificial case for smoke testing the CP-SAT solver."""

from __future__ import annotations

import pandas as pd

from lcs_cpsat.feasibility import compute_feasibility
from lcs_cpsat.models import SolverConfig
from lcs_cpsat.solver import build_and_solve
from lcs_cpsat.validation import validate_schedule


def _make_tiny_inputs():
    """
    Minimal problem: 1 Set A group x 6 slots, small B/C/D counts, 360-day horizon.
    """
    fields_df = pd.DataFrame(
        {
            "field_id": list(range(1, 13)),
            "ra": [float(i) for i in range(1, 13)],
            "dec": [0.0] * 12,
            "A_U": [0.5] * 10 + [1.5, 1.6],
        }
    )

    windows_rows = []
    for fid in range(1, 13):
        windows_rows.append(
            {
                "field_id": fid,
                "vis_start_day": 1,
                "vis_end_day": 400,
                "window_len_days": 400,
            }
        )
    windows_df = pd.DataFrame(windows_rows)

    eligibility_df = pd.DataFrame(
        {
            "field_id": list(range(1, 13)),
            "eligible_abc": [1] * 10 + [0, 0],
            "eligible_long_window": [1] * 8 + [0, 0, 0, 0],
            "eligible_d": [0] * 10 + [1, 1],
            "use1dgap": [0] * 12,
        }
    )

    config = SolverConfig(
        first_day=1,
        last_day=360,
        min_window_days=45,
        long_window_days=135,
        daily_capacity=11,
        set_a_count=6,
        set_a_n_groups=1,
        set_a_fields_per_group=6,
        set_b_count=4,
        set_c_count=0,
        set_d_count=1,
        num_windows_45=8,
        capacity_last_day=360,
        d_ranked_fields=[11, 12],
        time_limit_seconds=30,
        set_c_start_ind=1,
        use_set_b_division=False,
        use_window_index_capacity=True,
        solve_set_d_separately=True,
    )
    return fields_df, windows_df, eligibility_df, config


def test_tiny_case_solves():
    fields_df, windows_df, eligibility_df, config = _make_tiny_inputs()
    feasibility = compute_feasibility(fields_df, windows_df, eligibility_df, config)
    result = build_and_solve(fields_df, feasibility, config)

    assert result.status in ("OPTIMAL", "FEASIBLE")
    assert len(result.window_assignments) > 0

    report = validate_schedule(result)
    assert report["passed"].min() == 1

    categories = {a.category for a in result.window_assignments}
    assert "A" in categories
    assert "B" in categories
    assert "D" in categories
