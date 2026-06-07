# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/solver.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : CP-SAT model builder and solver for LCS scheduling
# ***************************************************************************

"""CP-SAT model builder and solver for LCS scheduling."""

from __future__ import annotations

import time
from collections import defaultdict
from dataclasses import replace
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd
from ortools.sat.python import cp_model

from .feasibility import build_windows_45
from .models import (
    DailyObservation,
    FeasibilityMaps,
    SolverConfig,
    SolverResult,
    WindowAssignment,
)


def _extinction_score(fields_df: pd.DataFrame, config: SolverConfig) -> Dict[int, int]:
    """
    Integer score per field from extinction headroom (lower A_U is better).

    :param fields_df: must contain field_id and A_U columns
    :param config: max_extinction threshold
    :return: field_id -> scaled score for objective
    """
    scores = {}
    for _, row in fields_df.iterrows():
        fid = int(row["field_id"])
        au = float(row["A_U"])
        delta = max(0.0, config.max_extinction - au)  # more headroom -> higher score
        scores[fid] = int(round(delta * 100))
    return scores


def _d_rank_score(config: SolverConfig) -> Dict[int, int]:
    """
    Rank-based score for Set D fields (earlier in d_ranked_fields is better).

    :param config: d_ranked_fields list and weight_d_rank
    :return: field_id -> score for objective
    """
    n = len(config.d_ranked_fields)
    return {
        fid: (n - idx) * config.weight_d_rank
        for idx, fid in enumerate(config.d_ranked_fields)
    }


def _sparse_days_in_window(
    start_day: int, end_day: int, phase: int, cadence: int
) -> Set[int]:
    """
    Days within [start_day, end_day] matching sparse cadence (every N days).

    :param phase: offset within the cadence cycle (mod cadence)
    :param cadence: e.g. 4 for one observation every 4 days
    :return: set of campaign days
    """
    days = set()
    for day in range(start_day, end_day + 1):
        if (day - start_day) % cadence == phase % cadence:
            days.add(day)
    return days


def _daily_days_in_window(start_day: int, end_day: int) -> Set[int]:
    """All campaign days in a closed interval."""
    return set(range(start_day, end_day + 1))


def build_and_solve(
    fields_df: pd.DataFrame,
    feasibility: FeasibilityMaps,
    config: SolverConfig,
) -> SolverResult:
    """
    Build the CP-SAT model, solve, and return structured results.

    Sets A/B/C/D are encoded as Boolean variables with set-specific constraints.
    Set A uses a 6×8 group×slot grid (v3 layout): exactly one field per cell.

    :param fields_df: field catalog with extinction values
    :param feasibility: precomputed feasible (field, window) pairs
    :param config: campaign and solver parameters
    :return: SolverResult with assignments, daily obs, and status
    """
    # Plan spans 8×45 = 360 days; extend capacity horizon if MATLAB exported less
    plan_last_day = (
        config.first_day + config.set_a_fields_per_group * config.min_window_days - 1
    )
    if config.capacity_last_day < plan_last_day:
        config = replace(config, capacity_last_day=plan_last_day)

    model = cp_model.CpModel()
    windows_45 = feasibility.windows_45
    windows_135 = feasibility.windows_135

    field_ids = fields_df["field_id"].astype(int).tolist()
    extinction_scores = _extinction_score(fields_df, config)
    d_scores = _d_rank_score(config)

    # ---- Set A: 6 groups × 8 slots (v3 layout), one field per (group, slot) ----
    n_groups = config.set_a_n_groups
    n_slots = config.set_a_fields_per_group
    a_vars: Dict[Tuple[int, int, int], cp_model.IntVar] = {}
    for f, wins in feasibility.feasible_a.items():
        for g in range(1, n_groups + 1):
            for s in range(1, n_slots + 1):
                if s in wins:  # field f can occupy slot s in group g
                    a_vars[(f, g, s)] = model.NewBoolVar(f"a_{f}_{g}_{s}")

    # ---- Set B: field selected once; each selected field gets 1 daily + 2 sparse windows ----
    b_sel: Dict[int, cp_model.IntVar] = {}
    b_daily: Dict[Tuple[int, int], cp_model.IntVar] = {}
    b_sparse: Dict[Tuple[int, int], cp_model.IntVar] = {}
    for f, wins in feasibility.feasible_b.items():
        if len(wins) < 3:  # B needs at least 3 windows (1 daily + 2 sparse)
            continue
        b_sel[f] = model.NewBoolVar(f"b_sel_{f}")
        for w in wins:
            b_daily[(f, w)] = model.NewBoolVar(f"b_daily_{f}_{w}")
            b_sparse[(f, w)] = model.NewBoolVar(f"b_sparse_{f}_{w}")

    # ---- Set C: one 135-day sparse window per selected field ----
    c_vars: Dict[Tuple[int, int], cp_model.IntVar] = {}
    for f, wins in feasibility.feasible_c.items():
        for w135 in wins:
            c_vars[(f, w135)] = model.NewBoolVar(f"c_{f}_{w135}")

    # ---- Set D: one 45-day daily window per selected field ----
    d_vars: Dict[Tuple[int, int], cp_model.IntVar] = {}
    for f, wins in feasibility.feasible_d.items():
        for w in wins:
            d_vars[(f, w)] = model.NewBoolVar(f"d_{f}_{w}")

    # ---- Constraint 1: each field appears in at most one set ----
    for f in field_ids:
        terms = []
        terms.extend(
            a_vars[(f, g, s)]
            for g in range(1, n_groups + 1)
            for s in range(1, n_slots + 1)
            if (f, g, s) in a_vars
        )
        if f in b_sel:
            terms.append(b_sel[f])
        terms.extend(c_vars[(f, w)] for w in feasibility.feasible_c.get(f, []) if (f, w) in c_vars)
        terms.extend(d_vars[(f, w)] for w in feasibility.feasible_d.get(f, []) if (f, w) in d_vars)
        if terms:
            model.Add(sum(terms) <= 1)

    # ---- Set A: exactly one field per (group, slot) — the v3 grouping constraint ----
    for g in range(1, n_groups + 1):
        for s in range(1, n_slots + 1):
            slot_terms = [
                a_vars[(f, g, s)]
                for f in feasibility.feasible_a
                if (f, g, s) in a_vars
            ]
            if slot_terms:
                model.Add(sum(slot_terms) == 1)
            else:
                model.Add(0 == 1)  # infeasible slot — no eligible field

    # ---- Set counts: A is implied by group×slot; B/C/D are explicit sums ----
    if a_vars:
        model.Add(sum(a_vars.values()) == config.set_a_count)
    else:
        model.Add(0 == config.set_a_count)

    if b_sel:
        model.Add(sum(b_sel.values()) == config.set_b_count)
    else:
        model.Add(0 == config.set_b_count)

    if c_vars:
        model.Add(sum(c_vars.values()) == config.set_c_count)
    else:
        model.Add(0 == config.set_c_count)

    if d_vars:
        model.Add(sum(d_vars.values()) == config.set_d_count)
    else:
        model.Add(0 == config.set_d_count)

    # ---- Set B structure: 1 daily window + 2 sparse windows per selected field ----
    for f, sel in b_sel.items():
        wins = feasibility.feasible_b[f]
        daily_terms = [b_daily[(f, w)] for w in wins if (f, w) in b_daily]
        sparse_terms = [b_sparse[(f, w)] for w in wins if (f, w) in b_sparse]
        model.Add(sum(daily_terms) == sel)       # exactly 1 daily if selected
        model.Add(sum(sparse_terms) == 2 * sel)  # exactly 2 sparse if selected
        for w in wins:
            if (f, w) in b_daily:
                model.Add(b_daily[(f, w)] + b_sparse[(f, w)] <= 1)  # not both on same window

        # Optional: force 3 consecutive windows when selected (v3 consecutive mode)
        if config.require_b_consecutive:
            triple_vars = []
            for k in range(1, len(windows_45) - 1):
                if all(w in wins for w in (k, k + 1, k + 2)):
                    tv = model.NewBoolVar(f"b_triple_{f}_{k}")
                    triple_vars.append(tv)
                    model.Add(b_daily[(f, k)] + b_daily[(f, k + 1)] + b_daily[(f, k + 2)] >= tv)
                    model.Add(b_sparse[(f, k)] + b_sparse[(f, k + 1)] + b_sparse[(f, k + 2)] >= 2 * tv)
                    for w in (k, k + 1, k + 2):
                        model.Add(b_daily[(f, w)] + b_sparse[(f, w)] >= tv)
            if triple_vars:
                model.Add(sum(triple_vars) == sel)

    # ---- Daily capacity: count active fields per campaign day, cap at daily_capacity ----
    day_vars: Dict[int, List[cp_model.IntVar]] = defaultdict(list)

    # Set A: daily cadence across the slot's 45-day window
    for (f, g, s), var in a_vars.items():
        win = windows_45[s - 1]
        for day in _daily_days_in_window(win.start_day, win.end_day):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for (f, w), var in d_vars.items():
        win = windows_45[w - 1]
        for day in _daily_days_in_window(win.start_day, win.end_day):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for (f, w), var in b_daily.items():
        win = windows_45[w - 1]
        for day in _daily_days_in_window(win.start_day, win.end_day):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for (f, w), var in b_sparse.items():
        win = windows_45[w - 1]
        for day in _sparse_days_in_window(
            win.start_day, win.end_day, config.sparse_phase, config.sparse_cadence
        ):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    span_by_idx = {idx: (start, end) for idx, start, end in windows_135}
    for (f, w135), var in c_vars.items():
        start, end = span_by_idx[w135]
        for day in _sparse_days_in_window(start, end, config.sparse_phase, config.sparse_cadence):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for day, vars_on_day in day_vars.items():
        if vars_on_day:
            model.Add(sum(vars_on_day) <= config.daily_capacity)

    # ---- Objective: maximize slack, extinction quality, and D rank ----
    objective_terms = []

    for (f, g, s), var in a_vars.items():
        slack = feasibility.slack_45.get((f, s), 0)
        obj = slack * config.weight_slack + extinction_scores.get(f, 0) * config.weight_extinction
        if obj:
            objective_terms.append(var * obj)

    for (f, w135), var in c_vars.items():
        slack = feasibility.slack_135.get((f, w135), 0)
        obj = slack * config.weight_slack + extinction_scores.get(f, 0) * config.weight_extinction
        if obj:
            objective_terms.append(var * obj)

    for f, sel in b_sel.items():
        obj = extinction_scores.get(f, 0) * config.weight_extinction
        if obj:
            objective_terms.append(sel * obj)
        for w in feasibility.feasible_b.get(f, []):
            slack = feasibility.slack_45.get((f, w), 0)
            if slack and (f, w) in b_daily:
                objective_terms.append(b_daily[(f, w)] * slack * config.weight_slack)
            if slack and (f, w) in b_sparse:
                objective_terms.append(b_sparse[(f, w)] * slack * config.weight_slack)

    for (f, w), var in d_vars.items():
        objective_terms.append(var * d_scores.get(f, 0))

    if objective_terms:
        model.Maximize(sum(objective_terms))

    # ---- Solve ----
    solver = cp_model.CpSolver()
    solver.parameters.max_time_in_seconds = config.time_limit_seconds
    t0 = time.time()
    status = solver.Solve(model)
    wall_time = time.time() - t0

    status_name = solver.StatusName(status)
    objective_value = solver.ObjectiveValue() if status in (
        cp_model.OPTIMAL,
        cp_model.FEASIBLE,
    ) else None

    window_assignments, daily_observations = _extract_solution(
        solver,
        status,
        a_vars,
        b_sel,
        b_daily,
        b_sparse,
        c_vars,
        d_vars,
        windows_45,
        windows_135,
        config,
    )

    return SolverResult(
        status=status_name,
        objective_value=objective_value,
        wall_time_seconds=wall_time,
        window_assignments=window_assignments,
        daily_observations=daily_observations,
        config=config,
        fields_df=fields_df,
        feasibility=feasibility,
    )


def _extract_solution(
    solver: cp_model.CpSolver,
    status: int,
    a_vars,
    b_sel,
    b_daily,
    b_sparse,
    c_vars,
    d_vars,
    windows_45,
    windows_135,
    config: SolverConfig,
) -> Tuple[List[WindowAssignment], List[DailyObservation]]:
    """
    Read Boolean variable values from the solved model into assignment records.

    :param solver: CP-SAT solver after Solve()
    :param status: OR-Tools status code
    :return: (window_assignments, daily_observations)
    """
    if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        return [], []

    assignments: List[WindowAssignment] = []
    span_by_idx = {idx: (start, end) for idx, start, end in windows_135}

    for (f, g, s), var in a_vars.items():
        if solver.Value(var):
            win = windows_45[s - 1]
            assignments.append(
                WindowAssignment(
                    category="A",
                    field_id=f,
                    cadence="daily",
                    start_day=win.start_day,
                    end_day=win.end_day,
                    window_index=s,
                    group_id=g,
                )
            )

    for f, sel in b_sel.items():
        if not solver.Value(sel):
            continue
        group_id = f
        for w in sorted({k[1] for k in b_daily if k[0] == f}):
            if solver.Value(b_daily[(f, w)]):
                win = windows_45[w - 1]
                assignments.append(
                    WindowAssignment(
                        category="B",
                        field_id=f,
                        cadence="daily",
                        start_day=win.start_day,
                        end_day=win.end_day,
                        window_index=w,
                        group_id=group_id,
                        notes="B_45",
                    )
                )
            if solver.Value(b_sparse[(f, w)]):
                win = windows_45[w - 1]
                assignments.append(
                    WindowAssignment(
                        category="B",
                        field_id=f,
                        cadence="sparse4",
                        start_day=win.start_day,
                        end_day=win.end_day,
                        window_index=w,
                        group_id=group_id,
                        notes="B_90",
                    )
                )

    for (f, w135), var in c_vars.items():
        if solver.Value(var):
            start, end = span_by_idx[w135]
            assignments.append(
                WindowAssignment(
                    category="C",
                    field_id=f,
                    cadence="sparse4",
                    start_day=start,
                    end_day=end,
                    window_index=w135,
                )
            )

    for (f, w), var in d_vars.items():
        if solver.Value(var):
            win = windows_45[w - 1]
            assignments.append(
                WindowAssignment(
                    category="D",
                    field_id=f,
                    cadence="daily",
                    start_day=win.start_day,
                    end_day=win.end_day,
                    window_index=w,
                )
            )

    daily_observations = build_daily_observations(assignments, config)
    return assignments, daily_observations


def build_daily_observations(
    assignments: List[WindowAssignment],
    config: SolverConfig,
) -> List[DailyObservation]:
    """
    Expand window assignments into per-day slot observations.

    Each active field on a day gets a slot_index (1..N) in encounter order.

    :param assignments: solved window assignments
    :param config: sparse cadence and capacity_last_day
    :return: flat list of DailyObservation records
    """
    day_fields: Dict[int, List[Tuple[int, str, str]]] = defaultdict(list)

    for item in assignments:
        if item.cadence == "daily":
            days = _daily_days_in_window(item.start_day, item.end_day)
        else:
            days = _sparse_days_in_window(
                item.start_day,
                item.end_day,
                config.sparse_phase,
                config.sparse_cadence,
            )
        for day in sorted(days):
            if day <= config.capacity_last_day:
                day_fields[day].append((item.field_id, item.category, item.cadence))

    observations: List[DailyObservation] = []
    for day in sorted(day_fields):
        entries = day_fields[day]
        for slot_index, (field_id, category, cadence) in enumerate(entries, start=1):
            observations.append(
                DailyObservation(
                    day=day,
                    slot_index=slot_index,
                    field_id=field_id,
                    category=category,
                    cadence=cadence,
                )
            )
    return observations
