# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/solver.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : CP-SAT model builder and solver for LCS scheduling
# ***************************************************************************

"""CP-SAT model builder and solver for LCS scheduling (LcsHelper_v3-aligned)."""

from __future__ import annotations

import time
from collections import defaultdict
from dataclasses import replace
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd
from ortools.sat.python import cp_model

from .models import (
    DailyObservation,
    FeasibilityMaps,
    SolverConfig,
    SolverResult,
    WindowAssignment,
)
from .v3_rules import (
    compute_window_occupancy,
    compute_inds_open,
    set_a_slot_calendar,
    set_b_division_table,
    set_c_super_windows,
    sparse_days_for_ind,
)


def _extinction_score(fields_df: pd.DataFrame, config: SolverConfig) -> Dict[int, int]:
    scores = {}
    for _, row in fields_df.iterrows():
        fid = int(row["field_id"])
        au = float(row["A_U"])
        delta = max(0.0, config.max_extinction - au)
        scores[fid] = int(round(delta * 100))
    return scores


def _d_rank_score(config: SolverConfig) -> Dict[int, int]:
    n = len(config.d_ranked_fields)
    return {
        fid: (n - idx) * config.weight_d_rank
        for idx, fid in enumerate(config.d_ranked_fields)
    }


def _daily_days_in_window(start_day: int, end_day: int) -> Set[int]:
    return set(range(start_day, end_day + 1))


def _add_window_index_capacity(
    model: cp_model.CpModel,
    config: SolverConfig,
    n_inds: int,
    a_vars: Dict[Tuple[int, int, int], cp_model.IntVar],
    a_moved: Dict[Tuple[int, int], cp_model.IntVar],
    b_daily: Dict[Tuple[int, int], cp_model.IntVar],
    b_sparse: Dict[Tuple[int, int], cp_model.IntVar],
    c_vars: Dict[Tuple[int, int], cp_model.IntVar],
    c_covers: Dict[Tuple[int, int], List[int]],
) -> List[cp_model.IntVar]:
    """
    v3 capacity: filled(k) = nA(k) + nB45(k) + n4(k)/4 <= daily_capacity.

    Integer form: 4*nA + 4*nB45 + n4 <= 4*daily_capacity, with n4 divisible by 4.
    """
    cap = config.daily_capacity
    overflow_vars: List[cp_model.IntVar] = []
    for k in range(1, n_inds + 1):
        n_a = [
            var for (f, g, s), var in a_vars.items() if s == k
        ]
        n_a.extend(var for (f, w), var in a_moved.items() if w == k)
        n_b45 = [var for (f, w), var in b_daily.items() if w == k]
        n_b90 = [var for (f, w), var in b_sparse.items() if w == k]
        n_c = [
            var for (f, sw), var in c_vars.items() if k in c_covers.get((f, sw), [])
        ]
        n4_terms = n_b90 + n_c
        if n_a or n_b45 or n4_terms:
            load4 = model.NewIntVar(0, 4 * (cap + 50), f"load4_{k}")
            overflow4 = model.NewIntVar(0, 4 * 50, f"overflow4_{k}")
            model.Add(load4 == 4 * sum(n_a) + 4 * sum(n_b45) + sum(n4_terms))
            model.Add(overflow4 >= load4 - 4 * cap)
            model.Add(overflow4 >= 0)
            overflow_vars.append(overflow4)
        if n4_terms:
            # n4(k) must be divisible by 4 for sparse interleaving
            n4_sum = model.NewIntVar(0, 200, f"n4_{k}")
            model.Add(n4_sum == sum(n4_terms))
            model.AddModuloEquality(0, n4_sum, 4)
    return overflow_vars


def _build_abc_model(
    fields_df: pd.DataFrame,
    feasibility: FeasibilityMaps,
    config: SolverConfig,
) -> Tuple[
    cp_model.CpModel,
    Dict[Tuple[int, int, int], cp_model.IntVar],
    Dict[Tuple[int, int], cp_model.IntVar],
    Dict[int, cp_model.IntVar],
    Dict[Tuple[int, int], cp_model.IntVar],
    Dict[Tuple[int, int], cp_model.IntVar],
    Dict[Tuple[int, int], cp_model.IntVar],
    Dict[Tuple[int, int], cp_model.IntVar],
    Dict[Tuple[int, int], List[int]],
    List,
    List,
]:
    """Build CP-SAT model for Sets A, B, C (no Set D)."""
    model = cp_model.CpModel()
    windows_45 = feasibility.windows_45
    n_groups = config.set_a_n_groups
    n_slots = config.set_a_fields_per_group
    n_inds = len(windows_45)

    field_ids = fields_df["field_id"].astype(int).tolist()
    extinction_scores = _extinction_score(fields_df, config)
    division = set_b_division_table(config.set_c_start_ind) if config.use_set_b_division else []
    super_windows = set_c_super_windows(config, windows_45)

    # ---- Set A: shared fixed windows; window_index == slot ----
    a_vars: Dict[Tuple[int, int, int], cp_model.IntVar] = {}
    if config.set_a_count > 0:
        for f in feasibility.feasible_a:
            for g in range(1, n_groups + 1):
                for s in range(1, n_slots + 1):
                    if feasibility.feasible_a_gs.get((f, g, s), False):
                        a_vars[(f, g, s)] = model.NewBoolVar(f"a_{f}_{g}_{s}")
    a_moved: Dict[Tuple[int, int], cp_model.IntVar] = {}

    # ---- Set B ----
    b_sel: Dict[int, cp_model.IntVar] = {}
    b_daily: Dict[Tuple[int, int], cp_model.IntVar] = {}
    b_sparse: Dict[Tuple[int, int], cp_model.IntVar] = {}
    b_row: Dict[Tuple[int, int], cp_model.IntVar] = {}

    if config.use_set_b_division and config.set_b_count > 0:
        for f in feasibility.feasible_b:
            row_vars = []
            w45_needed: Set[int] = set()
            w90_needed: Set[int] = set()
            for row in division:
                w45, w91, w92 = row.w45, row.w90_1, row.w90_2
                wins = feasibility.feasible_b[f]
                if w45 in wins and w91 in wins and w92 in wins:
                    b_row[(f, row.row_index)] = model.NewBoolVar(
                        f"b_row_{f}_{row.row_index}"
                    )
                    row_vars.append(b_row[(f, row.row_index)])
                    w45_needed.add(w45)
                    w90_needed.add(w91)
                    w90_needed.add(w92)
            if not row_vars:
                continue
            b_sel[f] = model.NewBoolVar(f"b_sel_{f}")
            for w in w45_needed:
                b_daily[(f, w)] = model.NewBoolVar(f"b_daily_{f}_{w}")
            for w in w90_needed:
                b_sparse[(f, w)] = model.NewBoolVar(f"b_sparse_{f}_{w}")
    elif config.set_b_count > 0:
        for f, wins in feasibility.feasible_b.items():
            if len(wins) < 3:
                continue
            b_sel[f] = model.NewBoolVar(f"b_sel_{f}")
            for w in wins:
                b_daily[(f, w)] = model.NewBoolVar(f"b_daily_{f}_{w}")
                b_sparse[(f, w)] = model.NewBoolVar(f"b_sparse_{f}_{w}")

    # ---- Set C: two fixed super-windows ----
    c_vars: Dict[Tuple[int, int], cp_model.IntVar] = {}
    c_covers: Dict[Tuple[int, int], List[int]] = {}
    start_to_ind = {w.start_day: w.index for w in windows_45}
    valid_super = [sw for sw in super_windows if sw.end_day <= config.last_day]
    overflow_vars: List[cp_model.IntVar] = []

    if config.set_c_count > 0:
        for f in feasibility.feasible_c:
            for sw in valid_super:
                if sw.index in feasibility.feasible_c.get(f, set()):
                    c_vars[(f, sw.index)] = model.NewBoolVar(f"c_{f}_{sw.index}")
                    si = start_to_ind.get(sw.start_day)
                    if si is not None:
                        c_covers[(f, sw.index)] = list(
                            range(si, min(si + 3, n_inds + 1))
                        )
                    else:
                        c_covers[(f, sw.index)] = []

    # ---- Field uniqueness (A/B/C only) ----
    for f in field_ids:
        terms = list(
            a_vars[(f, g, s)]
            for g in range(1, n_groups + 1)
            for s in range(1, n_slots + 1)
            if (f, g, s) in a_vars
        )
        if f in b_sel:
            terms.append(b_sel[f])
        terms.extend(
            c_vars[(f, sw)]
            for sw in {k[1] for k in c_vars if k[0] == f}
            if (f, sw) in c_vars
        )
        if terms:
            model.Add(sum(terms) <= 1)

    # ---- Set A: one field per (group, slot) on shared fixed windows ----
    if config.set_a_count > 0:
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
                    model.Add(0 == 1)

        model.Add(sum(a_vars.values()) == config.set_a_count)

    # ---- Set B counts and structure ----
    if b_sel:
        model.Add(sum(b_sel.values()) == config.set_b_count)

    if config.use_set_b_division:
        for f, sel in b_sel.items():
            rows = [b_row[(f, r.row_index)] for r in division if (f, r.row_index) in b_row]
            model.Add(sum(rows) == sel)
            # Link windows via OR over rows (handles duplicate triples in the table)
            for w in sorted({k[1] for k in b_daily if k[0] == f}):
                w45_rows = [
                    b_row[(f, r.row_index)]
                    for r in division
                    if (f, r.row_index) in b_row and r.w45 == w
                ]
                if w45_rows:
                    model.Add(b_daily[(f, w)] == sum(w45_rows))
            for w in sorted({k[1] for k in b_sparse if k[0] == f}):
                w90_rows = [
                    b_row[(f, r.row_index)]
                    for r in division
                    if (f, r.row_index) in b_row and (r.w90_1 == w or r.w90_2 == w)
                ]
                if w90_rows:
                    model.Add(b_sparse[(f, w)] == sum(w90_rows))
            for w in sorted({k[1] for k in b_daily if k[0] == f}):
                if (f, w) in b_sparse:
                    model.Add(b_daily[(f, w)] + b_sparse[(f, w)] <= 1)
        for row in division:
            ri = row.row_index
            row_terms = [b_row[(f, ri)] for f in b_sel if (f, ri) in b_row]
            if row_terms:
                model.Add(sum(row_terms) == 1)
    else:
        for f, sel in b_sel.items():
            wins = feasibility.feasible_b[f]
            daily_terms = [b_daily[(f, w)] for w in wins if (f, w) in b_daily]
            sparse_terms = [b_sparse[(f, w)] for w in wins if (f, w) in b_sparse]
            model.Add(sum(daily_terms) == sel)
            model.Add(sum(sparse_terms) == 2 * sel)
            for w in wins:
                if (f, w) in b_daily:
                    model.Add(b_daily[(f, w)] + b_sparse[(f, w)] <= 1)

    # ---- Set C: 16 fields, 8 per super-window ----
    if c_vars and config.set_c_count > 0:
        model.Add(sum(c_vars.values()) == config.set_c_count)
        for sw in valid_super:
            sw_terms = [c_vars[(f, sw.index)] for f in feasibility.feasible_c if (f, sw.index) in c_vars]
            if sw_terms:
                per_sw = config.set_c_count // max(len(valid_super), 1)
                model.Add(sum(sw_terms) == per_sw)

    # ---- Window-index capacity (v3 filled formula) ----
    if config.use_window_index_capacity:
        overflow_vars = _add_window_index_capacity(
            model, config, n_inds, a_vars, a_moved, b_daily, b_sparse, c_vars, c_covers
        )
    else:
        _add_calendar_day_capacity(
            model, config, a_vars, b_daily, b_sparse, c_vars, c_covers,
            windows_45, valid_super,
        )

    # ---- Objective ----
    objective_terms = []
    for var in overflow_vars:
        objective_terms.append(var * -1_000_000)
    for (f, g, s), var in a_vars.items():
        slack = feasibility.slack_a_gs.get((f, g, s), 0)
        obj = slack * config.weight_slack + extinction_scores.get(f, 0) * config.weight_extinction
        if obj:
            objective_terms.append(var * obj)

    for (f, sw), var in c_vars.items():
        slack = feasibility.slack_135.get((f, sw), 0)
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

    if objective_terms:
        model.Maximize(sum(objective_terms))

    return (
        model, a_vars, a_moved, b_sel, b_daily, b_sparse, c_vars, c_covers,
        division, valid_super,
    )


def _add_calendar_day_capacity(
    model, config, a_vars, b_daily, b_sparse, c_vars, c_covers,
    windows_45, super_windows,
) -> None:
    """Legacy per-calendar-day capacity (for tiny tests / fallback)."""
    day_vars: Dict[int, List[cp_model.IntVar]] = defaultdict(list)
    sw_by_idx = {sw.index: sw for sw in super_windows}

    for (f, g, s), var in a_vars.items():
        start, end, _ = set_a_slot_calendar(config, g, s, windows_45)
        for day in _daily_days_in_window(start, end):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for (f, w), var in b_daily.items():
        win = windows_45[w - 1]
        for day in _daily_days_in_window(win.start_day, win.end_day):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for (f, w), var in b_sparse.items():
        win = windows_45[w - 1]
        for day in range(win.start_day, win.end_day + 1):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for (f, sw_idx), var in c_vars.items():
        sw = sw_by_idx[sw_idx]
        for day in range(sw.start_day, sw.end_day + 1):
            if day <= config.capacity_last_day:
                day_vars[day].append(var)

    for day, vars_on_day in day_vars.items():
        if vars_on_day:
            model.Add(sum(vars_on_day) <= config.daily_capacity)


def _solve_model(model: cp_model.CpModel, config: SolverConfig) -> Tuple[int, cp_model.CpSolver, float]:
    solver = cp_model.CpSolver()
    solver.parameters.max_time_in_seconds = config.time_limit_seconds
    t0 = time.time()
    status = solver.Solve(model)
    return status, solver, time.time() - t0


def _place_set_d(
    abc_assignments: List[WindowAssignment],
    fields_df: pd.DataFrame,
    feasibility: FeasibilityMaps,
    config: SolverConfig,
) -> List[WindowAssignment]:
    """
    Stage 2: place Set D fields into open window-index slack (v3 schedule_SetD).

    Greedy by d_ranked_fields priority; each D uses one open daily slot at window k.
    """
    windows_45 = feasibility.windows_45
    inds_open = compute_inds_open(abc_assignments, config, windows_45)
    if not inds_open:
        return []

    open_counts: Dict[int, int] = defaultdict(int)
    for k in inds_open:
        open_counts[k] += 1

    d_assignments: List[WindowAssignment] = []
    placed = 0
    target = config.set_d_count

    def set_a_flex(field_id: int) -> int:
        return len(feasibility.feasible_a.get(field_id, set()))

    for fid in config.d_ranked_fields:
        if placed >= target:
            break
        if fid not in feasibility.feasible_d:
            continue
        wins = feasibility.feasible_d[fid]
        for k in sorted(open_counts.keys()):
            if open_counts[k] <= 0:
                continue
            if k not in wins:
                continue
            win = windows_45[k - 1]
            d_assignments.append(
                WindowAssignment(
                    category="D",
                    field_id=fid,
                    cadence="daily",
                    start_day=win.start_day,
                    end_day=win.end_day,
                    window_index=k,
                    group_id=301 + placed,
                    cadence_ind=k,
                )
            )
            open_counts[k] -= 1
            placed += 1
            break
        else:
            best = None
            for k_setd in sorted(wins):
                a_rows_here = [
                    item for item in abc_assignments
                    if item.category == "A"
                    and item.window_index == k_setd
                    and item.group_id != config.set_a_shifted_group
                ]
                for item in a_rows_here:
                    valid_open = [
                        k_open for k_open, count in open_counts.items()
                        if count > 0
                        and k_open in feasibility.feasible_a.get(item.field_id, set())
                    ]
                    if not valid_open:
                        continue
                    choice = (set_a_flex(item.field_id), k_setd, min(valid_open), item)
                    if best is None or choice[0:3] < best[0:3]:
                        best = choice

            if best is None:
                continue

            _, k_setd, k_open, moved_a = best
            open_counts[k_open] -= 1
            moved_win = windows_45[k_open - 1]
            moved_group = _assign_moved_set_a_group(abc_assignments, k_open)
            moved_a.start_day = moved_win.start_day
            moved_a.end_day = moved_win.end_day
            moved_a.window_index = k_open
            moved_a.group_id = moved_group

            d_win = windows_45[k_setd - 1]
            d_assignments.append(
                WindowAssignment(
                    category="D",
                    field_id=fid,
                    cadence="daily",
                    start_day=d_win.start_day,
                    end_day=d_win.end_day,
                    window_index=k_setd,
                    group_id=301 + placed,
                    cadence_ind=k_setd,
                )
            )
            placed += 1

    return d_assignments


def _preclean_set_a_moves(
    abc_assignments: List[WindowAssignment],
    feasibility: FeasibilityMaps,
    config: SolverConfig,
) -> bool:
    """
    Mirror LcsHelper_v3.clean_inds_before_setD.

    If ABC overfills some window indices, move non-shifted SetA rows from those
    indices into currently open indices where the same field is visible.
    """
    windows_45 = feasibility.windows_45
    n_a, n_b45, n_b90, n_c, filled, ok = compute_window_occupancy(
        abc_assignments, config, windows_45, include_d=False
    )
    if not ok:
        return False

    inds_open: List[int] = []
    inds_2move: List[int] = []
    for k, load in enumerate(filled, start=1):
        if load < config.daily_capacity:
            inds_open.extend([k] * int(config.daily_capacity - load))
        elif load > config.daily_capacity:
            inds_2move.extend([k] * int(load - config.daily_capacity))

    if not inds_2move:
        return True

    eligible = [
        item for item in abc_assignments
        if item.category == "A"
        and item.group_id != config.set_a_shifted_group
        and item.window_index in inds_2move
    ]

    moves: List[Tuple[WindowAssignment, int]] = []
    used_rows: Set[int] = set()
    for src in list(inds_2move):
        best_idx = None
        best_dst = None
        best_flex = None
        for idx, item in enumerate(eligible):
            if idx in used_rows or item.window_index != src:
                continue
            feasible_targets = [
                dst for dst in inds_open
                if dst in feasibility.feasible_a.get(item.field_id, set())
            ]
            if not feasible_targets:
                continue
            flex = len(feasibility.feasible_a.get(item.field_id, set()))
            if best_idx is None or (flex, min(feasible_targets), item.field_id) < (
                best_flex, best_dst, eligible[best_idx].field_id
            ):
                best_idx = idx
                best_dst = min(feasible_targets)
                best_flex = flex
        if best_idx is None or best_dst is None:
            continue
        used_rows.add(best_idx)
        inds_open.remove(best_dst)
        moves.append((eligible[best_idx], best_dst))

    for item, dst in moves:
        win = windows_45[dst - 1]
        item.window_index = dst
        item.start_day = win.start_day
        item.end_day = win.end_day
        item.group_id = _assign_moved_set_a_group(abc_assignments, dst)

    _, _, _, _, filled_after, ok_after = compute_window_occupancy(
        abc_assignments, config, windows_45, include_d=False
    )
    return ok_after and all(load <= config.daily_capacity for load in filled_after)


def _assign_moved_set_a_group(assignments: List[WindowAssignment], target_ind: int) -> int:
    used = {
        item.group_id
        for item in assignments
        if item.category == "A"
        and item.window_index == target_ind
        and item.group_id is not None
    }
    group = 7
    while group in used:
        group += 1
    return group


def build_and_solve_with_branching(
    fields_df: pd.DataFrame,
    windows_df: pd.DataFrame,
    eligibility_df: pd.DataFrame,
    config: SolverConfig,
    windows_1dgap_df: Optional[pd.DataFrame] = None,
) -> SolverResult:
    """
    Try set_c_start_ind in {3, 1} and Set A single-group shifts (v3 outer loops).

    :param windows_1dgap_df: optional 1-day-gap visibility windows
    :return: best SolverResult found
    """
    from .feasibility import compute_feasibility

    best: Optional[SolverResult] = None
    last_result: Optional[SolverResult] = None

    def shift_attempts() -> List[Tuple[int, int]]:
        """No shift first, then v3 phase-2 single-group rescue shifts."""
        attempts: List[Tuple[int, int]] = [(0, 0)]
        for group in range(1, config.set_a_n_groups + 1):
            for shift in range(1, config.max_set_a_shift_days + 1):
                attempts.append((group, shift))
        return attempts

    attempts = shift_attempts()
    total_attempts = max(1, 2 * len(attempts))
    per_attempt_seconds = max(0.25, config.time_limit_seconds / total_attempts)

    for sci in (3, 1):
        for shifted_group, shift_days in attempts:
            run_config = replace(
                config,
                set_c_start_ind=sci,
                set_a_shifted_group=shifted_group,
                set_a_shift_days=shift_days,
                time_limit_seconds=per_attempt_seconds,
            )
            feasibility = compute_feasibility(
                fields_df, windows_df, eligibility_df, run_config, windows_1dgap_df
            )
            last_result = build_and_solve(fields_df, feasibility, run_config)
            if last_result.status in ("OPTIMAL", "FEASIBLE"):
                return last_result
            best = last_result
            if shifted_group == 0:
                continue
            # Stop searching shifts for this sci once we leave the no-shift case
            # and hit infeasibility only after exhausting... keep searching
    return best if best is not None else last_result  # type: ignore[return-value]


def build_and_solve(
    fields_df: pd.DataFrame,
    feasibility: FeasibilityMaps,
    config: SolverConfig,
) -> SolverResult:
    """
    Build the CP-SAT model, solve ABC, optionally place Set D, return results.

    :param fields_df: field catalog
    :param feasibility: precomputed feasible pairs
    :param config: campaign and solver parameters
    :return: SolverResult
    """
    plan_last_day = (
        config.first_day + config.set_a_fields_per_group * config.min_window_days - 1
    )
    if config.capacity_last_day < plan_last_day:
        config = replace(config, capacity_last_day=plan_last_day)

    (
        model, a_vars, a_moved, b_sel, b_daily, b_sparse, c_vars, c_covers,
        division, valid_super,
    ) = _build_abc_model(fields_df, feasibility, config)

    status, solver, wall_time = _solve_model(model, config)
    status_name = solver.StatusName(status)

    objective_value = None
    if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        objective_value = solver.ObjectiveValue()

    abc_assignments, _ = _extract_solution(
        solver, status, a_vars, a_moved, b_sel, b_daily, b_sparse, c_vars,
        c_covers, division, valid_super, config, feasibility,
    )

    d_assignments: List[WindowAssignment] = []
    if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        if not _preclean_set_a_moves(abc_assignments, feasibility, config):
            status_name = "INFEASIBLE"
            abc_assignments = []

    if (
        config.solve_set_d_separately
        and config.set_d_count > 0
        and status_name in ("OPTIMAL", "FEASIBLE")
    ):
        d_assignments = _place_set_d(abc_assignments, fields_df, feasibility, config)
        if len(d_assignments) < config.set_d_count:
            status_name = "FEASIBLE" if d_assignments else status_name

    all_assignments = abc_assignments + d_assignments
    daily_observations = build_daily_observations(all_assignments, config)

    return SolverResult(
        status=status_name,
        objective_value=objective_value,
        wall_time_seconds=wall_time,
        window_assignments=all_assignments,
        daily_observations=daily_observations,
        config=config,
        fields_df=fields_df,
        feasibility=feasibility,
    )


def _extract_solution(
    solver: cp_model.CpSolver,
    status: int,
    a_vars,
    a_moved,
    b_sel,
    b_daily,
    b_sparse,
    c_vars,
    c_covers,
    division,
    super_windows,
    config: SolverConfig,
    feasibility: FeasibilityMaps,
) -> Tuple[List[WindowAssignment], List[DailyObservation]]:
    if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        return [], []

    assignments: List[WindowAssignment] = []
    sw_by_idx = {sw.index: sw for sw in super_windows}
    b90_ind_counter: Dict[int, int] = defaultdict(int)

    for (f, g, s), var in a_vars.items():
        if solver.Value(var):
            start, end, wind = set_a_slot_calendar(
                config, g, s, feasibility.windows_45
            )
            assignments.append(
                WindowAssignment(
                    category="A",
                    field_id=f,
                    cadence="daily",
                    start_day=start,
                    end_day=end,
                    window_index=wind,
                    group_id=g,
                )
            )
    moved_group_counter: Dict[int, int] = defaultdict(int)
    for (f, w), var in a_moved.items():
        if solver.Value(var):
            win = feasibility.windows_45[w - 1]
            moved_group_counter[w] += 1
            assignments.append(
                WindowAssignment(
                    category="A",
                    field_id=f,
                    cadence="daily",
                    start_day=win.start_day,
                    end_day=win.end_day,
                    window_index=w,
                    group_id=6 + moved_group_counter[w],
                )
            )

    for f, sel in b_sel.items():
        if not solver.Value(sel):
            continue
        b_windows = {k[1] for k in b_daily if k[0] == f}
        b_windows.update(k[1] for k in b_sparse if k[0] == f)
        for w in sorted(b_windows):
            if (f, w) in b_daily and solver.Value(b_daily[(f, w)]):
                win = feasibility.windows_45[w - 1]
                assignments.append(
                    WindowAssignment(
                        category="B",
                        field_id=f,
                        cadence="daily",
                        start_day=win.start_day,
                        end_day=win.end_day,
                        window_index=w,
                        group_id=100 + w,
                        notes="B_45",
                    )
                )
            if (f, w) in b_sparse and solver.Value(b_sparse[(f, w)]):
                win = feasibility.windows_45[w - 1]
                b90_ind_counter[w] += 1
                assignments.append(
                    WindowAssignment(
                        category="B",
                        field_id=f,
                        cadence="sparse4",
                        start_day=win.start_day,
                        end_day=win.end_day,
                        window_index=w,
                        group_id=200 + w,
                        cadence_ind=b90_ind_counter[w],
                        notes="B_90",
                    )
                )

    c_ind_counter: Dict[int, int] = defaultdict(int)
    for (f, sw_idx), var in c_vars.items():
        if solver.Value(var):
            sw = sw_by_idx[sw_idx]
            c_ind_counter[sw_idx] += 1
            assignments.append(
                WindowAssignment(
                    category="C",
                    field_id=f,
                    cadence="sparse4",
                    start_day=sw.start_day,
                    end_day=sw.end_day,
                    window_index=sw_idx,
                    group_id=10 + sw_idx,
                    cadence_ind=c_ind_counter[sw_idx],
                )
            )

    daily_observations = build_daily_observations(assignments, config)
    return assignments, daily_observations


def build_daily_observations(
    assignments: List[WindowAssignment],
    config: SolverConfig,
) -> List[DailyObservation]:
    """Expand window assignments into per-day slot observations."""
    day_fields: Dict[int, List[Tuple[int, str, str]]] = defaultdict(list)

    for item in assignments:
        if item.cadence == "daily":
            days = _daily_days_in_window(item.start_day, item.end_day)
        else:
            days = sparse_days_for_ind(
                item.start_day,
                item.end_day,
                item.cadence_ind,
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
