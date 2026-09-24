"""Test Set A shift configurations."""
from __future__ import annotations

import json
from collections import Counter
from dataclasses import replace
from pathlib import Path

import pandas as pd

from lcs_cpsat.feasibility import compute_feasibility
from lcs_cpsat.models import SolverConfig
from lcs_cpsat.solver import build_and_solve, build_and_solve_with_branching

data = Path(__file__).resolve().parent.parent.parent / "data" / "lcs_solver_inputs"
fields = pd.read_csv(data / "lcs_fields.csv").rename(
    columns={"Field": "field_id", "RA": "ra", "Dec": "dec", "AU": "A_U"}
)
windows = pd.read_csv(data / "lcs_visibility_windows.csv")
elig = pd.read_csv(data / "lcs_field_eligibility.csv")
w1 = pd.read_csv(data / "lcs_visibility_windows_1dgap.csv")
config = SolverConfig.from_dict(json.load(open(data / "lcs_params.json", encoding="utf-8")))

for sg, sh in [(0, 0), (1, 1), (1, -1), (2, 1)]:
    cfg = replace(
        config,
        use_set_b_division=False,
        set_c_start_ind=3,
        set_a_shifted_group=sg,
        set_a_shift_days=sh,
    )
    feas = compute_feasibility(fields, windows, elig, cfg, w1)
    res = build_and_solve(fields, feas, cfg)
    c = Counter(a.category for a in res.window_assignments)
    b = len({a.field_id for a in res.window_assignments if a.category == "B"})
    print(f"sg={sg} sh={sh}: {res.status} A={c.get('A', 0)} B={b} C={c.get('C', 0)} D={c.get('D', 0)}")

print("--- branching ---")
res = build_and_solve_with_branching(fields, windows, elig, config, w1)
c = Counter(a.category for a in res.window_assignments)
b = len({a.field_id for a in res.window_assignments if a.category == "B"})
print(
    f"branch: {res.status} sci={res.config.set_c_start_ind} "
    f"sg={res.config.set_a_shifted_group} sh={res.config.set_a_shift_days} "
    f"A={c.get('A', 0)} B={b} C={c.get('C', 0)} D={c.get('D', 0)}"
)
