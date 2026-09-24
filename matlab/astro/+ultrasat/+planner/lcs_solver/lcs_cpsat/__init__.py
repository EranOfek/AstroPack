# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/__init__.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : ULTRASAT LCS CP-SAT scheduling solver package exports
# ***************************************************************************

"""ULTRASAT LCS CP-SAT scheduling solver (LcsHelper_v3-aligned).

Only the main public objects are exported here.  Most users should call the CLI
modules or import build_and_solve_with_branching / scan_lcs_plans directly.
"""

from .models import SolverConfig, SolverResult
from .scanner import scan_lcs_plans
from .solver import build_and_solve, build_and_solve_with_branching

__all__ = [
    "SolverConfig",
    "SolverResult",
    "build_and_solve",
    "build_and_solve_with_branching",
    "scan_lcs_plans",
]
