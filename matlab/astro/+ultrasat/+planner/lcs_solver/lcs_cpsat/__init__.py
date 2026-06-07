# ***************************************************************************
# Project     : ULTRASAT SOC
# Filename    : matlab/astro/+ultrasat/+planner/lcs_solver/lcs_cpsat/__init__.py
# Author      : Chen Tishler
# Created     : 07/06/2026
# Modified    : 07/06/2026
# Description : ULTRASAT LCS CP-SAT scheduling solver package exports
# ***************************************************************************

"""
ULTRASAT LCS CP-SAT scheduling solver.

Public API:
  - SolverConfig, SolverResult — data models
  - build_and_solve — single CP-SAT run
  - scan_lcs_plans — date-range feasibility scan
"""

from .models import SolverConfig, SolverResult
from .scanner import scan_lcs_plans
from .solver import build_and_solve

__all__ = ["SolverConfig", "SolverResult", "build_and_solve", "scan_lcs_plans"]
