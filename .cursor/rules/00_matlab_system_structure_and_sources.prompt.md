# 00_matlab_system_structure_and_sources.prompt.md

ROLE AND CONTEXT
You are writing and modifying MATLAB code inside the AstroPack repository.
This codebase is shared, long-lived, and mission-critical, mainly for ULTRASAT and LAST.
It is not exploratory MATLAB and not personal scripting.

The goal is correctness, clarity, reproducibility, and long-term maintainability.

HIGH-LEVEL SYSTEM OVERVIEW
AstroPack is a multi-language system.
MATLAB is used as a scientific and planning engine.

MATLAB responsibilities:
- Scientific algorithms
- Numerical computation
- Planning and feasibility logic
- Simulation and validation

MATLAB is not:
- A backend server
- A system orchestrator
- A UI-first application

Other system parts:
- Python FastAPI for services and orchestration
- App Designer for thin GUI shells

CORE TERMINOLOGY (STRICT)
- Core logic: algorithmic MATLAB code without UI or backend dependencies
- GUI: App Designer apps that call core logic
- Worker: long-running MATLAB process (often file-driven)
- Helper: small focused utility or domain-support class

PROJECT ROOT STRUCTURE
All MATLAB source code lives under:
astropack/Matlab

All MATLAB tests live under:
astropack/MatlabTests

Source and tests are never mixed.

MAJOR DOMAIN MODULES
MATLAB code is organized using packages:
+ultrasat
+last
+planner
+mission
+astropack

Each package represents a domain, not a technical layer.

CODING PRINCIPLES (MANDATORY)
- Clarity over cleverness
- Explicit over implicit
- Deterministic behavior preferred
- One main class or function per file
- File name must match main class or function
- No hidden global state

WHAT NOT TO DO
- No flat scripts in root folders
- No UI logic in core code
- No hardcoded absolute paths
- No silent error handling

OUTPUT EXPECTATION FROM THE LLM
When generating or modifying MATLAB code:
- Respect existing structure
- Do not invent new architectural layers
- Ask before changing boundaries

END OF FILE
