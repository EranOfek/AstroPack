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
matlab/

MAJOR DOMAIN MODULES
MATLAB code is organized using packages and classes.
Examples for packages:
+ultrasat
+pipeline
+planner
+mission
+astropack
+imUtil
+imProc
+lcUtil
+astro
+celestial
+telescope

In many cases we are using sub packages.
Packages represent a topic. For example +astro/+cosmo/ contains functions related to cosmology.
Class dir name start with "@"
Some classes may live insode a package.


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

NAMING CONENTION
- Function names should by clear and represent what they are doing
- First letter in function name or method name must be lowre case letter.
- All variable names should start with upper case letter.
- Class names should start with upper case letter.
- Class property name should start with upper case letter.

INPUT ARGUMENTS
- When ever there is need for input arguments with default values, the arguments block should be used
- When keyword,value input argument are needed, the variable name containing the arguments will be named Args


HELP SECTION
- Each function should contain help section
- The first line in the help is always a single line description of the code
- For example on help format check existing functions.


UNIT TESTS
- Each package may contain a unitTest.m function.
- unitTest.m function may test one or more functions in the package.
- unitTest return true if sucssfull and will issue an error if failed.


END OF FILE
