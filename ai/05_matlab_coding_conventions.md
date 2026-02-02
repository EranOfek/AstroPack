# 05 — Coding Conventions

## File and Naming

- One class or function per file; filename matches the class/function name
- Packages use `+folder` naming; classes use `@folder` naming
- No flat scripts in root folders

## Style

- Vectorization preferred over loops
- Explicit over implicit
- Clarity over cleverness
- Deterministic behavior preferred (no wall-clock dependencies in tests)

## Structure Rules

- No hardcoded absolute paths — use configuration
- No UI logic in core scientific code
- No global state
- Source and tests are never mixed in the same directory
- No circular dependencies between packages

## MEX / GPU

- MEX only after profiling proves a bottleneck
- Always maintain a MATLAB reference implementation alongside MEX
- GPU optimization follows the same rule: profile first, always keep a CPU fallback

## App Designer GUIs

- Thin GUI shells; callbacks are short and delegate to core logic
- GUI classes never contain scientific computation
- State lives in DataModule, not in GUI properties
