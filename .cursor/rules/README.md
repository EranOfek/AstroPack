# AstroPack — Cursor Rules

Generic MATLAB rules for the AstroPack repository. **ULTRASAT-specific** rules live in `matlab/astro/+ultrasat/.cursor/rules/`.

## Workspace tips

| Work | Open folder | Rules loaded |
|------|-------------|--------------|
| Generic MATLAB / astro / image | `c:\Ultrasat\AstroPack` | This folder |
| ULTRASAT planner, API, services | `c:\Ultrasat\AstroPack\matlab\astro\+ultrasat` | `+ultrasat/.cursor/rules/` |
| Python SOC bridge | `Ultrasat.git` + `@` AstroPack files | `Ultrasat.git/.cursor/rules/08_matlab_bridge_and_astropack_rules.mdc` |

## Format

Rules are `.mdc` files with YAML frontmatter (`alwaysApply`, `globs`, `description`).

## Rule index (this folder — generic MATLAB)

| # | File | Activation |
|---|------|------------|
| 01 | `01_matlab_system_structure_and_sources.mdc` | always |
| 02 | `02_matlab_repository_domain_map_and_intent.mdc` | always |
| 03 | `03_matlab_obsolete_external_and_drafts_policy.mdc` | obsolete/external globs |
| 04 | `04_matlab_mex_optimization.mdc` | mex globs |
| 05 | `05_matlab_unit_tests_parallel_structure.mdc` | test globs |
| 06 | `06_matlab_gpu_optimization.mdc` | gpu globs |

## ULTRASAT-specific rules (+ultrasat)

| # | File |
|---|------|
| 01 | `+ultrasat/.cursor/rules/01_matlab_file_based_ipc_with_python.mdc` |
| 02 | `+ultrasat/.cursor/rules/02_matlab_ultrasat_planner_structure.mdc` |
| 03 | `+ultrasat/.cursor/rules/03_matlab_clickhouse_pipeline_usage.mdc` |
| 04 | `+ultrasat/.cursor/rules/04_python_api_matlab_client.mdc` |
| 05 | `+ultrasat/.cursor/rules/05_alerts_filter_end_to_end_flow.mdc` |
| 06 | `+ultrasat/.cursor/rules/06_matlab_services_workers_and_long_running_processes.mdc` |
| 07 | `+ultrasat/.cursor/rules/07_matlab_app_designer_structure.mdc` |

## Onboarding prompts (reference)

ChatGPT onboarding threads are linked in git history; use `matlab/` scope for codebase learning prompts.
