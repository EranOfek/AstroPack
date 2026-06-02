# ULTRASAT (+ultrasat) — Cursor Rules

Rules for the ULTRASAT-specific MATLAB package under `matlab/astro/+ultrasat/`.

## When to open this workspace

- Daily planner, API client, or MATLAB service work for ULTRASAT SOC
- Open folder: `c:\Ultrasat\AstroPack\matlab\astro\+ultrasat\`

Generic AstroPack MATLAB rules (not ULTRASAT-specific) remain at `AstroPack/.cursor/rules/` (01–06).

## Format

Rules are `.mdc` files with YAML frontmatter (`alwaysApply`, `globs`, `description`).

## Rule index

| # | File | Activation |
|---|------|------------|
| 01 | `01_matlab_file_based_ipc_with_python.mdc` | `+services/**` |
| 02 | `02_matlab_ultrasat_planner_structure.mdc` | `+planner/**` |
| 03 | `03_matlab_clickhouse_pipeline_usage.mdc` | ClickHouse-related globs |
| 04 | `04_python_api_matlab_client.mdc` | `+api/**` |
| 05 | `05_alerts_filter_end_to_end_flow.mdc` | `+services/+alerts_filter/**`, `+alerts_filters/**` |
| 06 | `06_matlab_services_workers_and_long_running_processes.mdc` | `+services/**`, `*_service.m` |
| 07 | `07_matlab_app_designer_structure.mdc` | `+gui/**`, `*.mlapp` |

## Related

- SOC Python bridge (repo-level): `Ultrasat.git/.cursor/rules/08_matlab_bridge_and_astropack_rules.mdc`
- Generic MATLAB: `AstroPack/.cursor/rules/` (01–06)
