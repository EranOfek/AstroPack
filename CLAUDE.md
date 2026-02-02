# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

AstroPack (a.k.a. MAATv2) is a large, mature MATLAB-based astronomy, astrophysics, and image processing framework developed at the Weizmann Institute of Science. It serves the ULTRASAT and LAST telescope missions. The codebase prioritizes correctness, reproducibility, and numerical reliability.

**Languages**: MATLAB (primary), Python (FastAPI services, integration), C/C++ (MEX performance kernels), SQL (PostgreSQL, ClickHouse).

## Running Tests

```matlab
runtests('tests')                    % Run all tests
runtests('tests/TestStruct2KeyVal')  % Run a single test file
```

Tests are function-based (return `functiontests(localfunctions)`), not class-based. Test directory mirrors `matlab/` structure.

## Detailed Knowledge Files

For deeper context, see the `ai/` directory:

- [`ai/00_matlab_system_structure.md`](ai/00_matlab_system_structure.md) — Architecture, layering, repo layout
- [`ai/01_matlab_domain_map_and_intent.md`](ai/01_matlab_domain_map_and_intent.md) — Scientific domains, missions, intent
- [`ai/02_matlab_module_responsibilities.md`](ai/02_matlab_module_responsibilities.md) — Folder-by-folder responsibilities
- [`ai/03_matlab_core_abstractions.md`](ai/03_matlab_core_abstractions.md) — Core classes (`@AstroImage`, `@Base`, etc.)
- [`ai/04_matlab_data_flow_and_ipc.md`](ai/04_matlab_data_flow_and_ipc.md) — MATLAB–Python IPC, file-based workflows
- [`ai/05_matlab_coding_conventions.md`](ai/05_matlab_coding_conventions.md) — Coding style, structure, rules
- [`ai/06_matlab_policies_external_obsolete.md`](ai/06_matlab_policies_external_obsolete.md) — External / obsolete / read-only policies
- [`ai/07_matlab_services_and_long_running.md`](ai/07_matlab_services_and_long_running.md) — Services, workers, resilience rules
- [`ai/08_matlab_database_patterns.md`](ai/08_matlab_database_patterns.md) — ClickHouse / PostgreSQL usage
- [`ai/09_matlab_glossary.md`](ai/09_matlab_glossary.md) — Project-specific terminology

## Quick Reference

- **One class or function per file**; filename matches the class/function name
- Packages: `+folder`; classes: `@folder`
- Vectorization over loops; MEX/GPU only after profiling
- No hardcoded paths, no global state, no UI in core logic
- External/obsolete code is read-only
- Services must survive bad inputs and recover per-request
- Database access isolated in `+db/`; bulk operations preferred

## Installation

```bash
git clone <repo> && git checkout dev1
```
Then in MATLAB: `edit manuals.Install`. See also `config/Installer.yml`.
