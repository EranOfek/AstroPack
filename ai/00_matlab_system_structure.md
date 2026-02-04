# 00 — System Structure and Architecture

## Project

AstroPack (a.k.a. MAATv2) — MATLAB-based astronomy, astrophysics, and image processing framework. Developed at the Weizmann Institute of Science for the ULTRASAT and LAST missions.

**Languages**: MATLAB (primary), Python (FastAPI services, integration), C/C++ (MEX kernels), SQL (PostgreSQL, ClickHouse).

## Repository Layout

- `matlab/` — All source code
- `tests/` — Unit tests (parallel directory structure to `matlab/`)
- `config/` — YAML, INI, JSON configuration files
- `database/` — SQL schema definitions
- `external/` — Third-party packages (read-only)

## Layering

```
App Designer GUIs (thin shells, short callbacks)
    ↓
DataModule / API Layer (state management, backend communication)
    ↓
Core Logic Classes (reusable, testable, no UI dependencies)
    ↓
Base Classes (@Base, @Component)
    ↓
MATLAB + MEX kernels
```

Each layer depends only on the layers below it. GUI code never contains scientific logic. Core logic classes have no UI dependencies.


Layering violations are considered architectural bugs, even if functionally correct.


## Running Tests

```matlab
runtests('tests')                    % All tests
runtests('tests/TestStruct2KeyVal')  % Single test file
```

Tests are function-based (return `functiontests(localfunctions)`), not class-based.

## Installation

```bash
git clone <repo> && git checkout dev1
```

In MATLAB: `edit manuals.Install`. See also `config/Installer.yml`.

## Required MATLAB Toolboxes

Communications Toolbox, Image Processing Toolbox, Statistical and Machine Learning Toolbox.
