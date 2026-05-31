# Running MATLAB from the Command Line (AstroPack / ULTRASAT)

Top-level reference for **humans**, **Cursor**, **Claude Code**, and **Codex**: how to run MATLAB debug scripts headlessly and read the console output.

Related docs (narrower scope):

- [01_python_api_matlab_client.prompt.md](../01_python_api_matlab_client.prompt.md) — API client verification loop
- [+api/docs/testing_namespace_manager_client.md](../+api/docs/testing_namespace_manager_client.md) — Namespace Manager example
- [CLAUDE.md](../../../../CLAUDE.md) (repo root) — `runtests`, architecture, env vars

---

## MATLAB binary (this machine)

```
C:\Matlab\R2025b\bin\matlab.exe
```

Alternate path seen in older notes: `C:\Matlab\R2025b\bin\win64\MATLAB.exe` (same install).

---

## Headless mode: `-batch`

Use **`-batch`** for every automated or agent-driven run:

- Starts a **new** MATLAB process (no stale workspace).
- Runs the user’s **`startup.m`** (adds AstroPack to the path).
- Executes the quoted MATLAB statement.
- Exits with **non-zero** on uncaught errors.

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "<MATLAB statement>" 2>&1
```

Capture stdout and stderr to a log file (recommended for agents):

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "<statement>" 2>&1 |
    Tee-Object -FilePath "<run.log>"
```

**Normal output:** AstroPack `startup.m` banner first, then script `fprintf` lines.

**Path failure:** `Unrecognized function or variable 'ultrasat...'` — fix `startup.m` or add path manually (see pitfalls).

---

## Environment variables

| Variable | Required for | Typical value (dev laptop) |
|----------|----------------|----------------------------|
| `ASTROPACK_DATA_PATH` | Planner, grids, TOO maps | e.g. `C:\AstroPack\matlab\data` or path containing `ULTRASAT\` |
| `SOC_PATH` | API clients, service debug | e.g. `S:\` or `c:\soc` (must contain `config\services.json`) |
| `SOC_API_KEY` | API clients | set in SOC deployment |

**Preflight (PowerShell) before planner debug runs:**

```powershell
Write-Host "ASTROPACK_DATA_PATH = $env:ASTROPACK_DATA_PATH"
if ($env:ASTROPACK_DATA_PATH) {
    Test-Path (Join-Path $env:ASTROPACK_DATA_PATH "ULTRASAT")
}
```

Planner scripts call `debug_ensureDataPath()` and may set a fallback if the variable is empty; prefer setting it explicitly.

---

## Invocation patterns

### Package function (debug script top function)

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.planner.debug.debug_Hcs()" 2>&1
```

Omit `()` only when the function takes no arguments and you use the form without parens (both work for zero-arg functions):

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.api.debug.clients.debug_ClientFactory" 2>&1
```

### Interactive MATLAB (same statement)

```matlab
ultrasat.planner.debug.debug_Hcs()
```

### Unit tests (repo `tests/` tree)

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "runtests('tests')" 2>&1
```

---

## Rules for AI agents (Cursor / Claude Code / Codex)

1. Always use **`matlab.exe -batch`**, not a long-lived GUI session.
2. Always redirect with **`2>&1`** so errors appear in the terminal log.
3. Read the **full** console output before reporting success or failure.
4. Do not use **`-nodisplay -nojvm`** unless you also add AstroPack to the path manually.
5. Prefer **`Tee-Object`** to a log under the repo or `+debug/working_dir/` for long runs.
6. Do not edit [`unitTest.m`](../+planner/unitTest.m) when validating planner behavior — use `+planner/+debug/debug_*.m` instead.

---

## Observation planner debug scripts

Location: [`+planner/+debug/`](../+planner/+debug/)

| Script | Run command | External data |
|--------|-------------|---------------|
| HCS | `ultrasat.planner.debug.debug_Hcs()` | None (in-memory targets) |
| DDT | `ultrasat.planner.debug.debug_Ddt()` | None |
| TOO | `ultrasat.planner.debug.debug_Too()` | Minimal path: none; prob-map: `+debug/input_data/lvc_2024_04_01_00_40_58_000000.csv` |
| LCS | `ultrasat.planner.debug.debug_Lcs()` | `ASTROPACK_DATA_PATH/ULTRASAT/LCS_nonoverlapping_grid_surveys.csv`, `LCS_nonoverlapping_grid.csv`; optional `api_response.mat` |
| AllSS | `ultrasat.planner.debug.debug_AllSs()` | `constructGrid`: grid file via `BaseDataDir`; builds: `alss_uniq_targ.mat` |

### HCS

```powershell
cd C:\Ultrasat\AstroPack
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.planner.debug.debug_Hcs()" 2>&1 |
    Tee-Object -FilePath "matlab\astro\+ultrasat\+planner\+debug\working_dir\debug_Hcs_run.log"
```

Sub-tests: `debug_Hcs_basic`, `debug_Hcs_inspect`, `debug_Hcs_customExptime`.

### DDT

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.planner.debug.debug_Ddt()" 2>&1 |
    Tee-Object -FilePath "matlab\astro\+ultrasat\+planner\+debug\working_dir\debug_Ddt_run.log"
```

Sub-tests: `debug_Ddt_basic`, `debug_Ddt_multipleGroups`, `debug_Ddt_inspect`.

### TOO

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.planner.debug.debug_Too()" 2>&1 |
    Tee-Object -FilePath "matlab\astro\+ultrasat\+planner\+debug\working_dir\debug_Too_run.log"
```

Sub-tests: `debug_Too_minimal` (always); `debug_Too_withProbMap`, `debug_Too_highCoverage` (skip with warning if CSV missing).

### LCS

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.planner.debug.debug_Lcs()" 2>&1 |
    Tee-Object -FilePath "matlab\astro\+ultrasat\+planner\+debug\working_dir\debug_Lcs_run.log"
```

Sub-tests: `debug_Lcs_buildLCS1`, `debug_Lcs_buildLCS_legacy`, `debug_Lcs_retrieveApproved`, `debug_Lcs_editAndDelete`.

### AllSS (All-Sky)

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.planner.debug.debug_AllSs()" 2>&1 |
    Tee-Object -FilePath "matlab\astro\+ultrasat\+planner\+debug\working_dir\debug_AllSs_run.log"
```

Sub-tests: `debug_AllSs_constructGrid` (no mat file); `debug_AllSs_buildWeekly`, `debug_AllSs_buildSemester` (need `alss_uniq_targ.mat`).

### Run all planner debug scripts (sequence)

```powershell
$matlab = "C:\Matlab\R2025b\bin\matlab.exe"
$logDir = "C:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+planner\+debug\working_dir"
@("debug_Hcs","debug_Ddt","debug_Too","debug_Lcs","debug_AllSs") | ForEach-Object {
    $name = $_
    Write-Host "========== $name =========="
    & $matlab -batch "ultrasat.planner.debug.${name}()" 2>&1 |
        Tee-Object -FilePath (Join-Path $logDir "${name}_run.log")
}
```

---

## API client debug (sanity check)

```powershell
& "C:\Matlab\R2025b\bin\matlab.exe" -batch "ultrasat.api.debug.clients.debug_ClientFactory" 2>&1
```

Requires `SOC_PATH` and `SOC_API_KEY`. See [01_python_api_matlab_client.prompt.md](../01_python_api_matlab_client.prompt.md).

---

## Common pitfalls

| Issue | Cause | Fix |
|-------|--------|-----|
| `Unrecognized function or variable 'ultrasat...'` | Path not set | Ensure `startup.m` adds `AstroPack\matlab`; or `-batch "addpath(genpath('C:\Ultrasat\AstroPack\matlab')); ultrasat...."` |
| Empty or skipped TOO prob-map tests | CSV not in `+debug/input_data/` | Copy `lvc_2024_04_01_00_40_58_000000.csv` from `ULTRASAT/` data folder |
| LCS / AllSS errors on grid files | `ASTROPACK_DATA_PATH` wrong | Point to folder containing `ULTRASAT\*.csv` / `*.mat` |
| `-nodisplay -nojvm` | Bypasses normal startup | Do not use for AstroPack debug unless you add path yourself |
| Long run, no visible output | Buffering | Use `Tee-Object`; wait for process exit |

---

## Debug script convention

- Filename: `debug_<Name>.m` in a `+debug/` package next to the code under test.
- Top function orchestrates; sub-functions `debug_<Name>_<scenario>` are runnable alone (cursor + F9 in MATLAB Editor).
- Run from CLI: `ultrasat.<package>.debug.debug_<Name>()`

Example locations:

- `+ultrasat/+planner/+debug/` — planner plan types (HCS, LCS, DDT, TOO, AllSS)
- `+ultrasat/+api/+debug/+clients/` — REST clients
- `+ultrasat/+alerts_filters/+lvk/+debug/` — LVK alert filters
