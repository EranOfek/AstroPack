# CLAUDE.md 

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running Tests

```matlab
runtests('tests')                        % All tests
runtests('tests/TestStruct2KeyVal')      % Single test file
ultrasat.planner.uplanner.unitTest       % Planner core unit test
```

Tests are function-based (`return functiontests(localfunctions)`), not class-based. Directory structure in `tests/` mirrors `matlab/`.

## Running Debug Scripts

Debug scripts follow the convention `debug_<ClassName>.m` under `+debug/+ultrasat/` (see [debug/CLAUDE.md](debug/CLAUDE.md)). Run them directly from MATLAB:

```matlab
debug.ultrasat.api.clients.debug_NamespaceManagerClient()
debug.ultrasat.api.clients.debug_ClientFactory()
debug.ultrasat.planner.guiutils.debug_MainModule()
```

## Environment Variables

Required before running any API client or the planner:

| Variable | Purpose |
|---|---|
| `SOC_PATH` | Root of the SOC deployment (contains `config/services.json`, `log/`) |
| `SOC_API_KEY` | API key for all FastAPI service calls |
| `ASTROPACK_DATA_PATH` | Root for MATLAB data files (Windows only) |

## Architecture

### Layer Order (strict, top-to-bottom dependency only)

```
App Designer GUIs (.mlapp)  ← thin shells, short callbacks only
    ↓
PlannerMain*Helper classes  ← one helper per functional area
    ↓
MainModule                  ← central state mediator (DataModule pattern)
    ↓
API Clients (ClientBase)    ← HTTP/REST, via ClientFactory + services.json
    ↓
Core Logic (uplanner, etc.) ← deterministic, no UI, no API dependencies
    ↓
Base Classes (Loggable, @Base, @Component)
```

GUI classes never contain scientific or API logic. `MainModule` owns all application state — the App Designer shell never calls API clients or `uplanner` directly.

### Package Map

| Package | Location | Purpose |
|---|---|---|
| `+ultrasat/+api/+clients/` | `matlab/astro/` | REST clients for FastAPI services |
| `+ultrasat/+api/+core/` | `matlab/astro/` | `Loggable`, `Config` base classes |
| `+ultrasat/+planner/` | `matlab/astro/` | `uplanner` core planning logic |
| `+ultrasat/+planner/+gui/` | `matlab/astro/` | App Designer `.mlapp` files |
| `+ultrasat/+planner/+guiutils/` | `matlab/astro/` | Helper classes for PlannerMain |
| `+ultrasat/+planner/+guiutils/+debug/` | `matlab/astro/` | Debug scripts for guiutils |
| `+imProc/`, `+pipeline/` | `matlab/image/` | Image processing algorithms |
| `+db/` | `matlab/util/` | Database access layer |
| `matlab/base/` | — | `@Base`, `@Component`, `@Configuration`, `@LogFile` |
| `matlab/external/` | — | Third-party; read-only, never modify |
| `matlab/obsolete/` | — | Historical reference; read-only, never call |

### API Client Pattern

All REST clients extend `ClientBase` (which extends `Loggable`). `ClientFactory` reads `$SOC_PATH/config/services.json` to resolve service base URLs. Clients never hardcode URLs.

```matlab
factory = ultrasat.api.clients.ClientFactory();
url     = factory.getServiceBaseUrl('namespace_manager');  % or 'user_manager', etc.
client  = ultrasat.api.clients.NamespaceManagerClient(url);
response = client.getNamespaceList();  % returns struct from JSON
```

`postRequest(endpoint, params)` is the sole HTTP method — all API calls go through it. Responses are structs decoded from JSON; check `response.status == 'ok'` or `response.ok`.

### App Designer + Extracted Source

`.mlapp` files are binary. A Python script (`extract_mlapp_code.py`, invoked via `_extract_mlapp_code.bat`) exports readable `.m` snapshots into `+gui/mlapp_source/` for code review and search. **Edit only the `.mlapp` file in App Designer**; the `_code.m` files are read-only snapshots.

### MATLAB–Python IPC

Long-running MATLAB worker services (`snr_service`, `slew_service`, `too_service`, `incoming_alerts_filter`) communicate with Python via file-based IPC: Python writes a JSON/MAT request file → MATLAB polls and processes → MATLAB writes a response file. No sockets or REST between MATLAB workers and Python.

### Database

- **ClickHouse**: analytical queries, pipeline results — bulk inserts only, no row-by-row loops.
- **PostgreSQL**: transactional data (LAST mission).
- All DB access is isolated in `matlab/util/+db/`; no inline SQL elsewhere.
- Connection profiles live in `config/Database.DbConnections.*`.

## Coding Conventions

- One class or function per file; filename matches the class/function name.
- MATLAB packages use `+folder`; classes use `@folder`.
- No hardcoded absolute paths — use `@Configuration` or environment variables.
- Vectorization preferred over loops.
- MEX only after profiling; always keep a MATLAB reference implementation alongside.
- `Drafts-*` folders are dead-end experiments — never build on them.

## GAIADR3spec Enrichment + catsHTM Tooling (in progress, 2026-07)

**Goal:** let `imProc.transmission.fitPhotCalibTrans` with `SelectionMethod='pythonLike'`
get all calibrator data from ONE catsHTM match (not 3–4) by appending **8 Gaia DR3
columns to `GAIADR3spec`** at positions **693–700**. Spectra stay at 7–692, so
`SpFluxCol=[7 349 350 692]` is unchanged.

The 8 columns (693–700): `PMRA`, `PMDec`, `phot_g_mean_mag`, `phot_bp_mean_mag`,
`phot_rp_mean_mag`, `bp_rp`, `phot_bp_rp_excess_factor` (from the **local `GAIADR3`
catsHTM**), and `classprob_dsc_combmod_star` (from a user-downloaded Gaia DR3 VOTable —
`xp_sampled_mean_spectrum ∩ astrophysical_parameters`; on the ESA archive use
`WHERE gs.has_xp_sampled='true'` instead of joining `xp_sampled_mean_spectrum`).

**Prep pipeline** — `matlab/astro/+VO/+prep/+GAIA/+dr3/`:
1. `buildGaiaClassprobHTM(VotFile,...)` — VOTable → CSV via STILTS `tpipe keepcols` → catsHTM `GAIADR3classprob` (RA,Dec,classprob).
2. `mergeGAIADR3spec(OutDir,...)` — per GAIADR3spec cell, in-memory match to `GAIADR3` (7 cols) + `GAIADR3classprob` (1) using **cached HTM indices + `load_cat` by id + `VO.search.match_cats`** (the `xmatch_2cats` pattern; no per-cell `cone_search`/`fminsearch`), appends via `catsHTM.insertColumns`. Args: `ClassprobDir` (auto-addpath), `SkipExisting` (resume), `BaseDir` (point at a local copy of GAIADR3spec — `/euclid` is NFS).
3. `checkGAIADR3specMerge(...)` — samples cells, reports per-column non-NaN fraction/min/median/max; auto-runs at end of merge.

**New `@catsHTM` methods** (`matlab/astro/@catsHTM/catsHTM.m`):
- `insertColumns(CatName, Names, Units, OutDir,...)` — append 1+ columns in ONE pass; `FillValue` may be `@(M)->[Nrows×K]`; writes OutDir files **fresh** (no `copyfile`); copies the (unchanged) index; `SkipExisting` resume. `insertColumn` is the single-column alias (error IDs kept as `catsHTM:insertColumn:*`).
- `renameCat(SrcName, DstName, CatDir)` — rename a catsHTM catalog in a dir: file renames + rewrite the index's internal `/<name>_HTM` dataset.

**Deploy (replace `GAIADR3spec` in place, same name):** registry stores no column
info (`Ncol` comes from the colcell), so just rsync the new 700-col `*_htm_*.hdf5` +
`*_htmColCell.mat` into `/euclid/catsHTM/GAIA/DR3spec/` (index is unchanged),
regenerate the md5 list `list.euler.checksum._GAIA_DR3spec_` (`md5sum *.hdf5 *.mat`,
run on the local copy for speed), then restart MATLAB (clears the cached
`GAIADR3spec_HTM` index var that `HDF5.load_check` keeps in the base workspace).

**Runtime status (enriched catalog DEPLOYED, 2026-08):** the single-match rewrite is
**live** everywhere. `findCalibCandidates` does ONE `match_catsHTM` to GAIADR3spec and
harvests cols 693–700 onto every candidate. Downstream now reads those tail columns with
no second match: `auditCalibCandidates` (bp_rp / excess), `AttachBP_RP` (BP_RP/MAG_BP/
MAG_RP), and `selectCalibratorsPythonLike` (single GAIADR3spec match; classprob is a
direct column read, no TAP). `fetchGaiaBPRP` and the `AuditCatName` arg thread were
removed. There is no remaining second catsHTM match, TAP query, or `fetchGaiaBPRP` call.

Tests: `tests/astro/catsHTM/test_add_remove_source.m`, `test_renameCat.m`.
