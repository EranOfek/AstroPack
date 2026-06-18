# LLM Guide: Authoring MATLAB Debug Files in `debug/`

This document instructs LLMs (and humans) how to create **debug / example entry-point**
files under `debug/`. Read this before generating or moving any debug code.

**Related docs:** root [CLAUDE.md](../CLAUDE.md) (repository conventions),
[tests/CLAUDE.md](../tests/CLAUDE.md) (the parallel guide for unit tests).

---

## 1. Purpose and Golden Rules

### What this folder is for

- `debug/` holds **runnable debug drivers and usage examples** for production code that
  lives under `matlab/`. They smoke-test a class/function, demonstrate how to call it, and
  print human-readable output. They are **not** unit tests (those live in `tests/`).
- Keeping them here keeps the production tree (`matlab/`) clean — no `+debug/` folders
  polluting the source packages.
- All debug code is reachable through one top-level package: `debug.*`.

### Golden rules (mandatory)

1. **Debug files never change production behavior.** They only *call* production code.
2. **Mirror the source PACKAGE path, not the filesystem path** (see Section 3).
3. **File name = primary function name** (without `.m`), prefixed with `debug_`.
4. **Production code must never depend on anything in `debug/`.** Dependencies point one
   way: `debug/` -> `matlab/`, never the reverse.
5. **No hardcoded absolute paths.** Use `ASTROPACK_PATH` (repo root) or `ASTROPACK_DATA_PATH` for production resources; use `fileparts(mfilename('fullpath'))` only for co-located debug fixtures (see Section 5).

---

## 2. Naming and Call Convention

| Item | Rule | Example |
|------|------|---------|
| File name | `debug_<SourceName>.m` | `debug_JsonFileIpc.m` |
| Primary function | Same as file name, first letter lowercase | `function debug_JsonFileIpc()` |
| Sub-helpers | Local functions in the same file, `debug_<thing>` | `function debug_writeRead()` |
| Call path | `debug.` + source package path | `debug.ultrasat.services.common.debug_JsonFileIpc()` |

A debug file usually mirrors one source symbol: `PlanWebPageExporter.m` ->
`debug_PlanWebPageExporter.m`; `ang2pix.m` -> a `debug_*` driver in the matching package.

---

## 3. Placement Rule: Mirror the Package Path (NOT the filesystem path)

This is the most important and most error-prone rule.

In MATLAB, the folder placed on the path (the **addpath root**) is **invisible** to the
namespace. Only `+`-prefixed folders form the package name. So when mapping a source
folder into `debug/`, you keep the `+package` segments and **drop the addpath-root
folders**: `matlab/`, `astro/`, `image/`, `util/`, `base/`, etc.

```
debug/                <- addpath root (must be on the MATLAB path)
  +debug/             <- top-level package, prefix for every call
    +<pkg>/+<pkg>/... <- ONLY the +package segments copied from the source
```

### Worked examples

| Source folder | Source package path | Debug folder | Call path |
|---------------|---------------------|--------------|-----------|
| `matlab/astro/+ultrasat/+services/+common` | `ultrasat.services.common` | `+debug/+ultrasat/+services/+common` | `debug.ultrasat.services.common.debug_X()` |
| `matlab/astro/+ultrasat/+planner/+guiutils` | `ultrasat.planner.guiutils` | `+debug/+ultrasat/+planner/+guiutils` | `debug.ultrasat.planner.guiutils.debug_X()` |
| `matlab/astro/+celestial/+healpix` | `celestial.healpix` | `+debug/+celestial/+healpix` | `debug.celestial.healpix.debug_X()` |

Documented paths start at `+debug/`; on disk, prepend the repo addpath root `debug/` (e.g.
`+debug/+ultrasat/+planner/` → `debug/+debug/+ultrasat/+planner/`).

### Do NOT

- **Do NOT add `astro` (or any addpath-root folder) to the path.** `astro` was never part
  of any call path. `debug.astro.ultrasat.planner...` is **wrong**.
- **Do NOT keep a `+debug` leaf folder** under the mirrored package. The single `+debug`
  package is at the top only. A source folder `.../+common/+debug/` becomes
  `+debug/+ultrasat/+services/+common/` (the leaf `+debug` is removed and replaced by the
  top-level `+debug` prefix).

### Reference subtree

`+debug/+celestial/+healpix/` is the canonical example. Match its structure.

### Paths in documentation

- READMEs and guides: use `+debug/+ultrasat/...` (same as the `Filename` header in `.m` files).
- MATLAB calls: use `debug.ultrasat...`.
- Markdown/wiki link **targets** may keep full relative filesystem paths (`../../../../debug/+debug/...`) so links resolve on disk.

---

## 4. Mandatory File Format

```matlab
function debug_<Name>()
    %==========================================================================
    % Project     : ...
    % Filename    : +debug/+ultrasat/<package-segments>/debug_<Name>.m
    % Author      : ...
    % Description : What this driver exercises.
    % Run by      : debug.ultrasat.<package>.debug_<Name>()
    %==========================================================================
    fprintf('\n========== DEBUG <NAME> ==========\n');

    debug_caseOne();
    debug_caseTwo();

    fprintf('========== DEBUG <NAME> DONE ==========\n');
end


function debug_caseOne()
    fprintf('\n--- caseOne ---\n');
    try
        Result = some.production.package.someFunction(args);
        fprintf('ok: %s\n', mat2str(Result));
    catch ME
        fprintf(2, 'FAILED: %s\n', ME.message);
    end
end
```

### Rules

| Rule | Detail |
|------|--------|
| Primary function | `debug_<Name>` matching the filename exactly |
| Block header | Use `%====` banner; include `Filename`, `Description`, `Run by` |
| `Filename` | Package-relative path starting at `+debug` — **omit** the filesystem addpath root (`debug/` before `+debug`). Example: `+debug/+ultrasat/+planner/debug_Hcs.m`. Do **not** use the MATLAB call namespace (`debug.ultrasat...`) here. |
| `Run by` | **Mandatory.** Place as the last header line before the closing `%====`, after all `Description` lines (including continuations). Value is the full call: `debug.ultrasat.<package>.debug_<Name>()` |
| Output | Print clear progress with `fprintf`; banner start/end lines help when run via `-batch` |
| Robustness | Wrap individual cases in `try/catch` so one failure does not abort the whole driver |
| Same-package calls | Call sibling debug helpers **unqualified** (they share the package); call production code **fully qualified** (`ultrasat.planner.uplanner(...)`) |

---

## 5. Additional Data / Fixture Files

Some drivers need input fixtures (CSV, JSON, MAT) or scratch output dirs.

### Fixtures that belong to the debug file

Keep them **inside the debug folder** so they travel with the `.m` file, and load them
relative to the file itself:

```matlab
ThisDir = fileparts(mfilename('fullpath'));
csvPath = fullfile(ThisDir, 'input_data', 'sample.csv');
```

Typical fixture folders: `input_data/` (read-only inputs), `sample_alerts/` (JSON
samples), `working_dir/` (generated outputs). Move these together with the `.m` files
whenever you relocate a debug folder, so the self-relative loads keep working.

### Reaching PRODUCTION resources (data shipped under `matlab/` or the repo `data/`)

Production data does **not** move into `debug/`. Never reach it with fragile `..` hops out
of the debug tree — those break the moment a file moves. Instead:

- **Repo root:** use `getenv('ASTROPACK_PATH')` (must be set to the AstroPack repository root). Always check for empty and `error(...)` if unset.
- **Bundled data:** use `getenv('ASTROPACK_DATA_PATH')` or `@Configuration`, same as production code.
- **Co-located debug fixtures:** keep using `fileparts(mfilename('fullpath'))` for files inside the debug folder (`input_data/`, `working_dir/`, `sample_alerts/`).

Example reaching production `+planner/data`:

```matlab
RepoRoot = getenv('ASTROPACK_PATH');
if isempty(RepoRoot)
    error('ASTROPACK_PATH is not set');
end
GridFile = fullfile(RepoRoot, 'matlab', 'astro', '+ultrasat', '+planner', 'data', 'LCS_fields.csv');
```

Do **not** chain `fileparts(mfilename('fullpath'))` to walk up to the repo root.

---

## 6. MATLAB Path Setup

For `debug.*` to resolve, the **`debug/` folder** (the addpath root containing `+debug`)
must be on the MATLAB path:

```matlab
addpath(genpath(fullfile(AstroPackRoot, 'debug')));
```

This mirrors how `matlab/astro/` is on the path so that `+ultrasat` / `+celestial`
resolve. Add it in the active `matlab/startup/startup.m` (or your batch `-batch` addpath
line) if not already present.

---

## 7. When You Move an Existing `+debug` Folder Into `debug/`

1. Move the **entire** folder contents (`.m`, README, fixtures, `.py`, `obsolete/`) so
   adjacency is preserved.
2. Apply the placement rule in Section 3 (strip addpath roots, drop the leaf `+debug`).
3. Update headers (`Filename`, `Run by`, usage `%` lines) to the new `debug.*` path. `Filename` uses `+debug/+ultrasat/...` (no leading `debug/`).
4. Update any **fully-qualified** runtime calls (e.g. RENAMED shim files) to the new path;
   unqualified same-package calls need no change.
5. Re-fix location-sensitive paths (Section 5): use `ASTROPACK_PATH` for production resources under `matlab/`; keep `mfilename` only for co-located fixtures.
6. Update docs that reference the old namespace (root `CLAUDE.md`, package `README.md`s,
   `docs/run_matlab_cli.md`).

---

## 8. Quick Checklist Before Submitting a Debug File

- [ ] Documented path is `+debug/<package-segments-only>/` (no `astro`, no leaf `+debug`); physical location is repo `debug/` + that path.
- [ ] File name is `debug_<Name>.m`; primary function matches.
- [ ] Header has `Filename` (`+debug/+ultrasat/...`), `Description`, and `Run by` (last line before closing `%====`).
- [ ] Calls production code fully qualified; calls siblings unqualified.
- [ ] Fixtures live inside the folder and load via `fileparts(mfilename('fullpath'))`.
- [ ] Production paths via `ASTROPACK_PATH` / `ASTROPACK_DATA_PATH`; no `fileparts` depth chains into `matlab/`.
- [ ] Runs cleanly via `debug.<path>.debug_<Name>()` with `debug/` on the path.
