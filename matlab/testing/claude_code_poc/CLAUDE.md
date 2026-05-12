# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Environment

- **Platform:** Windows 11, MATLAB R2023a (`C:\Matlab\R2023a`)
- **Compiler:** Microsoft Visual C++ 2022 (MSVC 14.43.34808) — pre-configured, no setup needed
- **MEX extension:** `mexw64`
- **Shell:** bash (Git Bash / Claude Code terminal)

## Project: MEX POC (`mex_poc/`)

A proof-of-concept showing how to build MATLAB MEX functions in C++ on Windows.

### File layout

```
mex_poc/
  src/array_stats.cpp        ← C++ MEX source
  matlab/array_stats_wrapper.m  ← MATLAB wrapper (returns struct)
  matlab/test_array_stats.m     ← pass/fail test script (no Toolboxes)
  matlab/array_stats.mexw64     ← compiled binary (output of build)
  build/build_mex.m             ← build script
```

### Build

```bash
matlab -batch "cd('mex_poc/build'); build_mex"
```

The build script auto-discovers paths, uses `-R2018a` (required for `mxGetDoubles`) and `-v` (verbose compiler output). To enable debug symbols, change `build_flags` in `build_mex.m` to include `-g`.

### Run tests

```bash
matlab -batch "cd('mex_poc/matlab'); test_array_stats"
```

### Build + test in one command

```bash
matlab -batch "cd('mex_poc/build'); build_mex; cd('../matlab'); test_array_stats"
```

## Critical MEX API rules

- **Always use `-R2018a`** in the `mex()` call. Without it, MATLAB defaults to the legacy R2017b API and `mxGetDoubles` is undefined (`error C3861`).
- **Use `mxGetDoubles(A)`** (R2018a typed API), not the deprecated `mxGetPr(A)`.
- **Use `mwSize`** (not `int`) for all array indices — it's 64-bit on 64-bit MATLAB.
- **Use `mexErrMsgIdAndTxt("ns:tag", "fmt", ...)`** — the ID allows structured `try/catch` in MATLAB by `ME.identifier`.
- **Never `free()` a `plhs[i]` pointer** — MATLAB owns output memory.
- **Data is column-major** — `data[row + col * nrows]` to index matrix element `(row, col)`.
- MATLAB scripts require all local function definitions **at the end of the file**, after all script-level statements.

## Adding a new MEX function

1. Add `src/my_func.cpp` with a `mexFunction` entry point; put algorithm logic in a separate C++ function (not inside `mexFunction`).
2. Add `matlab/my_func_wrapper.m` for input validation and struct output.
3. Add a build line in `build/build_mex.m`:
   ```matlab
   mex('-R2018a', '-v', fullfile(root_dir, 'src', 'my_func.cpp'), '-outdir', out_dir);
   ```
4. Add `matlab/test_my_func.m` following the same pass/fail pattern as `test_array_stats.m`.
