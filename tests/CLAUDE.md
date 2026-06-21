# LLM Guide: Auto-Generating MATLAB Unit Tests in `tests/`

This document instructs LLMs how to create **new** MATLAB function-based unit test files under `tests/`. Read this before generating any test code.

**Related docs:** [tests/README.md](README.md) (folder layout overview), root [CLAUDE.md](../CLAUDE.md) (repository conventions).

---

## 1. Purpose and Golden Rules

### What this folder is for

- `tests/` mirrors the `matlab/` source tree exactly.
- Tests use MATLAB **function-based unit tests** (`functiontests` + local test functions).
- Each test function receives a `matlab.unittest.TestCase` object as its first argument.

### Golden rules (mandatory)

1. **Create new files only.** Never modify existing files when generating tests.
2. **Do not touch source `unitTest.m` files** in `matlab/` (e.g. `matlab/image/@AstroSpec/unitTest.m`, `matlab/astro/+celestial/@Targets/unitTest.m`). Those remain as-is.
3. **Do not modify legacy copied tests** already under `tests/` (e.g. `tests/astro/+celestial/@Targets/test_@Targets_01.m`, `tests/util/@convert/test_convert_01.m`). Leave them unchanged even if they use older patterns.
4. **Mirror `matlab/` structure** when placing new test files.
5. **Primary function name must equal the file name** (without `.m`). This is required for `runtests` and `TestSuite.fromFolder` to discover tests reliably.
6. **Use function-based tests only** — not class-based `matlab.unittest.TestCase` subclasses.

> **Note on `.cursor/rules/05_matlab_unit_tests_parallel_structure.mdc`:** That rule describes an older class-based `Test`-prefixed convention. For **new** files in `tests/`, follow **this document** (function-based, `test_` prefix).

---

## 2. Mandatory File Format

Every new test file has this structure:

```matlab
function tests = test_<Name>
    % Unit tests for <fully.qualified.symbol>.
    %
    % Brief description of what is tested.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testSomething(testCase)
    % One-line description of this test case.

    % Arrange
    Input = ...;

    % Act
    Actual = some.package.function(Input);

    % Assert
    testCase.verifyEqual(Actual, Expected, 'Failure message.');
end
```

### Rules

| Rule | Detail |
|------|--------|
| Top function | `function tests = test_<Name>` where `<Name>` matches the filename |
| Suite builder | `tests = functiontests(localfunctions);` |
| Test functions | Local functions named `testXxx(testCase)` — camelCase, descriptive |
| Assertions | Use `testCase.verifyEqual`, `verifyTrue`, `verifyFalse`, `verifyError`, `verifyWarning`, etc. |
| Float compares | Always pass `'AbsTol'` or `'RelTol'` for non-exact numeric results |
| Failure messages | Always include a short diagnostic string as the last argument |

### Known legacy exception (do not copy)

`tests/astro/+celestial/+convert/test_CelestialConvert.m` uses primary function `TestCelestialConvert` which **does not match** its filename. That file predates the current convention. **New files must not repeat this mistake.**

### Canonical reference

See `tests/astro/+celestial/+healpix/test_ang2pix.m` — this is the correct pattern.

---

## 3. Granularity: When to Use Which Style

Two granularity modes are supported. Choose based on what you are testing.

### Mode A — Per-function (default)

**Use when:** testing a single public function in a `+package` folder.

| Source | New test file |
|--------|---------------|
| `matlab/astro/+celestial/+healpix/ang2pix.m` | `tests/astro/+celestial/+healpix/test_ang2pix.m` |
| `matlab/util/+tools/+struct/struct2keyval.m` | `tests/util/+tools/+struct/test_struct2keyval.m` |

- One test file per source function.
- File name: `test_<functionName>.m`
- Top function: `function tests = test_<functionName>`
- Local test functions cover that function's behaviors, edge cases, and options.

**Prefer this mode** for free functions and package functions with a clear single entry point.

### Mode B — Per-file / per-class / per-topic

**Use when:**

- Testing a `@ClassName` class with many methods that share setup.
- Testing a small, tightly coupled set of functions in one source file.
- Shared fixtures or expensive setup would be duplicated across many per-function files.

| Source | New test file |
|--------|---------------|
| `matlab/astro/+celestial/+convert/` (multiple related functions) | `tests/astro/+celestial/+convert/test_celestialConvert.m` |
| `matlab/image/@AstroSpec/` (class with many methods) | `tests/image/@AstroSpec/test_AstroSpec.m` |

- One test file groups related local test functions.
- File name: `test_<ClassName>.m` or `test_<Topic>.m`
- Top function name must still match the filename exactly.
- Each local function tests one behavior: `testPrecessCooRAandDec(testCase)`, `testWriteReadRoundTrip(testCase)`, etc.

**Reference:** `tests/astro/+celestial/+convert/test_CelestialConvert.m` (content style only — fix the filename/primary-function mismatch in new files).

### Decision checklist

| Question | If yes → |
|----------|----------|
| Single public function with independent behavior? | Mode A (per-function) |
| Class with shared state / expensive constructor? | Mode B (per-class) |
| Multiple functions always tested together? | Mode B (per-topic) |
| Large API surface (>10 public functions)? | Mode A, one file per function |

---

## 4. Placement and Naming Rules

### Path mirroring

Replace the `matlab/` prefix with `tests/`; keep the rest of the path identical:

```
matlab/astro/+celestial/+healpix/ang2pix.m
  → tests/astro/+celestial/+healpix/test_ang2pix.m

matlab/util/+tools/+struct/struct2keyval.m
  → tests/util/+tools/+struct/test_struct2keyval.m

matlab/image/@AstroSpec/AstroSpec.m
  → tests/image/@AstroSpec/test_AstroSpec.m
```

### Naming conventions

| Element | Convention | Example |
|---------|------------|---------|
| Test file | lowercase `test_` prefix | `test_ang2pix.m` |
| Top function | same as filename (no `.m`) | `function tests = test_ang2pix` |
| Local test func | `test` + camelCase description | `testRoundTripNested` |
| Helper class | `<Area>TestHelper.m` | `HealpixTestHelper.m` |
| Runner script | `run<Area>UnitTests.m` | `runHealpixUnitTests.m` |

### Avoid name collisions

Before creating a file, check whether a test already exists in the target folder:

- Legacy pattern: `test_<name>_01.m`, `test_@ClassName_01.m` — **do not overwrite or edit**.
- If a conflicting name exists, pick a distinct new name or use Mode B with a topic name that does not collide.

### Variable naming (AstroPack convention)

Follow repository coding rules even inside tests:

- Variables: start with uppercase (`Input`, `Expected`, `Actual`, `NSide`)
- Functions/methods in source code: lowercase first letter (call as `celestial.healpix.ang2pix`, not invented aliases)

---

## 5. Test Body Best Practices

### Determinism

- No dependence on wall-clock time (`now`, `datetime('now')`) unless the function under test requires it — and then use fixed Julian dates or known epochs.
- Fix random seeds: `rng(42)` at the start of tests using random data.
- Use known reference values with documented tolerances for floating-point astronomy/math.

### File I/O

- Use `tempname` or `tempdir` for any file read/write in tests.
- Clean up in `teardown(testCase)` or `teardownOnce(testCase)`.
- Never read from or write to production paths, `$SOC_PATH`, or `$ASTROPACK_DATA_PATH` unless explicitly required — and then mark with `@TODO` for human review.

### Setup and teardown

Use local setup/teardown functions when tests share fixtures:

```matlab
function setup(testCase)
    testCase.TestData = struct('NSide', 8, 'Pix', int64(0));
end

function teardown(testCase)
    if isfield(testCase, 'TempFile') && isfile(testCase.TempFile)
        delete(testCase.TempFile);
    end
end
```

For expensive one-time setup across all tests in a file, use `setupOnce(testCase)` / `teardownOnce(testCase)`.

### What to test

| Do test | Do not test (in this pattern) |
|---------|-------------------------------|
| Deterministic functions and algorithms | App Designer GUIs (`.mlapp`) |
| Core data transforms and numerical results | Long-running worker processes |
| Class methods with mock/fixture data | External REST services / live databases |
| Error conditions with `verifyError` | End-to-end pipeline integration |

### Assertion patterns

```matlab
% Exact equality
testCase.verifyEqual(Actual, Expected, 'Description.');

% Floating-point with tolerance
testCase.verifyEqual(Actual, Expected, 'AbsTol', 1e-10, 'Description.');

% Expected error
testCase.verifyError(@() myFunc(badInput), 'MATLAB:expectedErrorId', 'Description.');

% Logical conditions
testCase.verifyTrue(all(Actual > 0), 'All values must be positive.');
```

### Marking uncertain areas

When generating tests, mark items needing human review:

```matlab
% @TODO - Confirm reference values against published ephemeris.
% @Object - celestial.Targets requires a populated target list; fixture TBD.
```

---

## 6. Graceful Skips (Shared Helper Class)

When tests depend on optional resources (MEX binaries, toolboxes, data files), **skip** rather than fail if the dependency is missing.

### Pattern

Create a **new** helper class in the same test folder (only if one does not already exist):

```
tests/astro/+celestial/+healpix/HealpixTestHelper.m   ← reference implementation
```

Use `testCase.assumeTrue(...)` inside static helper methods. When the condition is false, the test is marked **Skipped**, not Failed.

### Reference: `HealpixTestHelper`

See `tests/astro/+celestial/+healpix/HealpixTestHelper.m` for:

- `mexAvailable(MexName)` — check MEX binary exists
- `assumeMex(testCase, MexName)` — skip if MEX missing
- `assumeFunctionExists(testCase, FunctionName)` — skip if `.m` file missing
- `assumeMappingToolbox(testCase)` — skip if toolbox function unavailable

### Usage in a test

```matlab
function testRequiresMex(testCase)
    MyAreaTestHelper.assumeMex(testCase, 'my_mex_function');
    Result = my.package.mex.my_mex_function(Input);
    testCase.verifyEqual(Result, Expected);
end
```

### When to create a helper

Create `<Area>TestHelper.m` when **two or more** test files in the same folder need the same skip/utility logic. Do not duplicate `assumeTrue` boilerplate across files.

---

## 7. Suite Runner Script (Optional Scaffolding)

For a cohesive test area (e.g. a `+package` folder with many `test_*.m` files), create a runner script at an appropriate level:

```
tests/astro/runHealpixUnitTests.m   ← reference implementation
```

### Runner responsibilities

1. Resolve `AstroPackRoot` from script location (do not hardcode absolute paths).
2. `addpath(genpath(...))` for required `matlab/` subtrees.
3. Optionally compile MEX or perform one-time environment setup.
4. `addpath` the test folder (so helper classes resolve).
5. Build suite: `matlab.unittest.TestSuite.fromFolder(TestFolder, 'IncludingSubfolders', false)`.
6. Run with detailed text output.
7. Print pass / fail / skip summary.
8. `error(...)` if any test failed (for CI/batch exit code).

### Reference: `runHealpixUnitTests.m`

See `tests/astro/runHealpixUnitTests.m` for the full pattern including MEX compilation and summary reporting.

---

## 8. How to Run Tests

### Single test file

```matlab
runtests('tests/astro/+celestial/+healpix/test_ang2pix.m')
```

From repo root with paths set, or after `addpath` as in the runner script.

### Entire folder (interactive)

```matlab
runtests('tests/astro/+celestial/+healpix')
```

### Folder with logging

```matlab
runTestsFromFolder('C:\Ultrasat\AstroPack\tests\astro\+celestial\+healpix', 'TestResults.log')
```

Uses `tests/runTestsFromFolder.m` — saves `TestResults.mat` in the folder.

### Batch / CI

```matlab
matlab -batch "run('C:\Ultrasat\AstroPack\tests\astro\runHealpixUnitTests.m')"
```

Adjust path to your `run<Area>UnitTests.m` script.

---

## 9. Copy-Paste Templates

### Template A — Per-function test file

```matlab
function tests = test_myFunction
    % Unit tests for my.package.myFunction.
    %
    % Tests basic behavior, edge cases, and option handling.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicCase(testCase)
    % Default arguments produce expected output.

    Input = 1;
    Expected = 2;
    Actual = my.package.myFunction(Input);

    testCase.verifyEqual(Actual, Expected, 'Basic case failed.');
end

function testEmptyInput(testCase)
    % Empty input returns empty output without error.

    Actual = my.package.myFunction([]);
    testCase.verifyEqual(Actual, [], 'Empty input failed.');
end

function testInvalidInputErrors(testCase)
    % Bad input throws expected error.

    testCase.verifyError(@() my.package.myFunction(-1), ...
        'my:package:myFunction:InvalidInput', ...
        'Negative input should error.');
end
```

### Template B — Per-class / per-topic test file

```matlab
function tests = test_MyClass
    % Unit tests for my.package.MyClass.
    %
    % Covers construction, key methods, and round-trip I/O.

    tests = functiontests(localfunctions);
end

%% Fixture

function setup(testCase)
    testCase.TestObj = my.package.MyClass('DefaultArg', 42);
end

%% Test Functions

function testDefaultConstruction(testCase)
    Obj = testCase.TestObj;
    testCase.verifyEqual(Obj.DefaultArg, 42, 'Default construction failed.');
end

function testMethodReturnsExpected(testCase)
    Obj = testCase.TestObj;
    Result = Obj.compute(10);
    testCase.verifyEqual(Result, 52, 'compute() failed.');
end

function testWriteReadRoundTrip(testCase)
    Obj = testCase.TestObj;
    TempFile = fullfile(tempdir, [tempname, '.mat']);
    testCase.applyFixture(matlab.unittest.fixtures.TemporaryFolderFixture);
    c = onCleanup(@() delete(TempFile)); %#ok<NASGU>

    Obj.write(TempFile);
    Loaded = my.package.MyClass(TempFile);
    testCase.verifyEqual(Loaded.DefaultArg, Obj.DefaultArg, 'Round-trip failed.');
end
```

### Template C — Shared test helper class

```matlab
classdef MyAreaTestHelper
    % MyAreaTestHelper  Shared utilities for my.area unit tests.
    %
    % Provides assume-skip helpers so tests degrade gracefully when
    % MEX binaries, toolboxes, or optional dependencies are missing.

    methods (Static)

        function Available = mexAvailable(MexName)
            % mexAvailable  True when the named MEX function exists.
            MexFcn = ['my.area.mex.' MexName];
            Available = (exist(MexFcn, 'file') == 3);
        end

        function assumeMex(testCase, MexName)
            % assumeMex  Skip the current test when MEX is unavailable.
            testCase.assumeTrue( ...
                MyAreaTestHelper.mexAvailable(MexName), ...
                sprintf('Skipping: MEX "%s" is not compiled.', MexName));
        end

        function assumeFunctionExists(testCase, FunctionName)
            % assumeFunctionExists  Skip when a function file is missing.
            testCase.assumeTrue( ...
                exist(FunctionName, 'file') == 2, ...
                sprintf('Skipping: function "%s" is not available.', FunctionName));
        end

        function assumeToolbox(testCase, ToolboxFunc, Label)
            % assumeToolbox  Skip when a toolbox function is unavailable.
            testCase.assumeTrue( ...
                exist(ToolboxFunc, 'file') == 2, ...
                sprintf('Skipping: %s is not available.', Label));
        end

    end
end
```

### Template D — Suite runner script

```matlab
% runMyAreaUnitTests  Run unit tests for my.area package.
%
% Usage:
%   matlab -batch "run('C:\Ultrasat\AstroPack\tests\<domain>\runMyAreaUnitTests.m')"

AstroPackRoot = fileparts(fileparts(fileparts(mfilename('fullpath'))));
addpath(genpath(fullfile(AstroPackRoot, 'matlab', '<domain>')));
addpath(genpath(fullfile(AstroPackRoot, 'matlab', 'base')));
addpath(genpath(fullfile(AstroPackRoot, 'matlab', 'util')));

TestFolder = fullfile(AstroPackRoot, 'tests', '<domain>', '<path-to-test-folder>');
addpath(TestFolder);

fprintf('=== Running my.area unit tests ===\n');
Suite = matlab.unittest.TestSuite.fromFolder(TestFolder, 'IncludingSubfolders', false);
Runner = matlab.unittest.TestRunner.withTextOutput( ...
    'OutputDetail', matlab.unittest.Verbosity.Detailed);
Results = Runner.run(Suite);

fprintf('\n=== Summary ===\n');
fprintf('Total:      %d\n', numel(Results));
fprintf('Passed:     %d\n', sum([Results.Passed]));
fprintf('Failed:     %d\n', sum([Results.Failed]));
fprintf('Incomplete: %d\n', sum([Results.Incomplete]));
Skipped = sum(strcmp({Results.Status}, 'Skipped'));
fprintf('Skipped:    %d\n', Skipped);

if any([Results.Failed])
    fprintf('\nFailed tests:\n');
    Failed = Results([Results.Failed]);
    for I = 1:numel(Failed)
        fprintf('  %s\n', Failed(I).Name);
    end
    error('my.area unit tests failed.');
end
```

Replace `<domain>`, `<path-to-test-folder>`, and `addpath` targets to match the area under test.

---

## 10. LLM Checklist

Before submitting generated test code, verify every item:

### Do

- [ ] Create **only new files** under `tests/`
- [ ] Mirror the `matlab/` path structure exactly
- [ ] Name file `test_<Name>.m` with top function `function tests = test_<Name>`
- [ ] Use `functiontests(localfunctions)` in the top function
- [ ] Name local tests `testSomething(testCase)` with camelCase
- [ ] Use `testCase.verifyEqual` / `verifyTrue` / `verifyError` with failure messages
- [ ] Use `'AbsTol'` / `'RelTol'` for floating-point comparisons
- [ ] Keep tests deterministic (`rng` fixed, no wall-clock dependence)
- [ ] Use temp files/folders for I/O; clean up in teardown
- [ ] Add `assumeTrue` skips for optional MEX, toolboxes, or missing deps
- [ ] Mark uncertain fixtures or reference values with `@TODO` or `@Object`
- [ ] Add brief `%` comments on each test function explaining intent

### Do not

- [ ] Do **not** modify any existing file (source or tests)
- [ ] Do **not** edit `unitTest.m` in `matlab/`
- [ ] Do **not** edit legacy `test_*_01.m` files already in `tests/`
- [ ] Do **not** use class-based `TestCase` subclasses for new tests
- [ ] Do **not** mismatch primary function name and filename
- [ ] Do **not** hardcode absolute paths (use `AstroPackRoot` pattern in runners)
- [ ] Do **not** depend on production data paths or live external services
- [ ] Do **not** generate GUI or worker integration tests in this pattern

---

## Quick Reference: Existing Examples

| Pattern | Example path |
|---------|--------------|
| Per-function tests (correct naming) | `tests/astro/+celestial/+healpix/test_ang2pix.m` |
| Per-topic tests (legacy name mismatch — do not copy naming) | `tests/astro/+celestial/+convert/test_CelestialConvert.m` |
| Skip helper class | `tests/astro/+celestial/+healpix/HealpixTestHelper.m` |
| Suite runner | `tests/astro/runHealpixUnitTests.m` |
| Legacy copied test (do not modify) | `tests/astro/+celestial/@Targets/test_@Targets_01.m` |
| Source unitTest (do not modify) | `matlab/astro/+celestial/@Targets/unitTest.m` |

---

## LLM Prompt Starter

When asking an LLM to generate tests for a specific source file, include:

```
Read tests/CLAUDE.md and generate NEW function-based unit tests for:
  Source: matlab/<path>/<file>.m

Requirements:
- Create only new files under tests/ (mirror path)
- Follow tests/CLAUDE.md exactly
- Do not modify any existing files
- Use test_<name>.m with matching top function name
- Include @TODO markers where human verification is needed
```
