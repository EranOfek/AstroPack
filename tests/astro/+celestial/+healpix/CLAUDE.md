# LLM Guide: celestial.healpix Unit Tests

Tests for `matlab/astro/+celestial/+healpix/` functions.

## Running Tests

```matlab
% From the AstroPack root, with paths set:
runtests('tests/astro/+celestial/+healpix')

% With logging (saves TestResults.mat):
runTestsFromFolder('C:\Ultrasat\AstroPack\tests\astro\+celestial\+healpix', 'TestResults.log')

% Via the runner script:
matlab -batch "run('C:\Ultrasat\AstroPack\tests\astro\runHealpixUnitTests.m')"
```

MATLAB 2025b: `C:\MATLAB\R2025b\bin\matlab.exe`.  
Required paths: `matlab/astro`, `matlab/base`, `matlab/util`, `matlab/external` (all via `addpath(genpath(...))`).

## File List

| Test file | Source function(s) | Notes |
|---|---|---|
| `test_ang2pix.m` | `ang2pix` | Requires `pix2ang_nested` MEX |
| `test_pix2ang.m` | `pix2ang` | Requires `pix2ang_nested` MEX; Lon output is in [0, 2π] |
| `test_coneSearch.m` | `coneSearch` | Requires MEX + Mapping Toolbox |
| `test_coneSearchRecur.m` | `coneSearchRecur` | Requires `pix2ang_nested` MEX |
| `test_coneSearch2PixRanges.m` | `coneSearch2PixRanges` | Requires `pix2ang_nested` MEX |
| `test_findNeighbors.m` | `findNeighbors` | Pure MATLAB + optional MEX |
| `test_healpixVertices.m` | `healpixVertices` | Pure MATLAB |
| `test_healpix_01.m` | `coneSearch`, `coneSearchRecur` | Integration smoke test |
| `test_latitudeRings.m` | `latitudeRings` | Pure MATLAB |
| `test_mexHealpix.m` | `mex.*` wrappers | All require compiled MEX |
| `test_nPix.m` | `nPix` | Pure MATLAB |
| `test_nRing.m` | `nRing` | Pure MATLAB |
| `test_nest2xyf.m` | `nest2xyf`, `xyf2nest` | Pure MATLAB |
| `test_pixBoundries.m` | `pixBoundries`, `isInside` | Both functions error by design |
| `test_pixRadius.m` | `pixRadius` | Pure MATLAB |
| `test_pixelResolution.m` | `increasePixelResolution`, `decreasePixelResolution` | Pure MATLAB |
| `test_pixelSons_nested.m` | `pixelSons_nested` | Double pixel index only (int64 not supported) |
| `test_plot.m` | `plot` | Requires MEX + axesm |
| `test_radius2NSide.m` | `radius2NSide` | Pure MATLAB |
| `test_rasterize_polygon.m` | `rasterize_polygon` | Requires MEX + optional deps |
| `test_uniqueId.m` | `pix2uniqueId`, `uniqueId2pix` | Pure MATLAB |
| `test_convertHealPixNsideNested.m` | `convertHealPixNsideNested` | Pure MATLAB |
| `test_convertHealPix2highNsideNested.m` | `convertHealPix2highNsideNested` | Pure MATLAB |

## Helper Class

`HealpixTestHelper` provides shared skip logic:
- `assumeMex(testCase, MexName)` — skips if the compiled `.mexw64` / `.mexa64` binary is absent (`.m` stub files do NOT count as present MEX).
- `assumeCoreAngPixMex(testCase)` — skips when `ang2pix_nested` or `pix2ang_nested` MEX is missing.
- `assumeMappingToolbox(testCase)` — skips when `reckon` (Mapping Toolbox) is unavailable.
- `assumeFunctionExists(testCase, name)` — skips when a function file is missing.

## Known Conventions

- Pixel indices are 0-based integers. High-NSide functions use `int64`; `pixelSons_nested` requires **double** input (MATLAB int64 + non-scalar double arithmetic is unsupported).
- `pix2ang` returns **Lon ∈ [0, 2π]** (not wrapped to [-π, π]).
- `ang2pix` and `pix2ang` MEX functions return **column vectors** regardless of input shape. Use `(:)` when comparing against row vectors.
- `verifyError` tests use `?MException` (any error) when the source function uses bare `assert()`, which produces an empty error identifier in MATLAB 2025b.

## Known Function Bugs (Failing Tests)

These tests are intentionally failing to document bugs in the source functions:

| Test | Function | Bug description |
|---|---|---|
| `test_coneSearch/testCentralPixelIncluded` | `coneSearch` | Small-radius cone does not include its own center pixel |
| `test_coneSearchRecur/testCentersWithinExpandedRadius` | `coneSearchRecur` | Returns pixels beyond `Radius + pixelRadius` bound |
| `test_findNeighbors/testNoNegativeIndices` | `findNeighbors` | "Out of range subscript" crash for some boundary pixels at NSide=16 |
| `test_healpixVertices/testVerticesNearPixelCenter` | `healpixVertices` | Vertex-to-center distances are ~6× too large (algorithm bug) |
| `test_pixRadius/testMaxRadiusGeEqualAreaRadius` | `pixRadius` | `MaxPixRadius = 1/NSide` is smaller than `PixelRadius = π/(√3·NSide)`, violating the invariant MaxPixRadius ≥ PixelRadius |

## Skipped Tests

Tests that skip on this machine (MEX not compiled, toolboxes absent):
- All `test_mexHealpix` tests that call `neighbors_nested`, `coneSearch`, `rasterize_polygon` MEX.
- `test_findNeighbors/testMatchesMexNeighborsWhenAvailable`
- `test_coneSearch2PixRanges/testConeAlgoWhenExternalMexAvailable`
- `test_plot/*` — requires axesm (Mapping Toolbox) and `plot.skyCircles`
- `test_rasterize_polygon/*` — requires `mex.coneSearch` and helper functions

## Adding New Tests

Follow the pattern in `tests/CLAUDE.md`. Key rules specific to this folder:

1. Use `HealpixTestHelper.assumeCoreAngPixMex(testCase)` in any test that calls `ang2pix` or `pix2ang`.
2. Pass double pixel indices to `pixelSons_nested` (not int64).
3. When comparing `ang2pix` / `pix2ang` output with a reference vector, use `(:)` on both sides to avoid row/column mismatches.
4. Longitude output from `pix2ang` is in `[0, 2π]`; do not check `abs(Lon) ≤ π`.
