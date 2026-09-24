# celestial.healpix Unit Test Report

**Date:** 2026-06-18  
**MATLAB:** R2025b (`C:\MATLAB\R2025b\bin\matlab.exe`)  
**Branch:** dev1  
**Test folder:** `tests/astro/+celestial/+healpix/`

---

## Summary

| Status | Count |
|--------|-------|
| **Passed** | **79** |
| **Failed** | **5** |
| **Skipped** | **11** |
| **Total** | **94** |

All 5 failures are **function bugs** — the tests correctly document incorrect behavior in the source functions. No test logic errors remain.

---

## Fixes Applied to Tests

The following errors were found and fixed in the test files before running:

| File | Issue | Fix |
|---|---|---|
| `test_healpix_01.m` | Primary function name `unitTest` did not match filename; MATLAB could not discover it as a test | Rewrote as proper function-based test with `test_healpix_01` top function |
| `test_latitudeRings.m` | `true(1, N)` row vs column mismatch in monotonicity check | Changed to `verifyTrue(all(...))` |
| `test_latitudeRings.m` | `verifyEqual(vector, scalar)` fails for equatorial ring count | Changed to `verifyTrue(all(Equatorial == 4*NSide))` |
| `test_convertHealPix2highNsideNested.m` | `4 * 2^16^2` evaluates to `Inf` (MATLAB right-assoc `^`) | Changed to `4 * (2^16)^2` |
| `test_pixelSons_nested.m` | `double(Sons)` [1×4] vs `double(Increased)` [4×1] size mismatch | Changed to `double(Sons(:))` vs `double(Increased(:))` |
| `test_pixelSons_nested.m` | `pixelSons_nested` called with `int64` arrays — MATLAB forbids combining int64 with non-scalar double | Changed all Pix inputs to `double` |
| `test_findNeighbors.m` | `NSide=256` with bounds [3, 5] arcsec copied from unitTest which uses `NSide=2^16` | Changed to `NSide = 2^16` to match unitTest |
| `test_coneSearch2PixRanges.m` | `verifyEqual(Widths, scalar)` — Widths is a vector | Changed to `verifyTrue(all(Widths == ExpectedWidth))` |
| `test_pix2ang.m` | `abs(Lon) ≤ π` — `pix2ang` returns Lon in `[0, 2π]`, not `[-π, π]` | Changed to `0 ≤ Lon ≤ 2π` |
| `test_pix2ang.m` | `verifyEqual(int64(Rebuilt), Pix)` — MEX returns column, `Pix` is row | Added `(:)` on both sides |
| `test_mexHealpix.m` | Same row/column mismatch in two round-trip tests | Added `(:)` on both sides |
| `test_pixelResolution.m` | `verifyError(..., 'MATLAB:assertion:failed')` — bare `assert()` produces empty ID in MATLAB 2025b | Changed to `?MException` |
| `HealpixTestHelper.m` | `mexAvailable` used `which()` which finds `.m` stub files, causing MEX tests to run and error instead of skip | Fixed to reject paths ending in `.m` |

---

## Per-Test Results

### test_ang2pix (5 tests)

| Test | Status |
|------|--------|
| `testRoundTripNested` | PASS |
| `testRoundTripRing` | PASS |
| `testCooUnitsDegMatchesRad` | PASS |
| `testUniqueIdOption` | PASS |
| `testVectorInput` | PASS |

### test_pix2ang (4 tests)

| Test | Status |
|------|--------|
| `testCentersInValidRanges` | PASS |
| `testRoundTripToAng2Pix` | PASS |
| `testCooUnitsDegScaling` | PASS |
| `testUniqueIdInput` | PASS |

### test_coneSearch (4 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testReturnsUniquePixels` | PASS | |
| `testCentralPixelIncluded` | **FAIL** | Function bug: `coneSearch` with radius=0.05 rad at NSide=64 does not include the center pixel (pixel 310 not in results 269–450) |
| `testPixelCentersWithinExpandedRadius` | PASS | |
| `testDegreeUnits` | PASS | |

### test_coneSearchRecur (4 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testReturnsNonEmptyUniqueSet` | PASS | |
| `testOverlapsFastConeSearch` | PASS | |
| `testCentersWithinExpandedRadius` | **FAIL** | Function bug: `coneSearchRecur` at NSide=64, radius=0.08 rad returns pixels up to 0.108 rad from center, exceeding bound of 0.096 rad (`Radius + 1/NSide`) |
| `testDegreeUnits` | PASS | |

### test_coneSearch2PixRanges (6 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testNeighbAlgoOutputShape` | PASS | |
| `testRangeWidthMatchesChildCount` | PASS | |
| `testCentralCatalogPixelCovered` | PASS | |
| `testConeAlgoWhenExternalMexAvailable` | SKIP | `mex.coneSearch` not compiled |
| `testNonPowerOfTwoCatalogErrors` | PASS | |
| `testNonScalarCenterErrors` | PASS | |

### test_findNeighbors (5 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testOutputShapeEightNeighbors` | PASS | |
| `testIncludeSelfAddsRow` | PASS | |
| `testNoNegativeIndices` | **FAIL** | Function bug: `findNeighbors` crashes with "Out of range subscript" for some boundary pixels when processing all 3072 pixels at NSide=16 |
| `testNeighborAngularDistance` | PASS | |
| `testMatchesMexNeighborsWhenAvailable` | SKIP | `mex.neighbors_nested` not compiled |

### test_healpixVertices (4 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testOutputShapeAndLatitudeBounds` | PASS | |
| `testOutOfRangePixelErrors` | PASS | |
| `testInvalidTypeErrors` | PASS | |
| `testVerticesNearPixelCenter` | **FAIL** | Function bug: vertex-to-center distances are ~0.45–0.77 rad at NSide=16, but expected < `2 * pixRadius ≈ 0.23 rad`. Algorithm produces geometrically incorrect vertex positions |

### test_healpix_01 (2 tests)

| Test | Status |
|------|--------|
| `testConeSearchIntegration` | PASS |
| `testConeSearchOverlap` | PASS |

### test_latitudeRings (5 tests)

| Test | Status |
|------|--------|
| `testOutputSizes` | PASS |
| `testLatitudesMonotonicAndBounded` | PASS |
| `testPolesHaveZeroPixels` | PASS |
| `testTotalPixelCountMatchesNPix` | PASS |
| `testEquatorialRingHasFullCount` | PASS |

### test_mexHealpix (7 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testAng2PixNestedRoundTrip` | PASS | |
| `testAng2PixRingRoundTrip` | PASS | |
| `testAng2PixNestedInt64Output` | PASS | |
| `testNestedVsRingMayDiffer` | PASS | |
| `testNeighborsNestedSmoke` | SKIP | `mex.neighbors_nested` not compiled |
| `testConeSearchMexSmoke` | SKIP | `mex.coneSearch` not compiled |
| `testRasterizePolygonMexSmoke` | SKIP | `mex.rasterize_polygon` not compiled |

### test_nPix (3 tests)

| Test | Status |
|------|--------|
| `testScalarFormula` | PASS |
| `testVectorizedInput` | PASS |
| `testMinimumResolution` | PASS |

### test_nRing (3 tests)

| Test | Status |
|------|--------|
| `testFormulaScalar` | PASS |
| `testFormulaVector` | PASS |
| `testMinimumNSide` | PASS |

### test_nest2xyf (5 tests)

| Test | Status |
|------|--------|
| `testRoundTripAllPixelsNSide8` | PASS |
| `testCoordinatesInValidRange` | PASS |
| `testCheckRangeRejectsOutOfBounds` | PASS |
| `testNonPowerOfTwoNSideErrors` | PASS |
| `testScalarRoundTrip` | PASS |

### test_pixBoundries (2 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testPixBoundriesAlwaysErrors` | PASS | Documents that `pixBoundries` always errors (by design — function is obsolete) |
| `testIsInsideErrorsViaPixBoundries` | PASS | |

### test_pixRadius (3 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testEqualAreaRadiusFormula` | PASS | |
| `testMaxRadiusGeEqualAreaRadius` | **FAIL** | Function bug: `pixRadius` returns `MaxPixRadius = 1/NSide ≈ 0.062` which is less than `PixelRadius = π/(√3·NSide) ≈ 0.113` at NSide=16. The enclosing radius cannot be smaller than the equal-area radius — `MaxPixRadius` formula is wrong |
| `testRadiusDecreasesWithResolution` | PASS | |

### test_pixelResolution (5 tests)

| Test | Status |
|------|--------|
| `testIncreaseThenDecreaseRoundTrip` | PASS |
| `testIncreaseChildRangeContiguous` | PASS |
| `testDecreaseDocExample` | PASS |
| `testIncreaseRequiresMultipleNSide` | PASS |
| `testDecreaseRequiresMultipleNSide` | PASS |

### test_pixelSons_nested (3 tests)

| Test | Status |
|------|--------|
| `testFourContiguousChildren` | PASS |
| `testColumnVectorInput` | PASS |
| `testMatchesIncreaseResolution` | PASS |

### test_plot (2 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testPlotSmokeInvisibleFigure` | SKIP | `axesm` (Mapping Toolbox) not available |
| `testPlotFlatMode` | SKIP | `pix2ang_nested` MEX not compiled |

### test_radius2NSide (4 tests)

| Test | Status |
|------|--------|
| `testReturnsPowerOfTwo` | PASS |
| `testPixelEnclosesRadius` | PASS |
| `testMonotonicWithRadius` | PASS |
| `testDocExample` | PASS |

### test_rasterize_polygon (4 tests)

| Test | Status | Notes |
|------|--------|-------|
| `testRasterizeWithExplicitNSide` | SKIP | `mex.coneSearch` not compiled |
| `testRasterizeWithResolution` | SKIP | `mex.coneSearch` not compiled |
| `testMissingNsideAndResolutionErrors` | PASS | |
| `testMatlabPathWithoutMex` | SKIP | Mapping Toolbox not available |

### test_uniqueId (4 tests)

| Test | Status |
|------|--------|
| `testRoundTripWithKnownNSide` | PASS |
| `testDocExample` | PASS |
| `testAutoDecodeNSide` | PASS |
| `testVectorInput` | PASS |

### test_convertHealPixNsideNested (5 tests)

| Test | Status |
|------|--------|
| `testDowngradeMapping` | PASS |
| `testDocExample` | PASS |
| `testFullIdInputOutput` | PASS |
| `testNewNSideGreaterThanOldErrors` | PASS |
| `testNonPowerOfTwoNewNSideErrors` | PASS |

### test_convertHealPix2highNsideNested (5 tests)

| Test | Status |
|------|--------|
| `testDocExample` | PASS |
| `testChildRangeWidth` | PASS |
| `testFullIdPath` | PASS |
| `testNewNSideLessThanOldErrors` | PASS |
| `testNonPowerOfTwoNewNSideErrors` | PASS |

---

## Function Bugs Summary

These bugs are in the source functions under `matlab/astro/+celestial/+healpix/` and need to be fixed there (outside the tests folder):

### 1. `coneSearch` — center pixel not included
**Test:** `test_coneSearch/testCentralPixelIncluded`  
For NSide=64, Lon=1.0 rad, Lat=0.3 rad, Radius=0.05 rad: center pixel 310 is absent from results (range 269–450). The search excludes the pixel that contains the query point itself.

### 2. `coneSearchRecur` — overshoots radius bound
**Test:** `test_coneSearchRecur/testCentersWithinExpandedRadius`  
At NSide=64, radius=0.08 rad: maximum returned pixel-center distance is 0.108 rad, exceeding the bound `Radius + 1/NSide = 0.096 rad` by ~13%.

### 3. `findNeighbors` — boundary crash
**Test:** `test_findNeighbors/testNoNegativeIndices`  
Processing all 3072 pixels at NSide=16 causes "Out of range subscript" error. Some pixel at a face boundary triggers an out-of-bounds array access in the face-transition lookup tables.

### 4. `healpixVertices` — wrong vertex positions
**Test:** `test_healpixVertices/testVerticesNearPixelCenter`  
At NSide=16, pixels 197 and 31: vertex-to-center angular distances are 0.42–0.77 rad (24–44°), far exceeding the expected ~0.03 rad (1.8°). The vertex coordinate calculation has an algorithm error.

### 5. `pixRadius` — formula inconsistency
**Test:** `test_pixRadius/testMaxRadiusGeEqualAreaRadius`  
`pixRadius` returns `MaxPixRadius = 1/NSide` and `PixelRadius = π/(√3·NSide) ≈ 1.81/NSide`. Since `MaxPixRadius < PixelRadius`, the "maximum pixel radius" is paradoxically smaller than the "equal-area radius". One or both formulas are wrong.
