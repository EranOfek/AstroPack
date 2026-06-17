function tests = test_coneSearch2PixRanges
    % Unit tests for coneSearch2PixRanges.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testNeighbAlgoOutputShape(testCase)
    % Default 'neighb' algo returns inclusive int64 [N x 2] ranges.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    RA = 1.0;
    Dec = 0.5;
    SearchRadius = 1 / 1024;
    NSideCat = 2^16;
    PixRanges = celestial.healpix.coneSearch2PixRanges(RA, Dec, SearchRadius, NSideCat);
    testCase.verifyEqual(size(PixRanges, 2), 2);
    testCase.verifyGreaterThan(size(PixRanges, 1), 0);
    testCase.verifyEqual(class(PixRanges), 'int64');
    testCase.verifyLessThanOrEqual(PixRanges(:, 1), PixRanges(:, 2));
end

function testRangeWidthMatchesChildCount(testCase)
    % Each range spans (NSideCat/NSideSearch)^2 nested child pixels.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    RA = 1.0;
    Dec = 0.5;
    SearchRadius = 0.01;
    NSideCat = 2^16;
    PixRanges = celestial.healpix.coneSearch2PixRanges(RA, Dec, SearchRadius, NSideCat);

    NSideSearch = 2.^floor(log2(1 / SearchRadius));
    NSideSearch = min(max(NSideSearch, 1), NSideCat);
    ExpectedWidth = (NSideCat / NSideSearch)^2;
    Widths = double(PixRanges(:, 2) - PixRanges(:, 1) + 1);
    testCase.verifyEqual(Widths, ExpectedWidth);
end

function testCentralCatalogPixelCovered(testCase)
    % Catalog pixel at center lies inside one returned range.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    RA = 0.5;
    Dec = 0.25;
    SearchRadius = 1 / 512;
    NSideCat = 2^14;
    PixRanges = celestial.healpix.coneSearch2PixRanges(RA, Dec, SearchRadius, NSideCat);
    CenterPix = double(celestial.healpix.ang2pix(NSideCat, RA, Dec));
    Covered = any(CenterPix >= PixRanges(:, 1) & CenterPix <= PixRanges(:, 2));
    testCase.verifyTrue(Covered);
end

function testConeAlgoWhenExternalMexAvailable(testCase)
    % 'cone' algorithm uses mex.coneSearch when the external MEX exists.
    HealpixTestHelper.assumeMex(testCase, 'coneSearch');
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    RA = 1.0;
    Dec = 0.5;
    SearchRadius = 1 / 1024;
    NSideCat = 2^12;
    PixRanges = celestial.healpix.coneSearch2PixRanges(RA, Dec, SearchRadius, NSideCat, ...
        'Algo', 'cone');
    testCase.verifyGreaterThan(size(PixRanges, 1), 0);
end

function testNonPowerOfTwoCatalogErrors(testCase)
    % NSideCat must be a power of two.
    testCase.verifyError( ...
        @() celestial.healpix.coneSearch2PixRanges(1, 0, 0.01, 6), ...
        'MATLAB:error');
end

function testNonScalarCenterErrors(testCase)
    % RA, Dec, and SearchRadius must be scalars.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);
    testCase.verifyError( ...
        @() celestial.healpix.coneSearch2PixRanges([1, 2], 0.5, 0.01, 2^16), ...
        'MATLAB:error');
end
