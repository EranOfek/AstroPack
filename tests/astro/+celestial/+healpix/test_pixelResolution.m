function tests = test_pixelResolution
    % Unit tests for increase/decreasePixelResolution.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testIncreaseThenDecreaseRoundTrip(testCase)
    % Downgrading after upgrading recovers the original parent index.
    Ipix0 = 36136;
    NSide0 = 2^7;
    NSide1 = 2^8;
    Increased = celestial.healpix.increasePixelResolution(Ipix0, NSide0, NSide1);
    Decreased = celestial.healpix.decreasePixelResolution(Increased, NSide1, NSide0);
    UniqueParents = unique(Decreased);
    testCase.verifyEqual(UniqueParents, Ipix0);
end

function testIncreaseChildRangeContiguous(testCase)
    % Children of one parent form a contiguous index block.
    Ipix0 = 10;
    NSide0 = 8;
    NSide1 = 32;
    Ratio = (NSide1 / NSide0)^2;
    Increased = celestial.healpix.increasePixelResolution(Ipix0, NSide0, NSide1);
    ExpectedFirst = Ipix0 * Ratio;
    ExpectedLast = (Ipix0 + 1) * Ratio - 1;
    testCase.verifyEqual(Increased(1), ExpectedFirst);
    testCase.verifyEqual(Increased(end), ExpectedLast);
    testCase.verifyEqual(numel(Increased), Ratio);
end

function testDecreaseDocExample(testCase)
    % Doc example indices collapse to a single parent at lower NSide.
    Ipix0 = [144545, 144546, 144544, 144547];
    NSide0 = 2^8;
    NSide1 = 2^7;
    Decreased = celestial.healpix.decreasePixelResolution(Ipix0, NSide0, NSide1);
    testCase.verifyEqual(unique(Decreased), floor(144545 / 4));
end

function testIncreaseRequiresMultipleNSide(testCase)
    % NSide1 must be an integer multiple of NSide0.
    testCase.verifyError( ...
        @() celestial.healpix.increasePixelResolution(0, 8, 12), ...
        ?MException);
end

function testDecreaseRequiresMultipleNSide(testCase)
    % NSide0 must be an integer multiple of NSide1.
    testCase.verifyError( ...
        @() celestial.healpix.decreasePixelResolution(0, 12, 8), ...
        ?MException);
end
