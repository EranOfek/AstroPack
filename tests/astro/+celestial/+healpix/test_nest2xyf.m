function tests = test_nest2xyf
    % Unit tests for nest2xyf and xyf2nest (NESTED Morton coding).

    tests = functiontests(localfunctions);
end

%% Test Functions

function testRoundTripAllPixelsNSide8(testCase)
    % Full round-trip over every pixel at NSide=8 (768 pixels).
    % Mirrors the check in celestial.healpix.unitTest.
    NSide = 8;
    Pix = int64(0:(12 * NSide^2 - 1));
    [X, Y, Face] = celestial.healpix.nest2xyf(NSide, Pix);
    Rebuilt = celestial.healpix.xyf2nest(NSide, X, Y, Face);
    testCase.verifyEqual(int64(Rebuilt), Pix);
end

function testCoordinatesInValidRange(testCase)
    % Decoded face/X/Y must lie in standard HEALPix nested ranges.
    NSide = 16;
    Pix = int64([0, 100, 500, 1000]);
    [X, Y, Face] = celestial.healpix.nest2xyf(NSide, Pix);
    testCase.verifyLessThanOrEqual(X, NSide - 1);
    testCase.verifyLessThanOrEqual(Y, NSide - 1);
    testCase.verifyLessThanOrEqual(Face, 11);
end

function testCheckRangeRejectsOutOfBounds(testCase)
    % nest2xyf with CheckRange=true errors on invalid pixel index.
    NSide = 8;
    BadPix = int64(12 * NSide^2);  % one past last valid index
    testCase.verifyError( ...
        @() celestial.healpix.nest2xyf(NSide, BadPix, 'CheckRange', true), ...
        ?MException);
end

function testNonPowerOfTwoNSideErrors(testCase)
    % Standard nested Morton coding requires NSide to be a power of two.
    NSide = 6;
    Pix = int64(0);
    testCase.verifyError( ...
        @() celestial.healpix.nest2xyf(NSide, Pix), ...
        ?MException);
end

function testScalarRoundTrip(testCase)
    % Single-pixel encode/decode is exact.
    NSide = 32;
    Pix = int64(197);
    [X, Y, Face] = celestial.healpix.nest2xyf(NSide, Pix);
    Rebuilt = celestial.healpix.xyf2nest(NSide, X, Y, Face);
    testCase.verifyEqual(int64(Rebuilt), Pix);
end
