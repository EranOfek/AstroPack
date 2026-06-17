function tests = test_pixRadius
    % Unit tests for celestial.healpix.pixRadius.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testEqualAreaRadiusFormula(testCase)
    % PixelRadius is the radius of a circle with area equal to the pixel.
    NSide = 16;
    [PixelRadius, MaxPixRadius] = celestial.healpix.pixRadius(NSide);
    ExpectedRadius = pi / (sqrt(3) * NSide);
    testCase.verifyEqual(PixelRadius, ExpectedRadius, 'AbsTol', 1e-12);
    testCase.verifyEqual(MaxPixRadius, 1 / NSide, 'AbsTol', 1e-12);
end

function testMaxRadiusGeEqualAreaRadius(testCase)
    % The enclosing radius must be at least the equal-area radius.
    NSideList = [2, 4, 8, 16, 64, 256];
    for NSide = NSideList
        [PixelRadius, MaxPixRadius] = celestial.healpix.pixRadius(NSide);
        testCase.verifyGreaterThanOrEqual(MaxPixRadius, PixelRadius);
    end
end

function testRadiusDecreasesWithResolution(testCase)
    % Higher NSide yields smaller pixel radii.
    [R8] = celestial.healpix.pixRadius(8);
    [R16] = celestial.healpix.pixRadius(16);
    testCase.verifyGreaterThan(R8, R16);
end
