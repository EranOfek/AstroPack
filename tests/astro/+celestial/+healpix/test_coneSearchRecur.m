function tests = test_coneSearchRecur
    % Unit tests for celestial.healpix.coneSearchRecur.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testReturnsNonEmptyUniqueSet(testCase)
    % Recursive search returns unique pixel indices.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 2^8;
    Lon = deg2rad(200.67);
    Lat = deg2rad(50.4);
    Radius = deg2rad(10);
    Result = celestial.healpix.coneSearchRecur(NSide, Lon, Lat, Radius);
    testCase.verifyGreaterThan(numel(Result), 0);
    testCase.verifyEqual(numel(Result), numel(unique(Result)));
end

function testOverlapsFastConeSearch(testCase)
    % Recursive and annulus-based coneSearch share most pixels.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeMappingToolbox(testCase);

    NSide = 2^8;
    RA = 200.67;
    Dec = 50.4;
    RadDeg = 10;
    R1 = celestial.healpix.coneSearchRecur(NSide, RA, Dec, RadDeg, ...
        'RadiusUnits', 'deg', 'CooUnits', 'deg');
    R2 = celestial.healpix.coneSearch(NSide, RA, Dec, RadDeg, ...
        'RadiusUnits', 'deg', 'CooUnits', 'deg');

    % Both are inclusive supersets; expect substantial overlap.
    Overlap = numel(intersect(int64(R1), int64(R2)));
    testCase.verifyGreaterThan(Overlap, 0.8 * min(numel(R1), numel(R2)));
end

function testCentersWithinExpandedRadius(testCase)
    % Returned pixel centers lie within Radius + enclosing pixel radius.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 64;
    Lon = 1.2;
    Lat = -0.2;
    Radius = 0.08;
    MaxRad = 1 / NSide;
    Result = celestial.healpix.coneSearchRecur(NSide, Lon, Lat, Radius);
    [PixLon, PixLat] = celestial.healpix.pix2ang(NSide, Result);
    Dist = celestial.coo.sphere_dist_fast(PixLon, PixLat, Lon, Lat);
    testCase.verifyLessThanOrEqual(max(Dist), Radius + MaxRad + 1e-6);
end

function testDegreeUnits(testCase)
    % Degree inputs match radian inputs after unit conversion.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 32;
    RA = 100.0;
    Dec = 20.0;
    RadDeg = 3.0;
    Rdeg = celestial.healpix.coneSearchRecur(NSide, RA, Dec, RadDeg, ...
        'CooUnits', 'deg', 'RadiusUnits', 'deg');
    Rrad = celestial.healpix.coneSearchRecur(NSide, deg2rad(RA), deg2rad(Dec), deg2rad(RadDeg));
    testCase.verifyEqual(sort(int64(Rdeg)), sort(int64(Rrad)));
end
