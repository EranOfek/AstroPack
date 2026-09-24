function tests = test_coneSearch
    % Unit tests for celestial.healpix.coneSearch.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testReturnsUniquePixels(testCase)
    % Result is a column vector of unique pixel indices.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeMappingToolbox(testCase);

    NSide = 2^8;
    Lon = deg2rad(200.67);
    Lat = deg2rad(50.4);
    Radius = deg2rad(10);
    Result = celestial.healpix.coneSearch(NSide, Lon, Lat, Radius);
    testCase.verifyGreaterThan(numel(Result), 0);
    testCase.verifyEqual(numel(Result), numel(unique(Result)));
end

function testCentralPixelIncluded(testCase)
    % Cone center maps to a pixel that appears in the search result.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeMappingToolbox(testCase);

    NSide = 64;
    Lon = 1.0;
    Lat = 0.3;
    Radius = 0.05;
    CenterPix = celestial.healpix.ang2pix(NSide, Lon, Lat);
    Result = celestial.healpix.coneSearch(NSide, Lon, Lat, Radius);
    testCase.verifyTrue(any(int64(Result) == int64(CenterPix)));
end

function testPixelCentersWithinExpandedRadius(testCase)
    % All returned centers lie within Radius + pixel radius.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeMappingToolbox(testCase);

    NSide = 32;
    Lon = 0.8;
    Lat = 0.2;
    Radius = 0.1;
    [PixelRadius] = celestial.healpix.pixRadius(NSide);
    Result = celestial.healpix.coneSearch(NSide, Lon, Lat, Radius);
    [PixLon, PixLat] = celestial.healpix.pix2ang(NSide, Result);
    Dist = celestial.coo.sphere_dist_fast(PixLon, PixLat, Lon, Lat);
    testCase.verifyLessThanOrEqual(max(Dist), Radius + PixelRadius + 1e-6);
end

function testDegreeUnits(testCase)
    % CooUnits and RadiusUnits 'deg' match radian equivalent search.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeMappingToolbox(testCase);

    NSide = 128;
    RA = 200.67;
    Dec = 50.4;
    RadDeg = 5.0;
    Rdeg = celestial.healpix.coneSearch(NSide, RA, Dec, RadDeg, ...
        'CooUnits', 'deg', 'RadiusUnits', 'deg');
    Rrad = celestial.healpix.coneSearch(NSide, deg2rad(RA), deg2rad(Dec), deg2rad(RadDeg));
    testCase.verifyEqual(sort(int64(Rdeg)), sort(int64(Rrad)));
end
