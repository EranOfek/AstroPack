function tests = test_pix2ang
    % Unit tests for celestial.healpix.pix2ang.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testCentersInValidRanges(testCase)
    % Pixel centers lie in lon [-pi,pi] (wrapped) and lat [-pi/2,pi/2].
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 16;
    Pix = int64(0:(12 * NSide^2 - 1));
    [Lon, Lat] = celestial.healpix.pix2ang(NSide, Pix);
    testCase.verifyLessThanOrEqual(Lat, pi/2);
    testCase.verifyGreaterThanOrEqual(Lat, -pi/2);
    testCase.verifyLessThanOrEqual(abs(Lon), pi);
end

function testRoundTripToAng2Pix(testCase)
    % Centers map back to the same pixel via ang2pix.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 32;
    Pix = int64([0, 10, 100, 500, 2000]);
    [Lon, Lat] = celestial.healpix.pix2ang(NSide, Pix);
    Rebuilt = celestial.healpix.ang2pix(NSide, Lon, Lat);
    testCase.verifyEqual(int64(Rebuilt), Pix);
end

function testCooUnitsDegScaling(testCase)
    % CooUnits 'deg' scales output by 180/pi relative to radians.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 16;
    Pix = int64(197);
    [LonRad, LatRad] = celestial.healpix.pix2ang(NSide, Pix, 'CooUnits', 'rad');
    [LonDeg, LatDeg] = celestial.healpix.pix2ang(NSide, Pix, 'CooUnits', 'deg');
    testCase.verifyEqual(LonDeg, LonRad * 180/pi, 'AbsTol', 1e-10);
    testCase.verifyEqual(LatDeg, LatRad * 180/pi, 'AbsTol', 1e-10);
end

function testUniqueIdInput(testCase)
    % UniqueID=true decodes FullID before coordinate lookup.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 16;
    Pix = int64(100);
    UniqueId = celestial.healpix.pix2uniqueId(NSide, Pix);
    [Lon1, Lat1] = celestial.healpix.pix2ang(NSide, Pix);
    [Lon2, Lat2] = celestial.healpix.pix2ang(NSide, UniqueId, 'UniqueID', true);
    testCase.verifyEqual(Lon1, Lon2, 'AbsTol', 1e-12);
    testCase.verifyEqual(Lat1, Lat2, 'AbsTol', 1e-12);
end
