function tests = test_healpix_01
    % Integration smoke test for celestial.healpix cone search functions.
    %
    % Converted from the original unitTest.m script. Tests that coneSearchRecur
    % and coneSearch return non-empty, consistent results.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testConeSearchIntegration(testCase)
    % coneSearch and coneSearchRecur return non-empty pixel lists.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeMappingToolbox(testCase);

    RA    = 200.67;
    Dec   = 50.4;
    NSide = 2^8;
    Rad   = 10;

    R1 = celestial.healpix.coneSearchRecur(NSide, RA, Dec, Rad, ...
        'RadiusUnits', 'deg', 'CooUnits', 'deg');
    R2 = celestial.healpix.coneSearch(NSide, RA, Dec, Rad, ...
        'RadiusUnits', 'deg', 'CooUnits', 'deg');

    testCase.verifyGreaterThan(numel(R1), 0, 'coneSearchRecur returned empty result.');
    testCase.verifyGreaterThan(numel(R2), 0, 'coneSearch returned empty result.');
end

function testConeSearchOverlap(testCase)
    % coneSearchRecur and coneSearch share substantial pixel overlap.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeMappingToolbox(testCase);

    RA    = 200.67;
    Dec   = 50.4;
    NSide = 2^8;
    Rad   = 10;

    R1 = celestial.healpix.coneSearchRecur(NSide, RA, Dec, Rad, ...
        'RadiusUnits', 'deg', 'CooUnits', 'deg');
    R2 = celestial.healpix.coneSearch(NSide, RA, Dec, Rad, ...
        'RadiusUnits', 'deg', 'CooUnits', 'deg');

    Overlap = numel(intersect(int64(R1), int64(R2)));
    testCase.verifyGreaterThan(Overlap, 0.5 * min(numel(R1), numel(R2)), ...
        'coneSearchRecur and coneSearch results have insufficient overlap.');
end
