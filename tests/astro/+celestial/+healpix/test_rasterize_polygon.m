function tests = test_rasterize_polygon
    % Unit tests for celestial.healpix.rasterize_polygon.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testRasterizeWithExplicitNSide(testCase)
    % Returns pixels inside polygon when mex.coneSearch and helpers exist.
    HealpixTestHelper.assumeMex(testCase, 'coneSearch');
    HealpixTestHelper.assumeFunctionExists(testCase, 'celestial.search.isPointInsidePolygon');
    HealpixTestHelper.assumeFunctionExists(testCase, 'celestial.polygon.spherical_polygon_circum_circle');

    P = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    NSide = 2^14;
    [Result, NSideOut] = celestial.healpix.rasterize_polygon(P, 'Nside', NSide, 'UseMex', true);
    testCase.verifyEqual(NSideOut, NSide);
    testCase.verifyGreaterThan(numel(Result), 0);

    % All returned pixel centers should lie inside the polygon.
    [PixLon, PixLat] = celestial.healpix.pix2ang(NSide, Result, 'CooUnits', 'deg');
    Inside = celestial.search.isPointInsidePolygon(PixLon, PixLat, P);
    testCase.verifyEqual(Inside, true(numel(Result), 1));
end

function testRasterizeWithResolution(testCase)
    % Resolution [arcsec] selects an appropriate NSide automatically.
    HealpixTestHelper.assumeMex(testCase, 'coneSearch');
    HealpixTestHelper.assumeFunctionExists(testCase, 'celestial.search.isPointInsidePolygon');
    HealpixTestHelper.assumeFunctionExists(testCase, 'celestial.polygon.spherical_polygon_circum_circle');

    P = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    [Result, NSideOut] = celestial.healpix.rasterize_polygon(P, 'Resolution', 5, 'UseMex', true);
    testCase.verifyGreaterThan(NSideOut, 0);
    testCase.verifyGreaterThan(numel(Result), 0);
end

function testMissingNsideAndResolutionErrors(testCase)
    % Either Nside or Resolution must be provided.
    P = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    testCase.verifyError( ...
        @() celestial.healpix.rasterize_polygon(P), ...
        'MATLAB:error');
end

function testMatlabPathWithoutMex(testCase)
    % UseMex=false falls back to MATLAB coneSearch when Mapping Toolbox exists.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);
    HealpixTestHelper.assumeMappingToolbox(testCase);
    HealpixTestHelper.assumeFunctionExists(testCase, 'celestial.search.isPointInsidePolygon');
    HealpixTestHelper.assumeFunctionExists(testCase, 'celestial.polygon.spherical_polygon_circum_circle');

    P = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    NSide = 2^12;
    [Result, NSideOut] = celestial.healpix.rasterize_polygon(P, ...
        'Nside', NSide, 'UseMex', false);
    testCase.verifyEqual(NSideOut, NSide);
    testCase.verifyGreaterThan(numel(Result), 0);
end
