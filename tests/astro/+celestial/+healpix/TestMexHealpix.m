classdef TestMexHealpix < matlab.unittest.TestCase
    % TestMexHealpix  Direct unit tests for celestial.healpix.mex wrappers.

    methods (Test)

        function testAng2PixNestedRoundTrip(testCase)
            % mex.ang2pix_nested and mex.pix2ang_nested are mutually inverse.
            HealpixTestHelper.assumeMex(testCase, 'ang2pix_nested');
            HealpixTestHelper.assumeMex(testCase, 'pix2ang_nested');

            NSide = 16;
            Pix = int64([0, 10, 100, 500]);
            [Lon, Lat] = celestial.healpix.mex.pix2ang_nested(NSide, double(Pix));
            Rebuilt = celestial.healpix.mex.ang2pix_nested(NSide, Lon, Lat);
            testCase.verifyEqual(int64(Rebuilt), Pix);
        end

        function testAng2PixRingRoundTrip(testCase)
            % mex.ang2pix_ring and mex.pix2ang_ring are mutually inverse.
            HealpixTestHelper.assumeMex(testCase, 'ang2pix_ring');
            HealpixTestHelper.assumeMex(testCase, 'pix2ang_ring');

            NSide = 16;
            Pix = int64([0, 50, 200]);
            [Lon, Lat] = celestial.healpix.mex.pix2ang_ring(NSide, double(Pix));
            Rebuilt = celestial.healpix.mex.ang2pix_ring(NSide, Lon, Lat);
            testCase.verifyEqual(int64(Rebuilt), Pix);
        end

        function testAng2PixNestedInt64Output(testCase)
            % ang2pix_nested returns int64 pixel indices.
            HealpixTestHelper.assumeMex(testCase, 'ang2pix_nested');

            NSide = 8;
            Lon = [0, 0.5, 1.0];
            Lat = [0, 0.2, -0.1];
            Pix = celestial.healpix.mex.ang2pix_nested(NSide, Lon, Lat);
            testCase.verifyEqual(class(Pix), 'int64');
            testCase.verifyEqual(numel(Pix), numel(Lon));
        end

        function testNestedVsRingMayDiffer(testCase)
            % Nested and ring orderings assign different indices to same sky point.
            HealpixTestHelper.assumeMex(testCase, 'ang2pix_nested');
            HealpixTestHelper.assumeMex(testCase, 'ang2pix_ring');

            NSide = 16;
            Lon = 1.0;
            Lat = 0.3;
            PixNest = celestial.healpix.mex.ang2pix_nested(NSide, Lon, Lat);
            PixRing = celestial.healpix.mex.ang2pix_ring(NSide, Lon, Lat);
            % They need not match; just verify both are valid indices.
            testCase.verifyGreaterThanOrEqual(PixNest, 0);
            testCase.verifyGreaterThanOrEqual(PixRing, 0);
            testCase.verifyLessThan(PixNest, 12 * NSide^2);
            testCase.verifyLessThan(PixRing, 12 * NSide^2);
        end

        function testNeighborsNestedSmoke(testCase)
            % mex.neighbors_nested returns int64 neighbor indices (external lib).
            HealpixTestHelper.assumeMex(testCase, 'neighbors_nested');

            NSide = 256;
            Pix = int64(20567);
            Neigh = celestial.healpix.mex.neighbors_nested(NSide, Pix);
            testCase.verifyEqual(size(Neigh), [8, 1]);
            testCase.verifyGreaterThanOrEqual(min(Neigh), 0);
        end

        function testConeSearchMexSmoke(testCase)
            % mex.coneSearch returns pixels for a small cone (external lib).
            HealpixTestHelper.assumeMex(testCase, 'coneSearch');

            NSide = 1024;
            Ind = celestial.healpix.mex.coneSearch(NSide, 1.0, 1.0, 0.1);
            testCase.verifyGreaterThan(numel(Ind), 0);
        end

        function testRasterizePolygonMexSmoke(testCase)
            % mex.rasterize_polygon rasterizes a small sky polygon (external lib).
            HealpixTestHelper.assumeMex(testCase, 'rasterize_polygon');

            P0 = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
            [Ind, NSideOut] = celestial.healpix.mex.rasterize_polygon(P0, 3);
            testCase.verifyGreaterThan(numel(Ind), 0);
            testCase.verifyGreaterThan(NSideOut, 0);
        end

    end
end
