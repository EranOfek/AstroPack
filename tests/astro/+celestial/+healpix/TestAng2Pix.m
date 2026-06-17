classdef TestAng2Pix < matlab.unittest.TestCase
    % TestAng2Pix  Unit tests for celestial.healpix.ang2pix.

    methods (Test)

        function testRoundTripNested(testCase)
            % ang2pix(pix2ang(p)) returns the same pixel (nested ordering).
            HealpixTestHelper.assumeCoreAngPixMex(testCase);

            NSide = 16;
            Pix = int64([0, 1, 100, 500, 1000]);
            [Lon, Lat] = celestial.healpix.pix2ang(NSide, Pix, 'Type', 'nested');
            Rebuilt = celestial.healpix.ang2pix(NSide, Lon, Lat, 'Type', 'nested');
            testCase.verifyEqual(int64(Rebuilt), Pix);
        end

        function testRoundTripRing(testCase)
            % ang2pix(pix2ang(p)) round-trip for ring ordering.
            HealpixTestHelper.assumeMex(testCase, 'ang2pix_ring');
            HealpixTestHelper.assumeMex(testCase, 'pix2ang_ring');

            NSide = 16;
            Pix = int64([0, 50, 200]);
            [Lon, Lat] = celestial.healpix.pix2ang(NSide, Pix, 'Type', 'ring');
            Rebuilt = celestial.healpix.ang2pix(NSide, Lon, Lat, 'Type', 'ring');
            testCase.verifyEqual(int64(Rebuilt), Pix);
        end

        function testCooUnitsDegMatchesRad(testCase)
            % CooUnits 'deg' and 'rad' produce identical pixel indices.
            HealpixTestHelper.assumeCoreAngPixMex(testCase);

            NSide = 8;
            LonDeg = 45.0;
            LatDeg = 30.0;
            PixDeg = celestial.healpix.ang2pix(NSide, LonDeg, LatDeg, ...
                'CooUnits', 'deg', 'Type', 'nested');
            PixRad = celestial.healpix.ang2pix(NSide, deg2rad(LonDeg), deg2rad(LatDeg), ...
                'Type', 'nested');
            testCase.verifyEqual(int64(PixDeg), int64(PixRad));
        end

        function testUniqueIdOption(testCase)
            % UniqueID=true returns pix2uniqueId encoding.
            HealpixTestHelper.assumeCoreAngPixMex(testCase);

            NSide = 16;
            Lon = 1.0;
            Lat = 0.5;
            Pix = celestial.healpix.ang2pix(NSide, Lon, Lat, 'UniqueID', true);
            Expected = celestial.healpix.pix2uniqueId(NSide, ...
                celestial.healpix.ang2pix(NSide, Lon, Lat));
            testCase.verifyEqual(Pix, Expected);
        end

        function testVectorInput(testCase)
            % Vector lon/lat yields vector pixel indices of the same size.
            HealpixTestHelper.assumeCoreAngPixMex(testCase);

            NSide = 8;
            Lon = [0, 0.5, 1.0, 1.5];
            Lat = [0, 0.2, -0.3, 0.4];
            Pix = celestial.healpix.ang2pix(NSide, Lon, Lat);
            testCase.verifyEqual(numel(Pix), numel(Lon));
        end

    end
end
