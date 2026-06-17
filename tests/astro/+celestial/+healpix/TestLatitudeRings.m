classdef TestLatitudeRings < matlab.unittest.TestCase
    % TestLatitudeRings  Unit tests for celestial.healpix.latitudeRings.

    methods (Test)

        function testOutputSizes(testCase)
            % Returns 4*NSide+1 ring latitudes and matching pixel counts.
            NSide = 16;
            [Lat, Npix] = celestial.healpix.latitudeRings(NSide);
            ExpectedCount = 4 * NSide + 1;
            testCase.verifyEqual(numel(Lat), ExpectedCount);
            testCase.verifyEqual(numel(Npix), ExpectedCount);
        end

        function testLatitudesMonotonicAndBounded(testCase)
            % Ring latitudes decrease from north pole to south pole.
            NSide = 8;
            Lat = celestial.healpix.latitudeRings(NSide);
            testCase.verifyLessThanOrEqual(Lat, pi/2);
            testCase.verifyGreaterThanOrEqual(Lat, -pi/2);
            testCase.verifyEqual(Lat(1), pi/2, 'AbsTol', 1e-12);
            testCase.verifyEqual(Lat(end), -pi/2, 'AbsTol', 1e-12);
            testCase.verifyEqual(diff(Lat) <= 0, true(1, numel(Lat) - 1));
        end

        function testPolesHaveZeroPixels(testCase)
            % North and south pole rings contain zero pixels.
            NSide = 16;
            [~, Npix] = celestial.healpix.latitudeRings(NSide);
            testCase.verifyEqual(Npix(1), 0);
            testCase.verifyEqual(Npix(end), 0);
        end

        function testTotalPixelCountMatchesNPix(testCase)
            % Summing ring pixel counts equals total HEALPix pixel count.
            NSide = 16;
            [~, Npix] = celestial.healpix.latitudeRings(NSide);
            testCase.verifyEqual(sum(Npix), celestial.healpix.nPix(NSide));
        end

        function testEquatorialRingHasFullCount(testCase)
            % Mid-equatorial rings hold 4*NSide pixels each.
            NSide = 8;
            [~, Npix] = celestial.healpix.latitudeRings(NSide);
            Equatorial = Npix(NSide + 1 : 3 * NSide);
            testCase.verifyEqual(Equatorial, 4 * NSide);
        end

    end
end
