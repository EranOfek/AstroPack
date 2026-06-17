classdef TestFindNeighbors < matlab.unittest.TestCase
    % TestFindNeighbors  Unit tests for celestial.healpix.findNeighbors.

    methods (Test)

        function testOutputShapeEightNeighbors(testCase)
            % Default output is 8 rows (neighbors only) x numel(Pix) columns.
            NSide = 8;
            Pix = int64([0, 10, 100]);
            Neigh = celestial.healpix.findNeighbors(NSide, Pix);
            testCase.verifyEqual(size(Neigh), [8, numel(Pix)]);
        end

        function testIncludeSelfAddsRow(testCase)
            % IncludeSelf=true appends the central pixel as the 9th row.
            NSide = 8;
            Pix = int64(50);
            Neigh = celestial.healpix.findNeighbors(NSide, Pix, 'IncludeSelf', true);
            testCase.verifyEqual(size(Neigh), [9, 1]);
            testCase.verifyEqual(Neigh(9, 1), Pix);
        end

        function testNoNegativeIndices(testCase)
            % All neighbor indices must be valid (non-negative after fill).
            NSide = 16;
            Pix = int64(0:(12 * NSide^2 - 1));
            Neigh = celestial.healpix.findNeighbors(NSide, Pix);
            testCase.verifyGreaterThanOrEqual(min(Neigh(:)), 0);
        end

        function testNeighborAngularDistance(testCase)
            % Neighbor centers lie within a few pixel radii of the source center.
            % Requires pix2ang MEX for coordinate lookup.
            HealpixTestHelper.assumeCoreAngPixMex(testCase);

            NSide = 256;
            Pix = int64(1:1000);
            Neigh = celestial.healpix.findNeighbors(NSide, Pix);
            [Lon0, Lat0] = celestial.healpix.pix2ang(NSide, Pix);
            Lon0 = Lon0(:).';
            Lat0 = Lat0(:).';

            [Lon, Lat] = celestial.healpix.pix2ang(NSide, Neigh(:));
            Lon = reshape(Lon, size(Neigh));
            Lat = reshape(Lat, size(Neigh));

            DistDeg = celestial.coo.sphere_dist_fast(Lon, Lat, Lon0, Lat0) * 180/pi * 3600;
            MinDist = min(DistDeg, [], 'all');
            MaxDist = max(DistDeg, [], 'all');

            % Matches tolerances used in celestial.healpix.unitTest.
            testCase.verifyGreaterThan(MinDist, 3);
            testCase.verifyLessThan(MaxDist, 5);
        end

        function testMatchesMexNeighborsWhenAvailable(testCase)
            % MATLAB findNeighbors agrees with mex.neighbors_nested when compiled.
            HealpixTestHelper.assumeMex(testCase, 'neighbors_nested');
            HealpixTestHelper.assumeCoreAngPixMex(testCase);

            NSide = 256;
            Pix = int64(1:500);
            NeighMatlab = celestial.healpix.findNeighbors(NSide, Pix);
            NeighMex = celestial.healpix.mex.neighbors_nested(NSide, Pix);
            testCase.verifyEqual(NeighMatlab, NeighMex);
        end

    end
end
