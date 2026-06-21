function tests = test_healpixVertices
    % Unit tests for celestial.healpix.healpixVertices.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testOutputShapeAndLatitudeBounds(testCase)
    % Returns [Npix x 4] longitude/latitude vertex matrices in radians.
    NSide = 16;
    Pix = [197; 31];
    [CornerLon, CornerLat] = celestial.healpix.healpixVertices(NSide, Pix, 'nested');
    testCase.verifyEqual(size(CornerLon), [2, 4]);
    testCase.verifyEqual(size(CornerLat), [2, 4]);
    testCase.verifyLessThanOrEqual(CornerLat, pi/2);
    testCase.verifyGreaterThanOrEqual(CornerLat, -pi/2);
end

function testOutOfRangePixelErrors(testCase)
    % Pixel index outside [0, 12*NSide^2-1] must error.
    NSide = 16;
    BadPix = 12 * NSide^2;
    testCase.verifyError( ...
        @() celestial.healpix.healpixVertices(NSide, BadPix), ...
        'MATLAB:error');
end

function testInvalidTypeErrors(testCase)
    % Only nested and ring types are accepted.
    NSide = 8;
    Pix = 0;
    testCase.verifyError( ...
        @() celestial.healpix.healpixVertices(NSide, Pix, 'invalid'), ...
        'MATLAB:error');
end

function testVerticesNearPixelCenter(testCase)
    % Each vertex should be within ~1 pixel radius of the pixel center.
    HealpixTestHelper.assumeCoreAngPixMex(testCase);

    NSide = 16;
    Pix = [197; 31];
    [CornerLon, CornerLat] = celestial.healpix.healpixVertices(NSide, Pix, 'nested');
    [CenterLon, CenterLat] = celestial.healpix.pix2ang(NSide, Pix);

    MaxRad = celestial.healpix.pixRadius(NSide);
    for Ipix = 1:numel(Pix)
        for K = 1:4
            Dist = celestial.coo.sphere_dist_fast( ...
                CornerLon(Ipix, K), CornerLat(Ipix, K), ...
                CenterLon(Ipix), CenterLat(Ipix));
            testCase.verifyLessThan(Dist, 2 * MaxRad);
        end
    end
end
