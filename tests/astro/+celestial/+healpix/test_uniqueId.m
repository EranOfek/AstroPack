function tests = test_uniqueId
    % Unit tests for pix2uniqueId and uniqueId2pix.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testRoundTripWithKnownNSide(testCase)
    % UniqueId = Pix + 4*Nside^2 and decodes back to the same pixel.
    NSide = 16;
    Pix = int64([0, 1, 100, 500]);
    UniqueId = celestial.healpix.pix2uniqueId(NSide, Pix);
    Expected = Pix + 4 * NSide^2;
    testCase.verifyEqual(UniqueId, Expected);
    [DecodedNSide, DecodedPix] = celestial.healpix.uniqueId2pix(NSide, UniqueId);
    testCase.verifyEqual(DecodedNSide, NSide);
    testCase.verifyEqual(DecodedPix, Pix);
end

function testDocExample(testCase)
    % Doc example: uniqueId2pix([], 1025) then pix2uniqueId round-trip.
    [NSide, Pix] = celestial.healpix.uniqueId2pix([], 1025);
    Rebuilt = celestial.healpix.pix2uniqueId(NSide, Pix);
    testCase.verifyEqual(Rebuilt, 1025);
end

function testAutoDecodeNSide(testCase)
    % Empty NSide triggers automatic order decoding from FullID.
    FullId = 4 * 16^2 + 1234;
    [NSide, Pix] = celestial.healpix.uniqueId2pix([], FullId);
    testCase.verifyEqual(NSide, 16);
    testCase.verifyEqual(Pix, 1234);
end

function testVectorInput(testCase)
    % Both functions accept vector pixel indices.
    NSide = 8;
    Pix = int64([0, 10, 20, 30]);
    UniqueId = celestial.healpix.pix2uniqueId(NSide, Pix);
    [~, DecodedPix] = celestial.healpix.uniqueId2pix(NSide, UniqueId);
    testCase.verifyEqual(DecodedPix, Pix);
end
