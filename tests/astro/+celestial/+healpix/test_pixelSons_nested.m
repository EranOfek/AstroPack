function tests = test_pixelSons_nested
    % Unit tests for celestial.healpix.pixelSons_nested.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testFourContiguousChildren(testCase)
    % Each parent pixel maps to four consecutive child indices.
    NSide = 16;  % unused by formula but kept for API consistency
    Pix = int64([0; 1; 100]);
    Sons = celestial.healpix.pixelSons_nested(NSide, Pix);
    testCase.verifySize(Sons, [3, 4]);
    for I = 1:numel(Pix)
        Expected = 4 * double(Pix(I)) + (0:3);
        testCase.verifyEqual(double(Sons(I, :)), Expected);
    end
end

function testColumnVectorInput(testCase)
    % Input column vector yields one row of sons per pixel.
    Pix = int64([0; 5; 10]);
    Sons = celestial.healpix.pixelSons_nested(8, Pix);
    testCase.verifyEqual(size(Sons), [3, 4]);
end

function testMatchesIncreaseResolution(testCase)
    % Sons at next level match increasePixelResolution for same parent.
    Ipix0 = 50;
    NSide0 = 8;
    NSide1 = 16;
    Sons = celestial.healpix.pixelSons_nested(NSide0, int64(Ipix0));
    Increased = celestial.healpix.increasePixelResolution(Ipix0, NSide0, NSide1);
    testCase.verifyEqual(double(Sons), double(Increased));
end
