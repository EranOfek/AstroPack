function tests = test_pixelSons_nested
    % Unit tests for celestial.healpix.pixelSons_nested.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testFourContiguousChildren(testCase)
    % Each parent pixel maps to four consecutive child indices.
    % pixelSons_nested uses (0:1:3) double vector internally, so Pix must be double.
    NSide = 16;  % unused by formula but kept for API consistency
    Pix = [0; 1; 100];  % double column vector
    Sons = celestial.healpix.pixelSons_nested(NSide, Pix);
    testCase.verifySize(Sons, [3, 4]);
    for I = 1:numel(Pix)
        Expected = 4 * Pix(I) + (0:3);
        testCase.verifyEqual(double(Sons(I, :)), Expected);
    end
end

function testColumnVectorInput(testCase)
    % Input column vector yields one row of sons per pixel.
    Pix = [0; 5; 10];  % double column vector
    Sons = celestial.healpix.pixelSons_nested(8, Pix);
    testCase.verifyEqual(size(Sons), [3, 4]);
end

function testMatchesIncreaseResolution(testCase)
    % Sons at next level match increasePixelResolution for same parent.
    Ipix0 = 50;
    NSide0 = 8;
    NSide1 = 16;
    Sons = celestial.healpix.pixelSons_nested(NSide0, Ipix0);  % double scalar, compatible with internal (0:1:3)
    Increased = celestial.healpix.increasePixelResolution(Ipix0, NSide0, NSide1);
    testCase.verifyEqual(double(Sons(:)), double(Increased(:)));
end
