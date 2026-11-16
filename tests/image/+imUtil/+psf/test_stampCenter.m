function tests = test_stampCenter
    % imUtil.psf.stampCenter: the light center of PSF stamps as an offset from the stamp center
    %   'centroid' is the first moment; 'core' follows the core and not a faint offset wing;
    %   offsets are in detector pixels (samples / Oversample), for cubes and cells of any sizes.
    tests = functiontests(localfunctions);
end

function K = gauss(N, D1, D2, Sigma)
    % a Gaussian centered D1, D2 samples off the center of an N x N stamp
    C = (N + 1) / 2;
    [I2, I1] = meshgrid(1:N, 1:N);
    K = exp(-((I1 - C - D1).^2 + (I2 - C - D2).^2) / (2 * Sigma^2));
end

function testCentroidOfShiftedGaussians(testCase)
    Cube = cat(3, gauss(41, 1.5, -2.25, 2), gauss(41, 0, 0, 3), gauss(41, -3, 0.5, 2.5));
    [D1, D2] = imUtil.psf.stampCenter(Cube);
    verifyEqual(testCase, [D1 D2], [1.5 -2.25; 0 0; -3 0.5], 'AbsTol', 1e-6);
end

function testOversampleAndEvenSize(testCase)
    % an even-sized stamp: the center is between two samples
    K = gauss(40, 0.5, 0.5, 3);
    [D1, D2] = imUtil.psf.stampCenter(K, 'Oversample', 5);
    verifyEqual(testCase, [D1 D2], [0.1 0.1], 'AbsTol', 1e-6);
end

function testCoreIgnoresWing(testCase)
    % a core at (1, -2) and a faint, broad wing far off: the first moment moves, the core does not
    K = gauss(61, 1, -2, 1.5) + 0.05 * gauss(61, 15, 10, 5);
    [C1, C2] = imUtil.psf.stampCenter(K);
    [K1, K2] = imUtil.psf.stampCenter(K, 'Method', 'core', 'Window', 1);
    verifyGreaterThan(testCase, hypot(C1 - 1, C2 + 2), 2);
    verifyEqual(testCase, [K1 K2], [1 -2], 'AbsTol', 0.02);
end

function testCellAndEmptyStamp(testCase)
    [D1, D2] = imUtil.psf.stampCenter({gauss(41, 2, 1, 2), zeros(11), gauss(45, -1, 0, 2)});
    verifyEqual(testCase, [D1([1 3]) D2([1 3])], [2 1; -1 0], 'AbsTol', 1e-6);
    verifyTrue(testCase, isnan(D1(2)) && isnan(D2(2)));
end
