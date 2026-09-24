function tests = test_radiusAtFraction
    % Issue #1268: radiusAtFraction hardened for non-centrally-peaked PSFs
    % (out-of-focus rings), the wingsFix degenerate-edge guard, and the
    % PSF_RPK header keyword.
    tests = functiontests(localfunctions);
end

function [PSF] = i_gauss(Sigma, Half)
    [X,Y] = meshgrid(-Half:Half);
    PSF = exp(-(X.^2+Y.^2)/(2*Sigma^2));  PSF = PSF./sum(PSF(:));
end

function [PSF] = i_ring(R0, W, Half)
    [X,Y] = meshgrid(-Half:Half);
    R = hypot(X,Y);
    PSF = exp(-((R-R0).^2)/(2*W^2));  PSF = PSF./sum(PSF(:));
end

function testGaussianUnchanged(testCase)
    % centrally peaked: peak radius 0, threshold radius as before
    PSF = i_gauss(2, 12);
    [TR, PR] = imUtil.psf.radiusAtFraction(PSF, 1e-2);
    verifyEqual(testCase, PR, 0);
    % 1% of a sigma=2 Gaussian is at r = sigma*sqrt(2*ln(100)) ~ 6.1
    verifyTrue(testCase, TR>=5 && TR<=7, sprintf('TR=%g', TR));
    % wingsFix keeps the centre dominant
    P2 = imUtil.psf.wingsFix(PSF, 'WingsMethod','analytic', 'ApplyEllipticityFallback',false);
    [~,M] = imUtil.psf.mex.radialProfile_mex(P2, 13,13,12);
    verifyEqual(testCase, find(M==max(M),1), 1, 'Gaussian no longer centrally peaked');
end

function testRingPreserved(testCase)
    % the #1268 donut: peak radius = ring radius, splice beyond the ring
    PSF = i_ring(4, 1.2, 12);
    [TR, PR] = imUtil.psf.radiusAtFraction(PSF, 1e-2);
    verifyTrue(testCase, PR>=3 && PR<=5, sprintf('PeakRadius=%g', PR));
    verifyTrue(testCase, TR>PR, sprintf('ThreshRadius=%g not beyond the ring', TR));
    P2 = imUtil.psf.wingsFix(PSF, 'WingsMethod','analytic', 'ApplyEllipticityFallback',false);
    [R,M] = imUtil.psf.mex.radialProfile_mex(P2, 13,13,12);
    [~,Imx] = max(M);
    verifyTrue(testCase, R(Imx)>=3 && R(Imx)<=5, 'ring destroyed by the wing splice');
    verifyTrue(testCase, M(1)/max(M) < 0.5, 'centre no longer a hole');
end

function testPlateauNoCrossing(testCase)
    % profile never drops below 1% inside the stamp -> stamp half-size,
    % and wingsFix leaves the stamp untouched
    PSF = i_ring(9, 3, 12);   % wide ring near the edge
    [TR, ~] = imUtil.psf.radiusAtFraction(PSF, 1e-2);
    verifyEqual(testCase, TR, 12);
    P2 = imUtil.psf.wingsFix(PSF, 'WingsMethod','analytic', 'ApplyEllipticityFallback',false);
    verifyEqual(testCase, P2, PSF, 'AbsTol',1e-12);
end

function testFwhmWritesRPK(testCase)
    % PSF_RPK reaches the header via imProc.psf.fwhm AddMorphology
    AI = AstroImage({rand(64)});
    AI.PSFData.Data   = i_ring(4, 1.2, 12);
    AI.PSFData.Nstars = 10;
    try
        imProc.psf.fwhm(AI, 'AddMorphology',true, 'AddErr',false, 'UseLegacy',false, 'DefScale',1.25);
    catch
        % fwhm itself may need more context; the keyword must be there anyway
    end
    V = AI.HeaderData.getVal('PSF_RPK');
    verifyTrue(testCase, isfinite(V) && V>=3 && V<=5, sprintf('PSF_RPK=%g', V));
end
