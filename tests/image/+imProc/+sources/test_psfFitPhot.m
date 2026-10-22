function tests = test_psfFitPhot
    % Unit tests for imProc.sources.psfFitPhot.
    %
    % The 'BackFromAnnulus' option (issue #1442): the PSF fit takes the
    % background of each source from the median of its 'BackAnnulus' ring and
    % keeps the noise from the variance image.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testOffIsUnchanged(testCase)
    % BackFromAnnulus=false gives exactly the default result.

    % Arrange
    [AI, XY, PSF] = makeImage(0.5);

    % Act
    [~, R0] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false);
    [~, R1] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false, 'BackFromAnnulus',false);

    % Assert
    testCase.verifyTrue(isequaln(R0.Flux, R1.Flux) && isequaln(R0.MagErr, R1.MagErr), 'BackFromAnnulus=false changed the PSF fit.');
end

function testRecoversFluxWithBiasedBackground(testCase)
    % A background image 2 above the sky biases the flux low; the ring background removes the bias.

    % Arrange
    [AI, XY, PSF, Flux] = makeImage(2);

    % Act
    [~, R0] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false);
    [~, R1] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false, 'BackFromAnnulus',true, 'BackAnnulus',[10 12]);

    % Assert
    testCase.verifyLessThan(max(R0.Flux(:)./Flux), 0.99, 'The biased background image should lower the flux by more than 1%.');
    testCase.verifyEqual(R1.Flux(:), Flux, 'RelTol',1e-3, 'The ring background should recover the true flux.');
end

function testNoiseFromVarianceImage(testCase)
    % The error comes from the variance image, as in the default fit, not from the ring scatter.

    % Arrange: unbiased background image, so the default and the ring fluxes agree
    [AI, XY, PSF] = makeImage(0);

    % Act
    [~, R0] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false);
    [~, R1] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false, 'BackFromAnnulus',true, 'BackAnnulus',[10 12]);
    [~, R2] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false, 'BackStdFromAnnulus',true, 'BackAnnulus',[10 12]);

    % Assert
    testCase.verifyEqual(R1.MagErr, R0.MagErr, 'RelTol',1e-6, 'BackFromAnnulus should keep the error of the variance image.');
    testCase.verifyGreaterThan(max(abs(R2.MagErr(:)./R0.MagErr(:)-1)), 0.01, 'BackStdFromAnnulus should take its noise from the ring instead.');
end

function testFallbackWithoutValidRing(testCase)
    % A source whose ring has no valid pixels keeps the background image.

    % Arrange
    [AI, XY, PSF] = makeImage(0.5);
    [Xg, Yg] = meshgrid(1:size(AI.Image,2), 1:size(AI.Image,1));
    R = sqrt((Xg-XY(2,1)).^2 + (Yg-XY(2,2)).^2);
    Im = AI.Image;
    Im(R>=9.5 & R<=12.5) = NaN;
    AI.Image = Im;

    % Act
    [~, R0] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false);
    [~, R1] = imProc.sources.psfFitPhot(AI.copy, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false, 'BackFromAnnulus',true, 'BackAnnulus',[10 12]);

    % Assert
    testCase.verifyEqual(R1.Flux(2), R0.Flux(2), 'RelTol',1e-10, 'The source with no valid ring should keep the background image.');
    testCase.verifyGreaterThan(R1.Flux(1), R0.Flux(1), 'The other source should still use its ring.');
end

function testRingLargerThanStampErrors(testCase)
    % A ring that does not fit in the stamp raises an error.

    % Arrange
    [AI, XY] = makeImage(0.5);
    PSF = imUtil.kernel2.gauss(1.5, [17 17]);

    % Act
    Msg = '';
    try
        imProc.sources.psfFitPhot(AI, 'XY',XY, 'PSF',PSF, 'ColSN',[], 'UpdateCat',false, 'BackFromAnnulus',true, 'BackAnnulus',[10 12]);
    catch ME
        Msg = ME.message;
    end

    % Assert
    testCase.verifySubstring(Msg, 'exceeds the stamp half size', 'A ring larger than the stamp half size should raise its own error.');
end

%% Helpers

function [AI, XY, PSF, Flux] = makeImage(BackBias, Var)
    % Noise-free image: sky 100 plus two Gaussian stars; background image = sky + BackBias.
    if nargin<2
        Var = 100;
    end
    Sky   = 100;
    Sigma = 1.5;
    XY    = [60.3 70.6; 140.7 120.2];
    Flux  = [5000; 2000];
    [Xg, Yg] = meshgrid(1:200, 1:200);
    Im = Sky + zeros(200);
    for I = 1:size(XY,1)
        Im = Im + Flux(I)./(2*pi*Sigma^2).*exp(-((Xg-XY(I,1)).^2 + (Yg-XY(I,2)).^2)./(2*Sigma^2));
    end
    AI = AstroImage({Im});
    AI.Back = (Sky + BackBias) + zeros(200);
    AI.Var  = Var + zeros(200);
    PSF = imUtil.kernel2.gauss(Sigma, [25 25]);
    PSF = PSF./sum(PSF(:));
end
