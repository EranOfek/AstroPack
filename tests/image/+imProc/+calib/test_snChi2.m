function tests = test_snChi2
    % Unit tests for imProc.calib.snChi2 (issue #1271)
    tests = functiontests(localfunctions);
end

function testRecoverKnownFloor(testCase)
    % Synthetic catalog with a known systematic floor K:
    % chi2dof = chi2rnd(dof)/dof + (K/MAGERR)^2
    rng(1);
    N      = 4000;
    Dof    = 25;
    Ktrue  = 0.012;
    MagErr = 10.^(-3 + 2.5.*rand(N,1));
    Chi2   = chi2rnd(Dof, N, 1)./Dof + (Ktrue./MagErr).^2 .* (1 + 0.1.*randn(N,1));

    AI = AstroImage;
    AI.CatData = AstroCatalog({[Chi2, MagErr]}, 'ColNames',{'PSF_CHI2DOF','MAGERR_PSF'});

    [AI, Res] = imProc.calib.snChi2(AI);

    verifyEqual(testCase, Res.SNCHI2, Ktrue, 'RelTol',0.1);
    verifyGreaterThan(testCase, Res.Nsrc, 100);
    verifyEqual(testCase, AI.HeaderData.getVal('SNCHI2'), Res.SNCHI2, 'AbsTol',1e-10);
end

function testFluxErrFallback(testCase)
    % Without MAGERR_PSF the estimate must come from 1.086*FLUXERR_PSF
    rng(2);
    N      = 3000;
    Dof    = 25;
    Ktrue  = 0.02;
    MagErr = 10.^(-3 + 2.*rand(N,1));
    Chi2   = chi2rnd(Dof, N, 1)./Dof + (Ktrue./MagErr).^2;

    AI = AstroImage;
    AI.CatData = AstroCatalog({[Chi2, MagErr./1.086]}, 'ColNames',{'PSF_CHI2DOF','FLUXERR_PSF'});

    [~, Res] = imProc.calib.snChi2(AI);
    verifyEqual(testCase, Res.SNCHI2, Ktrue, 'RelTol',0.1);
end

function testDegenerateInputs(testCase)
    % Empty catalog and missing columns give NaN without error
    AI1 = AstroImage;
    [AI1, Res1] = imProc.calib.snChi2(AI1);
    verifyTrue(testCase, isnan(Res1.SNCHI2));
    verifyEqual(testCase, Res1.Nsrc, 0);

    AI2 = AstroImage;
    AI2.CatData = AstroCatalog({rand(50,2)}, 'ColNames',{'X','Y'});
    [AI2, Res2] = imProc.calib.snChi2(AI2);
    verifyTrue(testCase, isnan(Res2.SNCHI2));
    verifyTrue(testCase, isnan(AI2.HeaderData.getVal('SNCHI2')));
end

function testTooFewSources(testCase)
    % Below MinNsrc the estimate must be NaN
    Chi2   = 5.*ones(5,1);
    MagErr = 0.01.*ones(5,1);
    AI = AstroImage;
    AI.CatData = AstroCatalog({[Chi2, MagErr]}, 'ColNames',{'PSF_CHI2DOF','MAGERR_PSF'});
    [~, Res] = imProc.calib.snChi2(AI, 'MinNsrc',10);
    verifyTrue(testCase, isnan(Res.SNCHI2));
    verifyEqual(testCase, Res.Nsrc, 5);
end

function testNoHeaderUpdate(testCase)
    % UpdateHeader=false must leave the header untouched
    Chi2   = 5.*ones(50,1);
    MagErr = 0.01.*ones(50,1);
    AI = AstroImage;
    AI.CatData = AstroCatalog({[Chi2, MagErr]}, 'ColNames',{'PSF_CHI2DOF','MAGERR_PSF'});
    [AI, Res] = imProc.calib.snChi2(AI, 'UpdateHeader',false);
    verifyEqual(testCase, Res.SNCHI2, sqrt(4)*0.01, 'AbsTol',1e-12);
    verifyTrue(testCase, isnan(AI.HeaderData.getVal('SNCHI2')));
end
