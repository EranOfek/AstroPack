function tests = test_limmag
    % Unit tests for imProc.calib.limmag (LIMMAG + LIMMAGER, issue #1232).
    %
    % Uses synthetic catalogs with a known Mag = a*log10(SN) + b relation
    % and known Gaussian scatter, so both the limiting magnitude and its
    % uncertainty have ground-truth values. The key assertion is that
    % LIMMAGER estimates the SCATTER around the fitted line and is
    % therefore independent of the number of fitted sources (the raw
    % polyfit residual norm grows like sqrt(N) and must not be used).
    tests = functiontests(localfunctions);
end

%% Helpers

function AI = makeAI(N, Slope, LimMagTrue, LimSN, Sigma, Seed)
    % Synthetic AstroImage whose catalog follows
    %   MAG_PSF = Slope*log10(SN) + b + N(0, Sigma)
    % with b chosen so that the line passes through LimMagTrue at LimSN.
    rng(Seed);
    LogSN = log10(4.5) + (log10(45) - log10(4.5)) .* rand(N,1);
    SN    = 10.^LogSN;
    B     = LimMagTrue - Slope .* log10(LimSN);
    Mag   = Slope .* LogSN + B + Sigma .* randn(N,1);
    AI    = AstroImage;
    AI.CatData = AstroCatalog({[Mag, SN]}, 'ColNames', {'MAG_PSF','SN'});
end

function Tf = headerHasKey(AI, Key)
    Tf = any(strcmp(AI.HeaderData.Data(:,1), Key));
end

%% Tests

function testRecoverLineAndScatter(testCase)
    % LIMMAG recovers the true line value at LimSN; LIMMAGER recovers the
    % injected per-source scatter.
    Sigma = 0.15;
    AI = makeAI(2000, -2.5, 20.5, 5, Sigma, 1);
    [AI, Res] = imProc.calib.limmag(AI);
    verifyLessThan(testCase, abs(Res.LimMag - 20.5), 0.05, ...
        'LIMMAG does not recover the true line value at LimSN');
    verifyLessThan(testCase, abs(Res.LimMagErr - Sigma), 0.03, ...
        'LIMMAGER does not recover the injected scatter');
end

function testErrIndependentOfN(testCase)
    % Same injected scatter at N=200 and N=2000 must give the same
    % LIMMAGER (the raw residual norm would differ by sqrt(10)).
    Sigma = 0.15;
    [~, R1] = imProc.calib.limmag(makeAI( 200, -2.5, 20.5, 5, Sigma, 2));
    [~, R2] = imProc.calib.limmag(makeAI(2000, -2.5, 20.5, 5, Sigma, 3));
    Ratio = R1.LimMagErr ./ R2.LimMagErr;
    verifyGreaterThan(testCase, Ratio, 0.8, ...
        'LIMMAGER depends on the number of fitted sources');
    verifyLessThan(testCase, Ratio, 1.25, ...
        'LIMMAGER depends on the number of fitted sources');
end

function testHeaderKeywords(testCase)
    % Header carries LIMMAG + LIMMAGER matching the Result struct;
    % emptying either keyword suppresses only that keyword.
    AI = makeAI(500, -2.5, 20.5, 5, 0.1, 4);
    [AI, Res] = imProc.calib.limmag(AI);
    verifyEqual(testCase, AI.HeaderData.getVal('LIMMAG'),   Res.LimMag,    'AbsTol',1e-6);
    verifyEqual(testCase, AI.HeaderData.getVal('LIMMAGER'), Res.LimMagErr, 'AbsTol',1e-6);

    AI2 = makeAI(500, -2.5, 20.5, 5, 0.1, 4);
    AI2 = imProc.calib.limmag(AI2, 'KeyLimMagErr','');
    verifyTrue(testCase,  headerHasKey(AI2, 'LIMMAG'));
    verifyFalse(testCase, headerHasKey(AI2, 'LIMMAGER'), ...
        'empty KeyLimMagErr must suppress the LIMMAGER keyword');

    AI3 = makeAI(500, -2.5, 20.5, 5, 0.1, 4);
    AI3 = imProc.calib.limmag(AI3, 'KeyLimMag','');
    verifyFalse(testCase, headerHasKey(AI3, 'LIMMAG'));
    verifyTrue(testCase,  headerHasKey(AI3, 'LIMMAGER'), ...
        'LIMMAGER must be written independently of KeyLimMag');
end

function testTooFewSources(testCase)
    % Below MinNsrc both values are NaN, in the Result and in the header.
    AI = makeAI(5, -2.5, 20.5, 5, 0.1, 5);
    [AI, Res] = imProc.calib.limmag(AI);
    verifyTrue(testCase, isnan(Res.LimMag));
    verifyTrue(testCase, isnan(Res.LimMagErr));
    verifyTrue(testCase, isnan(AI.HeaderData.getVal('LIMMAG')));
    verifyTrue(testCase, isnan(AI.HeaderData.getVal('LIMMAGER')));
end

function testPollutionRejected(testCase)
    % NaN rows and sources outside the S/N window do not affect the fit.
    Sigma = 0.1;
    AI = makeAI(1000, -2.5, 20.5, 5, Sigma, 6);
    [~, ResClean] = imProc.calib.limmag(AI.copy);

    Extra = [NaN 10; 21 NaN; 15 500; 24 1];   % NaN mag; NaN SN; SN>MaxSN; SN<MinSN
    Dirty = AI.copy;
    Dirty.CatData.Catalog = [Dirty.CatData.Catalog; Extra];
    [~, ResDirty] = imProc.calib.limmag(Dirty);

    verifyEqual(testCase, ResDirty.Nsrc, ResClean.Nsrc, ...
        'polluted rows leaked into the fit');
    verifyEqual(testCase, ResDirty.LimMag,    ResClean.LimMag,    'AbsTol',1e-10);
    verifyEqual(testCase, ResDirty.LimMagErr, ResClean.LimMagErr, 'AbsTol',1e-10);
end
