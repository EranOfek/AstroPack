function tests = test_starsSpatialDensity
    % Unit tests for imProc.cat.starsSpatialDensity (issue #1274)
    tests = functiontests(localfunctions);
end

function AC = localUniformCat(N, Sz, Seed)
    rng(Seed);
    AC = AstroCatalog({[Sz.*rand(N,1), Sz.*rand(N,1), 100.*rand(N,1)]}, ...
                      'ColNames',{'X','Y','SN'});
end

function testUniformFieldQuantilesClose(testCase)
    % A uniform field: all quantiles near N/Nbin, upper ones only mildly above
    AC = localUniformCat(14400, 1716, 1);
    Res = imProc.cat.starsSpatialDensity(AC);
    Expected = 14400/144;
    verifyEqual(testCase, Res.Value(1), Expected, 'RelTol',0.15);
    verifyTrue(testCase, Res.Value(4) > Res.Value(1));          % Q95 > Q50
    verifyTrue(testCase, Res.Value(4) < 1.5*Res.Value(1));      % but not by much
    verifyEqual(testCase, Res.Nsrc, 14400);
    verifyEqual(testCase, Res.Nbin, [12 12]);
    verifyEqual(testCase, Res.KeyNames, {'SPATQ50','SPATQ75','SPATQ90','SPATQ95'});
end

function testClumpRaisesUpperQuantiles(testCase)
    % Adding an extended clump must raise Q95 while leaving Q50 alone.
    % The clump is given sigma ~150 pix (~1 grid cell), so that it spans
    % roughly a dozen cells as the real ghost-halo floods of issue #1274
    % do; a much more compact clump occupies only ~4 of the 144 cells and
    % is invisible to Q95, which is the 137th ranked cell.
    rng(2);
    N = 7200; Sz = 1716;
    Base = [Sz.*rand(N,1), Sz.*rand(N,1)];
    Clump = [800 + 150.*randn(1800,1), 800 + 150.*randn(1800,1)];
    AC0 = AstroCatalog({Base}, 'ColNames',{'X','Y'});
    AC1 = AstroCatalog({[Base; Clump]}, 'ColNames',{'X','Y'});
    R0 = imProc.cat.starsSpatialDensity(AC0);
    R1 = imProc.cat.starsSpatialDensity(AC1);
    verifyEqual(testCase, R1.Value(1), R0.Value(1), 'RelTol',0.2);   % Q50 ~ unchanged
    verifyGreaterThan(testCase, R1.Value(4), 1.5*R0.Value(4));       % Q95 up
end

function testHeaderKeywords(testCase)
    AI = AstroImage({zeros(1716,1716)});
    AI.CatData = localUniformCat(3600, 1716, 3);
    [Res, AI] = imProc.cat.starsSpatialDensity(AI);
    for I = 1:numel(Res.KeyNames)
        verifyEqual(testCase, AI.HeaderData.getVal(Res.KeyNames{I}), Res.Value(I), 'AbsTol',1e-10);
    end
    verifyEqual(testCase, AI.HeaderData.getVal('SPATNBIN'), 144);
    % the source count is not written: N_STARS already carries it
    verifyEqual(testCase, Res.Nsrc, 3600);
end

function testMinSNFilter(testCase)
    % With SN uniform in [0,100], MinSN=50 must halve the counts
    AC = localUniformCat(14400, 1716, 4);
    RAll = imProc.cat.starsSpatialDensity(AC);
    RCut = imProc.cat.starsSpatialDensity(AC, 'MinSN',50);
    verifyEqual(testCase, RCut.Nsrc, RAll.Nsrc/2, 'RelTol',0.05);
    verifyLessThan(testCase, RCut.Value(1), RAll.Value(1));
end

function testCustomGridAndQuantiles(testCase)
    AC = localUniformCat(1600, 1716, 5);
    Res = imProc.cat.starsSpatialDensity(AC, 'Nbin',[8 4], 'Quantiles',[0.25 0.50]);
    verifyEqual(testCase, Res.Nbin, [8 4]);
    verifyEqual(testCase, Res.KeyNames, {'SPATQ25','SPATQ50'});
    verifyEqual(testCase, Res.Value(2), 1600/32, 'RelTol',0.25);
end

function testEmptyAndMissingColumns(testCase)
    Res1 = imProc.cat.starsSpatialDensity(AstroCatalog);
    verifyTrue(testCase, all(isnan(Res1.Value)));
    verifyEqual(testCase, Res1.Nsrc, 0);

    AC = AstroCatalog({rand(10,2)}, 'ColNames',{'RA','Dec'});
    Res2 = imProc.cat.starsSpatialDensity(AC);
    verifyTrue(testCase, all(isnan(Res2.Value)));
end

function testArrayInput(testCase)
    AI = AstroImage({zeros(1716,1716), zeros(1716,1716)});
    AI(1).CatData = localUniformCat(3600, 1716, 6);
    AI(2).CatData = localUniformCat(7200, 1716, 7);
    Res = imProc.cat.starsSpatialDensity(AI);
    verifyEqual(testCase, numel(Res), 2);
    verifyGreaterThan(testCase, Res(2).Value(1), Res(1).Value(1));
end
