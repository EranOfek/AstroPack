function Result = unitTest()
    % Unit tests for imUtil.background.modeVar_LogHist
    %   Emphasizes comparison of UseMex=true vs UseMex=false for various inputs.
    % Output : - True if all tests passed.
    % Author : Dina Kovaleva (Apr 2026)
    % Example: imUtil.background.unitTest

    Result = true;

    %% imUtil.background.modeVar_SampleHist + modeVar_SampleHist_mex

    Im=single(poissrnd(ones(256,256).*100));
    [m1,v1]=imUtil.background.modeVar_LogHist(Im);
    [m2,v2]=imUtil.background.modeVar_SampleHist(Im);
    [m3,v3]=imUtil.background.modeVar_SampleHist(Im, 'UseMex',false);
    
    if abs(m1-m2)>1
        m1-m2
        error('Problem with: imUtil.background.modeVar_SampleHist');
    end
    if abs(m2-m3)>1e-5 || abs(v2-v3)>1e-5
        m2-m3
        v2-v3
        error('Problem with: imUtil.background.modeVar_SampleHist');
    end

    % random test:

    Nsim = 100;
    m1   = zeros(Nsim,1);
    v1   = zeros(Nsim,1);
    m2   = zeros(Nsim,1);
    v2   = zeros(Nsim,1);
    for i=1:Nsim
        Im = single(poissrnd(ones(256,256).*100));
        [m1(i),v1(i)]=imUtil.background.modeVar_LogHist(Im);
        [m2(i),v2(i)]=imUtil.background.modeVar_SampleHist(Im, 'UseMex',false);
    end

    mean(m1-m2)
    mean(v1-v2)
    std(v1-v2)
    [mean(m1), mean(v1), std(m1), std(v1)]
    [mean(m2), mean(v2), std(m2), std(v2)]
    if std(m1-m2)>1
        error('Problem with imUtil.background.modeVar_SampleHist (mode)');
    end

    if abs(mean(v1-v2))>3
        mean(v1-v2)
        error('Problem with imUtil.background.modeVar_SampleHist (var)');
    end

    if abs(mean(m1)-100)>1
        error('Problem with imUtil.background.modeVar_SampleHist (mode)');
    end
    if abs(mean(v1)-100)>2
        error('Problem with imUtil.background.modeVar_SampleHist (var)');
    end

    %%
    
    Tol    = 1e-3;   % relative tolerance for Mode and Var between MEX / non-MEX

    fprintf('=== imUtil.background.modeVar_LogHist unit tests ===\n');

    % ------------------------------------------------------------------
    % Helper: check relative agreement between MEX and non-MEX outputs
    % ------------------------------------------------------------------
    function checkPair(ModeMex, VarMex, ModeNoMex, VarNoMex, Label)
        RelMode = abs(ModeMex - ModeNoMex) ./ abs(ModeNoMex);
        RelVar  = abs(VarMex  - VarNoMex)  ./ abs(VarNoMex);
        if RelMode > Tol || RelVar > Tol
            error('FAIL [%s]: Mode %.4g vs %.4g (rel %.3g), Var %.4g vs %.4g (rel %.3g)', ...
                  Label, ModeMex, ModeNoMex, RelMode, VarMex, VarNoMex, RelVar);
        end
        fprintf('  PASS [%s]: Mode=%.4g  Var=%.4g\n', Label, ModeMex, VarMex);
    end

    % ------------------------------------------------------------------
    % 1. Small 1-D array, low background (~50 e-)
    % ------------------------------------------------------------------
    rng(1);
    Back1  = 50;
    Array1 = double(poissrnd(Back1, [1, 5000])) + 0.1;   % ensure positive
    [M1,  V1]  = imUtil.background.modeVar_LogHist(Array1, 'UseMex', true);
    [M1n, V1n] = imUtil.background.modeVar_LogHist(Array1, 'UseMex', false);
    checkPair(M1, V1, M1n, V1n, '1D low-back (50 e-)');

    % sanity: mode should be near the true background
    if abs(M1 - Back1) / Back1 > 0.15
        error('FAIL: mode %.2f far from true background %d', M1, Back1);
    end

    % ------------------------------------------------------------------
    % 2. Small 1-D array, medium background (~300 e-)
    % ------------------------------------------------------------------
    Back2  = 300;
    Array2 = double(poissrnd(Back2, [1, 5000])) + 0.1;
    [M2,  V2]  = imUtil.background.modeVar_LogHist(Array2, 'UseMex', true);
    [M2n, V2n] = imUtil.background.modeVar_LogHist(Array2, 'UseMex', false);
    checkPair(M2, V2, M2n, V2n, '1D medium-back (300 e-)');

    % ------------------------------------------------------------------
    % 3. 2-D image patch, high background (~2000 e-)
    % ------------------------------------------------------------------
    Back3  = 2000;
    Array3 = double(poissrnd(Back3, [512, 512])) + 0.1;
    [M3,  V3]  = imUtil.background.modeVar_LogHist(Array3, 'UseMex', true);
    [M3n, V3n] = imUtil.background.modeVar_LogHist(Array3, 'UseMex', false);
    checkPair(M3, V3, M3n, V3n, '2D high-back 512x512 (2000 e-)');

    % ------------------------------------------------------------------
    % 4. Large 2-D image, background ~500 e-
    % ------------------------------------------------------------------
    Back4  = 500;
    Array4 = double(poissrnd(Back4, [2048, 2048])) + 0.1;
    [M4,  V4]  = imUtil.background.modeVar_LogHist(Array4, 'UseMex', true);
    [M4n, V4n] = imUtil.background.modeVar_LogHist(Array4, 'UseMex', false);
    checkPair(M4, V4, M4n, V4n, '2D large 2048x2048 (500 e-)');

    % ------------------------------------------------------------------
    % 5. With DiluteFactor > 1
    % ------------------------------------------------------------------
    [M5,  V5]  = imUtil.background.modeVar_LogHist(Array4, 'UseMex', true,  'DiluteFactor', 4);
    [M5n, V5n] = imUtil.background.modeVar_LogHist(Array4, 'UseMex', false, 'DiluteFactor', 4);
    checkPair(M5, V5, M5n, V5n, '2D large with DiluteFactor=4');

    % ------------------------------------------------------------------
    % 6. With MinVal / MaxVal clipping
    % ------------------------------------------------------------------
    [M6,  V6]  = imUtil.background.modeVar_LogHist(Array3, 'UseMex', true,  'MinVal', Back3*0.5, 'MaxVal', Back3*2);
    [M6n, V6n] = imUtil.background.modeVar_LogHist(Array3, 'UseMex', false, 'MinVal', Back3*0.5, 'MaxVal', Back3*2);
    checkPair(M6, V6, M6n, V6n, '2D with MinVal/MaxVal');

    % ------------------------------------------------------------------
    % 7. With quantile removal
    % ------------------------------------------------------------------
    [M7,  V7]  = imUtil.background.modeVar_LogHist(Array3, 'UseMex', true,  'RemoveLowerQuantile', 0.01, 'RemoveUpperQuantile', 0.05);
    [M7n, V7n] = imUtil.background.modeVar_LogHist(Array3, 'UseMex', false, 'RemoveLowerQuantile', 0.01, 'RemoveUpperQuantile', 0.05);
    checkPair(M7, V7, M7n, V7n, '2D with quantile removal');

    % ------------------------------------------------------------------
    % 8. Convert2single
    % ------------------------------------------------------------------
    [M8,  V8]  = imUtil.background.modeVar_LogHist(Array2, 'UseMex', true,  'Convert2single', true);
    [M8n, V8n] = imUtil.background.modeVar_LogHist(Array2, 'UseMex', false, 'Convert2single', true);
    checkPair(M8, V8, M8n, V8n, '1D Convert2single');

    % ------------------------------------------------------------------
    % 9. CalcPoissVar flag (forces Poisson variance estimate)
    % ------------------------------------------------------------------
    [M9,  V9]  = imUtil.background.modeVar_LogHist(Array2, 'UseMex', true,  'CalcPoissVar', true);
    [M9n, V9n] = imUtil.background.modeVar_LogHist(Array2, 'UseMex', false, 'CalcPoissVar', true);
    checkPair(M9, V9, M9n, V9n, '1D CalcPoissVar=true');

    % ------------------------------------------------------------------
    % 10. UseSlash=false (polyfit path) vs UseSlash=true, both non-MEX
    % ------------------------------------------------------------------
    [M10a, V10a] = imUtil.background.modeVar_LogHist(Array2, 'UseMex', false, 'UseSlash', true);
    [M10b, V10b] = imUtil.background.modeVar_LogHist(Array2, 'UseMex', false, 'UseSlash', false);
    RelMode10 = abs(M10a - M10b) / abs(M10b);
    RelVar10  = abs(V10a - V10b) / abs(V10b);
    if RelMode10 > Tol || RelVar10 > Tol
        error('FAIL [UseSlash comparison]: Mode %.4g vs %.4g, Var %.4g vs %.4g', M10a, M10b, V10a, V10b);
    end
    fprintf('  PASS [UseSlash true vs false (non-MEX)]: Mode=%.4g  Var=%.4g\n', M10a, V10a);

    % ------------------------------------------------------------------
    % 11. Output must be finite scalars for all test arrays
    % ------------------------------------------------------------------
    TestArrays = {Array1, Array2, Array3};
    for Ii = 1:numel(TestArrays)
        [Mc, Vc] = imUtil.background.modeVar_LogHist(TestArrays{Ii}, 'UseMex', true);
        if ~isscalar(Mc) || ~isfinite(Mc) || ~isscalar(Vc) || ~isfinite(Vc)
            error('FAIL: non-finite or non-scalar output for test array %d', Ii);
        end
    end
    fprintf('  PASS [output is finite scalar for all inputs]\n');

    % ------------------------------------------------------------------
    % Helper: build a 2000x2000 Poisson background image with 100 stars.
    %   Each star is a cluster of ClustSz x ClustSz pixels (ClustSz in 3..4)
    %   with flux = Background * StarFactor (StarFactor drawn from [10,1000]).
    % ------------------------------------------------------------------
    function Img = makeStarField(Background, Seed)
        rng(Seed);
        Img      = double(poissrnd(Background, [2000, 2000])) + 0.1;
        Nstars   = 100;
        Margin   = 10;   % keep stars away from edges
        for Ks = 1:Nstars
            StarFactor = 10.^(1 + 2.*rand());          % uniform in log: 10..1000 x back
            ClustSz    = randi([3, 4]);                 % 3x3 or 4x4 pixels
            Row = randi([Margin, 2000-Margin-ClustSz]);
            Col = randi([Margin, 2000-Margin-ClustSz]);
            RowIdx = Row : Row+ClustSz-1;
            ColIdx = Col : Col+ClustSz-1;
            Img(RowIdx, ColIdx) = Img(RowIdx, ColIdx) + StarFactor .* Background;
        end
    end

    % ------------------------------------------------------------------
    % 12. 2000x2000, background 500 e-, 100 stars (default args)
    % ------------------------------------------------------------------
    fprintf('\n--- Star-field tests (2000x2000, 100 stars) ---\n');
    Back12  = 500;
    Img12   = makeStarField(Back12, 42);
    [M12,  V12]  = imUtil.background.modeVar_LogHist(Img12, 'UseMex', true);
    [M12n, V12n] = imUtil.background.modeVar_LogHist(Img12, 'UseMex', false);
    checkPair(M12, V12, M12n, V12n, 'star-field back=500, default args');
    if abs(M12 - Back12) / Back12 > 0.05
        error('FAIL: mode %.2f far from background %d (stars biased result)', M12, Back12);
    end

    % ------------------------------------------------------------------
    % 13. 2000x2000, background 200 e-, 100 stars, with DiluteFactor=2
    % ------------------------------------------------------------------
    Back13 = 200;
    Img13  = makeStarField(Back13, 7);
    [M13,  V13]  = imUtil.background.modeVar_LogHist(Img13, 'UseMex', true,  'DiluteFactor', 2);
    [M13n, V13n] = imUtil.background.modeVar_LogHist(Img13, 'UseMex', false, 'DiluteFactor', 2);
    checkPair(M13, V13, M13n, V13n, 'star-field back=200, DiluteFactor=2');
    if abs(M13 - Back13) / Back13 > 0.05
        error('FAIL: mode %.2f far from background %d', M13, Back13);
    end

    % ------------------------------------------------------------------
    % 14. 2000x2000, background 1000 e-, 100 stars, upper quantile removal
    %     (stars raise the upper tail; quantile clipping should still leave
    %      mode accurate)
    % ------------------------------------------------------------------
    Back14 = 1000;
    Img14  = makeStarField(Back14, 13);
    [M14,  V14]  = imUtil.background.modeVar_LogHist(Img14, 'UseMex', true,  'RemoveUpperQuantile', 0.01);
    [M14n, V14n] = imUtil.background.modeVar_LogHist(Img14, 'UseMex', false, 'RemoveUpperQuantile', 0.01);
    checkPair(M14, V14, M14n, V14n, 'star-field back=1000, RemoveUpperQuantile=0.01');
    if abs(M14 - Back14) / Back14 > 0.05
        error('FAIL: mode %.2f far from background %d', M14, Back14);
    end

    % ------------------------------------------------------------------
    % 15. 2000x2000, background 300 e-, 100 stars, MinVal+MaxVal clipping
    %     (MaxVal alone triggers a dimension error in the function because
    %      Array>[] fails; must supply both MinVal and MaxVal together)
    % ------------------------------------------------------------------
    Back15 = 300;
    Img15  = makeStarField(Back15, 99);
    [M15,  V15]  = imUtil.background.modeVar_LogHist(Img15, 'UseMex', true,  'MinVal', 1, 'MaxVal', Back15*5);
    [M15n, V15n] = imUtil.background.modeVar_LogHist(Img15, 'UseMex', false, 'MinVal', 1, 'MaxVal', Back15*5);
    checkPair(M15, V15, M15n, V15n, 'star-field back=300, MinVal=1 MaxVal=5*back');
    if abs(M15 - Back15) / Back15 > 0.05
        error('FAIL: mode %.2f far from background %d', M15, Back15);
    end

    % ------------------------------------------------------------------
    % 16. 2000x2000, background 800 e-, 100 stars, Convert2single
    % ------------------------------------------------------------------
    Back16 = 800;
    Img16  = makeStarField(Back16, 55);
    [M16,  V16]  = imUtil.background.modeVar_LogHist(Img16, 'UseMex', true,  'Convert2single', true);
    [M16n, V16n] = imUtil.background.modeVar_LogHist(Img16, 'UseMex', false, 'Convert2single', true);
    checkPair(M16, V16, M16n, V16n, 'star-field back=800, Convert2single');
    if abs(M16 - Back16) / Back16 > 0.05
        error('FAIL: mode %.2f far from background %d', M16, Back16);
    end

    % ------------------------------------------------------------------
    % 17. 2000x2000, background 150 e-, 100 stars, DiluteFactor=4 + upper quantile
    % ------------------------------------------------------------------
    Back17 = 150;
    Img17  = makeStarField(Back17, 3);
    [M17,  V17]  = imUtil.background.modeVar_LogHist(Img17, 'UseMex', true,  'DiluteFactor', 4, 'RemoveUpperQuantile', 0.02);
    [M17n, V17n] = imUtil.background.modeVar_LogHist(Img17, 'UseMex', false, 'DiluteFactor', 4, 'RemoveUpperQuantile', 0.02);
    checkPair(M17, V17, M17n, V17n, 'star-field back=150, DiluteFactor=4 + upper-quantile');
    if abs(M17 - Back17) / Back17 > 0.05
        error('FAIL: mode %.2f far from background %d', M17, Back17);
    end

    fprintf('=== All tests PASSED ===\n');

end
