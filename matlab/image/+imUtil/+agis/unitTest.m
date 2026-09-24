function Result = unitTest()
    % Unit test for the imUtil.agis fitting engine, using only the
    % ProperMotion + Affine terms.
    %
    % Simulates: Nsrc=500 sources, Nepoch=1e4 epochs, magnitudes uniform in
    % [14,19], mag-dependent per-epoch astrometric noise (~10 mas at the
    % bright end, growing as 10*10^(0.2*(mag-14))), over a ~5-year baseline
    % with small random per-epoch affine distortions. Runs the fit in two
    % phases: (1) unweighted, (2) mag-dependent weights, warm-started from
    % phase 1. Checks recovered source parameters (x0,y0,mux,muy) against
    % the known truth, and sanity-checks the residual RMS and the gauge-fix.
    %
    % Output : - Result : true if all checks pass, false otherwise.
    % Author : N. Segev / imUtil.agis rewrite
    % Example: Result = imUtil.agis.unitTest();

    Result = true;

    try
        rng(1);

        %% ----- simulation setup -----
        Nsrc     = 500;
        Nepoch   = 1e4;
        PixScale = 400;      % mas / pixel (KMTNet-like)
        FovPix   = 300;      % field size, pixels

        MagMin = 14; MagMax = 19;
        MagPerSrc = MagMin + (MagMax-MagMin) .* rand(1, Nsrc);
        Mag = repmat(MagPerSrc, Nepoch, 1);               % [Nepoch x Nsrc]

        % mag-dependent per-epoch astrometric noise: ~10 mas at mag=14
        SigmaMasPerSrc = 10 .* 10.^(0.2*(MagPerSrc - MagMin));
        SigmaPixPerSrc = SigmaMasPerSrc ./ PixScale;
        SigmaPix = repmat(SigmaPixPerSrc, Nepoch, 1);

        T0 = 2457000;
        BaselineDays = 5*365.25;
        JD = sort(T0 + BaselineDays .* rand(Nepoch,1));
        RefEpoch = median(JD);
        T = JD - RefEpoch;
        [~, FixIdx] = min(abs(T));                        % reference (pinned) epoch

        %% ----- truth: source parameters -----
        X0True = FovPix .* rand(1, Nsrc);
        Y0True = FovPix .* rand(1, Nsrc);
        MuMasYr = 2 .* randn(1, Nsrc);                     % sigma = 2 mas/yr
        MuPixDay = (MuMasYr ./ PixScale) ./ 365.25;
        MuXTrue = MuPixDay .* (1 + 0.3.*randn(1, Nsrc));
        MuYTrue = MuPixDay .* (1 + 0.3.*randn(1, Nsrc));

        %% ----- truth: per-epoch affine parameters (small, additive; zero at FixIdx) -----
        A1 = 0.001*randn(Nepoch,1); A2 = 0.001*randn(Nepoch,1); A3 = 0.05*randn(Nepoch,1);
        A4 = 0.001*randn(Nepoch,1); A5 = 0.001*randn(Nepoch,1); A6 = 0.05*randn(Nepoch,1);
        A1(FixIdx)=0; A2(FixIdx)=0; A3(FixIdx)=0; A4(FixIdx)=0; A5(FixIdx)=0; A6(FixIdx)=0;

        %% ----- build noiseless model, then add noise -----
        Ttile   = repmat(T, 1, Nsrc);
        X0tile  = repmat(X0True, Nepoch, 1);
        Y0tile  = repmat(Y0True, Nepoch, 1);
        MuXtile = repmat(MuXTrue, Nepoch, 1);
        MuYtile = repmat(MuYTrue, Nepoch, 1);

        XModel = X0tile + MuXtile.*Ttile + A1.*X0tile + A2.*Y0tile + A3;
        YModel = Y0tile + MuYtile.*Ttile + A4.*X0tile + A5.*Y0tile + A6;

        X = XModel + SigmaPix .* randn(Nepoch, Nsrc);
        Y = YModel + SigmaPix .* randn(Nepoch, Nsrc);

        FieldStruct.X   = X;
        FieldStruct.Y   = Y;
        FieldStruct.Mag = Mag;

        %% ----- build Data + Terms -----
        Data = imUtil.agis.buildFitData(FieldStruct, JD, 'RefEpoch', RefEpoch);

        Terms = [imUtil.agis.properMotion(Data), ...
                 imUtil.agis.affine(Data, 'FixEpoch', FixIdx)];

        %% ----- phase 1: unweighted -----
        tic;
        [State1, ~] = imUtil.agis.runFit(Data, Terms, 'NIter', 6, 'UseWeights', false);
        Time1 = toc;

        [Rx1, Ry1] = imUtil.agis.computeResiduals(Data, Terms, State1);
        Rms2D_1 = sqrt(mean(Rx1(:).^2 + Ry1(:).^2, 'omitnan'));

        %% ----- phase 2: mag-dependent weights, warm-started from phase 1 -----
        tic;
        [State2, ~] = imUtil.agis.runFit(Data, Terms, 'NIter', 6, 'UseWeights', true, ...
            'InitialState', State1);
        Time2 = toc;

        [Rx2, Ry2] = imUtil.agis.computeResiduals(Data, Terms, State2);
        Rms2D_2 = sqrt(mean(Rx2(:).^2 + Ry2(:).^2, 'omitnan'));

        fprintf('Phase 1 (unweighted) : %.1f s, 2D RMS residual = %.4f pix\n', Time1, Rms2D_1);
        fprintf('Phase 2 (weighted)   : %.1f s, 2D RMS residual = %.4f pix\n', Time2, Rms2D_2);

        %% ----- extract recovered parameters -----
        PmFit  = State2.Params.ProperMotion;   % [4 x Nsrc]
        X0Fit  = PmFit(1,:); Y0Fit  = PmFit(2,:);
        MuXFit = PmFit(3,:); MuYFit = PmFit(4,:);

        RmsX0  = sqrt(mean((X0Fit  - X0True ).^2));
        RmsY0  = sqrt(mean((Y0Fit  - Y0True ).^2));
        RmsMuX = sqrt(mean((MuXFit - MuXTrue).^2));
        RmsMuY = sqrt(mean((MuYFit - MuYTrue).^2));

        % population-averaged theoretical proper-motion precision (order-of-magnitude)
        TStd = std(T);
        ExpectedSigmaMu = sqrt(mean(SigmaPixPerSrc.^2)) ./ (TStd .* sqrt(Nepoch));

        fprintf('RMS X0  error : %.4g pix\n', RmsX0);
        fprintf('RMS Y0  error : %.4g pix\n', RmsY0);
        fprintf('RMS MuX error : %.4g pix/day (expected order ~%.4g)\n', RmsMuX, ExpectedSigmaMu);
        fprintf('RMS MuY error : %.4g pix/day (expected order ~%.4g)\n', RmsMuY, ExpectedSigmaMu);

        %% ----- checks -----
        TolMu   = 8;      % generous factor around the order-of-magnitude estimate
        TolPos  = 0.05;   % pixels, generous given typical recovery ~0.005 pix

        Check.MuX  = RmsMuX < TolMu * ExpectedSigmaMu;
        Check.MuY  = RmsMuY < TolMu * ExpectedSigmaMu;
        Check.X0   = RmsX0  < TolPos;
        Check.Y0   = RmsY0  < TolPos;
        Check.ResidNotWorse = Rms2D_2 <= 1.5 * Rms2D_1;
        Check.GaugePinHeld  = all(State2.Params.Affine(:, FixIdx) == 0);
        Check.NoNaN = ~any(isnan(PmFit(:)));

        FN = fieldnames(Check);
        for Ik = 1:numel(FN)
            fprintf('  Check %-16s : %s\n', FN{Ik}, string(Check.(FN{Ik})));
            Result = Result && Check.(FN{Ik});
        end

        if Result
            fprintf('imUtil.agis.unitTest: PASSED\n');
        else
            fprintf('imUtil.agis.unitTest: FAILED\n');
        end

    catch ME
        warning('imUtil:agis:unitTest:error', 'Unit test threw an error: %s', ME.message);
        Result = false;
    end
end
