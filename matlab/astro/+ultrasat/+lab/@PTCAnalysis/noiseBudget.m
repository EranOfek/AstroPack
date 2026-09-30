function S = noiseBudget(Obj, Args)
    % Noise budget and signal-to-noise curve of one setup, per pixel, in
    % electrons -- the quantity that decides whether signals of several tens
    % of ADU can be measured.
    %   For an incident charge Q the pixel collects Qc = Q - max(T, 0), the
    %   first T electrons being lost to the threshold when there is one, and
    %     sigma_eff^2(Q) = RN^2 + Qc + DC*t
    %                      + [(1-f)*sigma_T]^2        offset fixed pattern
    %                      + [(1-f)*sigma_DC*t]^2     DSNU
    %                      + [(1-f)*PRNU*Qc]^2        photo-response FPN
    %     SNR(Q)          = Qc / sigma_eff(Q)
    %   with f = 1 when the fixed patterns are removed by calibration and
    %   f = 0 when they are not. Both flavours are returned: the dark
    %   pattern of this detector was measured to repeat to 95-103% between
    %   runs, so it is static and f = 1 is defensible, but the uncalibrated
    %   curve is what a single raw frame gives.
    %   All fixed-pattern terms are the INTRINSIC spreads (fit noise
    %   removed, see perPixelThreshold / perPixelFits), so they do not
    %   inherit the setup-dependent lever arm of the ladder.
    %   Shot noise: with the gain in ADU/e- the variance of Qc electrons is
    %   Qc electrons^2, so the whole budget is written in electrons and only
    %   RN is converted (RN_e = RN_ADU / Gain).
    % Input  : * ...,key,val,...
    %            'Threshold' - perPixelThreshold output ([] = computed).
    %            'Zero'      - zeroNoiseStats output ([] = computed).
    %            'Mask'      - pixels to use ([] = all).
    %            'Parity'    - 'All' (default) | 'Even' | 'Odd'.
    %            'Method'    - threshold method for T: 'light' (default) or
    %                          'dark'; 'none' sets T = 0 while keeping the
    %                          fixed patterns, which isolates what the
    %                          threshold alone costs (compare Qlim).
    %            'Q'         - charge grid [e-] (default logspace(0,3,181)).
    %            'ExpTime'   - [s] integration time (default ExpSen).
    %            'SNR'       - detection SNR for the limiting signal (5).
    % Output : - Structure with Q, SNR_raw, SNR_cal, SigmaEff_raw,
    %            SigmaEff_cal [e-], the term-by-term budget at every Q
    %            (Terms.RN, .Shot, .DarkShot, .OffsetFPN, .DSNU, .PRNU),
    %            the inputs used (RN_e, DC_e, SigmaT_e, SigmaDC_e, PRNU,
    %            Threshold_e, Gain, ExpTime) and the limiting signals
    %            Qlim_raw / Qlim_cal [e-] at which SNR = Args.SNR
    %            (NaN when the curve never reaches it).
    % Example: P.run;  N = P.noiseBudget;  semilogx(N.Q, N.SNR_cal)
    arguments
        Obj
        Args.Threshold = [];
        Args.Zero      = [];
        Args.Mask      = [];
        Args.Parity    = 'All';
        Args.Method    = 'light';
        Args.Q         = [];
        Args.ExpTime   = [];
        Args.SNR (1,1) double = 5;
    end
    Th = Args.Threshold;
    Ze = Args.Zero;
    if isempty(Th)
        Th = Obj.perPixelThreshold('Mask',Args.Mask);
    end
    if isempty(Ze)
        Ze = Obj.zeroNoiseStats('Mask',Args.Mask);
    end
    if ~isfield(Th, Args.Parity) || ~isfield(Ze, Args.Parity)
        error('ultrasat:lab:PTCAnalysis:parity', 'No %s subset (run with Parity=''rawcol'')', Args.Parity);
    end
    Tq = Th.(Args.Parity);
    Zq = Ze.(Args.Parity);
    G  = Th.GainUsed;
    Tt = Args.ExpTime;
    if isempty(Tt)
        Tt = Th.ExpSen;
    end
    Q = Args.Q;
    if isempty(Q)
        Q = logspace(0, 3, 181);
    end
    switch lower(Args.Method)
        case 'light'
            Te = Tq.MedianLightE;
        case 'dark'
            Te = Tq.MedianDarkE;
        case 'none'
            Te = 0;
        otherwise
            error('ultrasat:lab:PTCAnalysis:method', 'Unknown Method %s', Args.Method);
    end
    % Offset fixed pattern: from the additive term of the bright-ladder
    % pattern fit, which is well measured, and NOT from the spread of the
    % per-pixel threshold, which the 3-step bright window cannot resolve
    % (its apparent spread is almost entirely intercept fit noise).
    Se = NaN;
    if isfield(Tq, 'OffsetFPN_e')
        Se = Tq.OffsetFPN_e;
    end
    if ~isfinite(Se)
        switch lower(Args.Method)
            case 'light', Se = Tq.StdLightIntrE;
            case 'dark',  Se = Tq.StdDarkIntrE;
            otherwise,    Se = 0;
        end
    end

    RNe  = Zq.ReadNoiseMedian./G;
    DCe  = Tq.MedianDCE;
    SDCe = Tq.StdDCIntrE;
    PRNU = Tq.PRNU;
    if ~isfinite(PRNU), PRNU = 0; end
    if ~isfinite(Se),   Se = 0;   end
    if ~isfinite(SDCe), SDCe = 0; end

    B = ultrasat.lab.PTCAnalysis.budgetCurve(Q, ...
            struct('RN_e',RNe, 'DC_e',DCe, 'SigmaT_e',Se, 'SigmaDC_e',SDCe, ...
                   'PRNU',PRNU, 'Threshold_e',Te, 'ExpTime',Tt, 'SNRdet',Args.SNR));
    S = B;
    S.Parity = Args.Parity;
    S.Method = Args.Method;
    S.Gain   = G;
end
