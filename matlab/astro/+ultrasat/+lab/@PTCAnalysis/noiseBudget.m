function S = noiseBudget(Obj, Args)
    % Noise budget and signal-to-noise curve of one setup, per pixel, in
    % electrons -- the quantity that decides whether signals of several tens
    % of ADU can be measured.
    %   For an incident charge Q the pixel collects Qc = max(Q-T, 0), the
    %   first T electrons being lost to the threshold, and
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
    %                          'dark'; 'none' sets T = 0.
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
    if strcmpi(Args.Method, 'none')
        Se = 0;
    end
    RNe  = Zq.ReadNoiseMedian./G;
    DCe  = Tq.MedianDCE;
    SDCe = Tq.StdDCIntrE;
    PRNU = Tq.PRNU;
    if ~isfinite(PRNU), PRNU = 0; end
    if ~isfinite(Se),   Se = 0;   end
    if ~isfinite(SDCe), SDCe = 0; end

    Qc = max(Q - Te, 0);                              % charge actually collected
    S  = struct('Q',Q, 'Qc',Qc, 'Parity',Args.Parity, 'Method',Args.Method, ...
                'Gain',G, 'ExpTime',Tt, 'SNRdet',Args.SNR);
    S.RN_e = RNe;  S.DC_e = DCe;  S.SigmaT_e = Se;  S.SigmaDC_e = SDCe;
    S.PRNU = PRNU; S.Threshold_e = Te;
    S.Terms = struct('RN',RNe.^2 + 0.*Q, 'Shot',Qc, 'DarkShot',DCe.*Tt + 0.*Q, ...
                     'OffsetFPN',Se.^2 + 0.*Q, 'DSNU',(SDCe.*Tt).^2 + 0.*Q, 'PRNU',(PRNU.*Qc).^2);
    Base = S.Terms.RN + S.Terms.Shot + S.Terms.DarkShot;
    S.SigmaEff_cal = sqrt(Base);
    S.SigmaEff_raw = sqrt(Base + S.Terms.OffsetFPN + S.Terms.DSNU + S.Terms.PRNU);
    S.SNR_cal = Qc./S.SigmaEff_cal;
    S.SNR_raw = Qc./S.SigmaEff_raw;
    S.Qlim_cal = local_lim(S.SNR_cal);
    S.Qlim_raw = local_lim(S.SNR_raw);

    function Ql = local_lim(Snr)
        % lowest Q at which SNR crosses the detection level (log interpolation)
        Ql = NaN;
        Ix = find(Snr>=Args.SNR, 1, 'first');
        if isempty(Ix)
            return
        end
        if Ix==1
            Ql = Q(1);
        else
            Ql = exp(interp1(Snr([Ix-1 Ix]), log(Q([Ix-1 Ix])), Args.SNR, 'linear'));
        end
    end
end
