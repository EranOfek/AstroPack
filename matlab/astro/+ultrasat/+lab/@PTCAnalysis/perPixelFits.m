function S = perPixelFits(Obj, Type, Args)
    % Per-pixel response fit in the individual-pixel regime, with the fit
    % noise of every parameter computed analytically and removed from the
    % pixel-to-pixel spread.
    %   The steps are selected exactly as in fitResponse (FitRange /
    %   FitSteps, including the 'auto' rule), so the fit window is the same
    %   as in the reports: the dark ladder is non-linear at BOTH ends -- a
    %   knee at low signal (the three lowest steps of run 31 lie +55, +32
    %   and +13 ADU above the line) and the INL above ~2.9 kADU (step 9 at
    %   -39 ADU) -- and only a two-sided window avoids both. 'Select',
    %   'linlimit' fits instead every step below LinLimit, which lengthens
    %   the lever arm at the price of the low-signal knee.
    %   The points are weighted with their variance. By default
    %   ('Weights','measured') that variance is MEASURED, not modelled:
    %       sigma_k,i^2 = [ V_k + (RN_i^2 - median RN^2) ] / Nrep_k   [ADU^2]
    %   where V_k is the median over pixels of the per-pixel temporal
    %   variance of step k, corrected for the chi2 median bias
    %   (median = Chi2med/Dof * variance; ln2 for 3 repeats). This is the
    %   right weight and needs no model: the shot noise of a ladder point
    %   follows the COLLECTED charge Q, not the measured signal Q-T, since
    %   the first T electrons are lost after they have fluctuated -- the
    %   same fact that makes the PTC intercept g*T rather than the read
    %   noise. A modelled weight (RN^2 + Gain*S_k), available as
    %   'Weights','model', misses that term and over-weights the lowest
    %   dark steps, whose measured signal can even be negative.
    %   Only the per-step median enters, so the weights do not correlate
    %   with an individual pixel's data. The parameter
    %   covariance of a weighted straight line is then exact:
    %       Var(Slope) = Sw/D,  Var(Intercept) = Swxx/D,  D = Sw*Swxx-Swx^2
    %   and it is the fit noise that must be taken out of the observed
    %   spread of Slope / Intercept over the pixels (see paramSpread).
    %   This matters because the lever arm depends on the setup: a low
    %   dark-current run whose dark ladder only reaches ~150 ADU has a much
    %   noisier intercept than one reaching 3500 ADU, and without the
    %   correction that difference would be read as a larger true spread.
    % Input  : - 'D' (signal vs exposure time) or 'B' (vs intensity).
    %          * ...,key,val,...
    %            'Select'    - 'fitrange' (default): the steps chosen by
    %                          FitRange / FitSteps, as in fitResponse;
    %                          'linlimit': every step below LinLimit.
    %            'LinLimit'  - [ADU] highest step median to fit with
    %                          'linlimit' (default 2900, where the measured
    %                          INL is still <0.5%).
    %            'MinSignal' - [ADU] lowest step median ([] = none), 'linlimit'.
    %            'MinSteps'  - minimum number of steps for 'linlimit'; the
    %                          window is topped up with the lowest steps.
    %            'Mask'      - logical map of the pixels to summarise ([]=all).
    %            'Weights'   - 'measured' (default), 'model' or 'none' (OLS).
    %            'Weighted'  - false forces 'none' (kept for readability).
    %            'Robust'    - robust observed spread in the summary
    %                          (default true, see paramSpread).
    % Output : - Structure with the per-pixel maps Slope, Intercept,
    %            VarSlope, VarIntercept, CovSlopeIntercept, ResidRMS,
    %            Chi2Dof (with 'Weights','none' the parameter variances are
    %            scaled by the OLS residual variance and Chi2Dof is that
    %            variance rather than a goodness of fit); the selection (Steps, X, StepMedian, Nused,
    %            LinLimit, GainADU, Weighted); and All / Even / Odd
    %            summaries, each with SlopeSpread and InterceptSpread
    %            (paramSpread structures: Median, StdObs, StdRobust, StdFit,
    %            StdIntr, RelIntr, RelUL95, Sigma), CorrSlopeIntercept and
    %            MedianChi2Dof.
    % Example: P.run;  F = P.perPixelFits('D');  F.All.InterceptSpread
    arguments
        Obj
        Type
        Args.Select = 'fitrange';
        Args.LinLimit (1,1) double = 2900;
        Args.MinSignal = [];
        Args.MinSteps (1,1) double = 3;
        Args.Mask = [];
        Args.Weighted logical = true;
        Args.Weights = 'measured';
        Args.Robust logical = true;
    end
    if ~Args.Weighted
        Args.Weights = 'none';
    end
    if ~strcmp(Obj.Mode, 'region')
        error('ultrasat:lab:PTCAnalysis:mode', 'perPixelFits needs region mode (per-pixel ladders are not kept in full mode)');
    end
    L = Obj.ladderOf(Type);
    if ~isfield(L, 'Mean') || isempty(L.Mean)
        error('ultrasat:lab:PTCAnalysis:order', 'Run combineSteps before perPixelFits');
    end
    if isempty(Obj.ZeroNoise)
        error('ultrasat:lab:PTCAnalysis:order', 'Run subtractZero before perPixelFits');
    end
    Nstep = numel(L.X);
    Med   = median(reshape(L.Mean, [], Nstep), 1, 'omitnan');
    switch lower(Args.Select)
        case 'fitrange'                              % same window as fitResponse
            [Range, Sel] = Obj.fitSelection(Type, L.Step, L);
        case 'linlimit'
            Range = [-Inf Inf];
            Sel   = isfinite(Med) & Med<min(Args.LinLimit, Obj.SatLevel);
            if ~isempty(Args.MinSignal)
                Sel = Sel & Med>=Args.MinSignal;
            end
            if nnz(Sel)<Args.MinSteps                % top up with the lowest steps
                [~, Order] = sort(Med, 'ascend');
                Order = Order(isfinite(Med(Order)) & ~Sel(Order));
                Sel(Order(1:min(Args.MinSteps-nnz(Sel), numel(Order)))) = true;
            end
        otherwise
            error('ultrasat:lab:PTCAnalysis:select', 'Unknown Select %s', Args.Select);
    end
    Idx   = find(Sel);
    Steps = L.Step(Idx);
    X     = double(L.X(Idx));
    Nrep  = double(L.Nframes(Idx));
    Gn    = 1;
    if isstruct(Obj.PTC) && isfield(Obj.PTC, 'GainUsed') && ~isempty(Obj.PTC.GainUsed)
        Gn = Obj.PTC.GainUsed;
    end
    RN2  = double(Obj.ZeroNoise).^2;
    WMask = Args.Mask;
    if isempty(WMask)
        WMask = true(size(RN2));
    end
    RN2med = median(RN2(WMask & isfinite(RN2)), 'omitnan');
    VarStep = NaN(1, numel(Idx));                  % [ADU^2] variance of ONE frame of the step
    if strcmpi(Args.Weights, 'measured')
        if ~isfield(L, 'VarTemporal') || isempty(L.VarTemporal)
            error('ultrasat:lab:PTCAnalysis:noVar', 'No VarTemporal in the ladder: use Weights=''model''');
        end
        for I = 1:1:numel(Idx)
            Dof = max(Nrep(I)-1, 1);
            Vi  = double(L.VarTemporal(:,:,Idx(I)));
            Vi  = Vi(WMask & isfinite(Vi));
            VarStep(I) = median(Vi).*Dof./(2.*gammaincinv(0.5, Dof./2));
        end
    end

    % weighted sums over the selected steps
    Z = zeros(size(RN2));
    Sw = Z;  Swx = Z;  Swy = Z;  Swxx = Z;  Swxy = Z;  Nok = Z;
    for I = 1:1:numel(Idx)
        Yi = double(L.Mean(:,:,Idx(I)));
        Wi = local_weight(I);
        Ok = isfinite(Yi) & isfinite(Wi) & Wi>0 & Yi>=Range(1) & Yi<=Range(2);
        Wi(~Ok) = 0;  Yi(~Ok) = 0;
        Sw   = Sw   + Wi;
        Swx  = Swx  + Wi.*X(I);
        Swy  = Swy  + Wi.*Yi;
        Swxx = Swxx + Wi.*X(I).^2;
        Swxy = Swxy + Wi.*Yi.*X(I);
        Nok  = Nok  + Ok;
    end
    D      = Sw.*Swxx - Swx.^2;
    Bad    = Nok<3 | ~isfinite(D) | D<=0;
    D(Bad) = NaN;
    S = struct('Type',Type, 'Select',Args.Select, 'Steps',Steps, 'X',X, ...
               'StepMedian',Med(Idx), 'FitRange',Range, 'LinLimit',Args.LinLimit, ...
               'GainADU',Gn, 'Weights',Args.Weights, 'VarStep',VarStep, ...
               'SlopeUnit',Obj.slopeUnit(Type));
    S.Slope             = (Sw.*Swxy - Swx.*Swy)./D;
    S.Intercept         = (Swy.*Swxx - Swx.*Swxy)./D;
    S.VarSlope          = Sw./D;
    S.VarIntercept      = Swxx./D;
    S.CovSlopeIntercept = -Swx./D;
    S.Nused             = Nok;
    % residuals
    Chi2 = Z;  Res2 = Z;
    for I = 1:1:numel(Idx)
        Yi = double(L.Mean(:,:,Idx(I)));
        Ri = Yi - S.Intercept - S.Slope.*X(I);
        Wi = local_weight(I);
        Ok = isfinite(Ri) & Yi>=Range(1) & Yi<=Range(2);
        Ri(~Ok) = 0;
        Chi2 = Chi2 + Wi.*Ri.^2;
        Res2 = Res2 + Ri.^2;
    end
    S.ResidRMS = sqrt(Res2./Nok);
    S.Chi2Dof  = Chi2./max(Nok-2, 1);
    S.ResidRMS(Bad) = NaN;  S.Chi2Dof(Bad) = NaN;
    if strcmpi(Args.Weights, 'none')
        % Equal weights: Sw/D and Swxx/D are in units of the (unknown)
        % variance of one point, so they must be scaled by its OLS estimate
        % s^2 = sum(resid^2)/(n-2). With real weights that scaling is 1 by
        % construction and Chi2Dof is the goodness of fit instead.
        Sig2 = Res2./max(Nok-2, 1);
        S.VarSlope          = S.VarSlope.*Sig2;
        S.VarIntercept      = S.VarIntercept.*Sig2;
        S.CovSlopeIntercept = S.CovSlopeIntercept.*Sig2;
    end

    Mask = Args.Mask;
    if isempty(Mask)
        Mask = true(size(RN2));
    end
    S.All = local_sum(Mask);
    if ~isempty(Obj.ParityMap)
        S.Even = local_sum(Mask & ~Obj.ParityMap);
        S.Odd  = local_sum(Mask &  Obj.ParityMap);
    end

    function Wi = local_weight(I)
        % variance weight of step I, per pixel
        switch lower(Args.Weights)
            case 'measured'
                % floored: a pixel whose read noise sits far below the
                % median must not acquire a near-infinite weight
                Wi = Nrep(I)./max(VarStep(I) + RN2 - RN2med, 0.25.*VarStep(I));
            case 'model'
                Wi = Nrep(I)./(RN2 + Gn.*max(Med(Idx(I)), 0));
            case 'none'
                Wi = ones(size(RN2));
            otherwise
                error('ultrasat:lab:PTCAnalysis:weights', 'Unknown Weights %s', Args.Weights);
        end
    end

    function Q = local_sum(M)
        M = M & isfinite(S.Slope) & isfinite(S.Intercept);
        Q = struct('Npix',nnz(M));
        if nnz(M)<3
            return
        end
        Q.SlopeSpread     = ultrasat.lab.PTCAnalysis.paramSpread(S.Slope(M),     S.VarSlope(M),     'Robust',Args.Robust);
        Q.InterceptSpread = ultrasat.lab.PTCAnalysis.paramSpread(S.Intercept(M), S.VarIntercept(M), 'Robust',Args.Robust);
        Cc = corrcoef(S.Slope(M), S.Intercept(M));
        Q.CorrSlopeIntercept = Cc(1,2);
        Q.MedianChi2Dof      = median(S.Chi2Dof(M), 'omitnan');
        Q.MedianResidRMS     = median(S.ResidRMS(M), 'omitnan');
    end
end
