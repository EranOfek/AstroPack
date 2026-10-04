function S = perPixelThreshold(Obj, Args)
    % Per-pixel charge thresholds and dark current, with the fit noise of
    % each quantity propagated and removed from its pixel-to-pixel spread.
    %   Same two methods as threshold, but built on perPixelFits (all steps
    %   below the linearity limit, weighted) instead of the narrow DESY
    %   window, and with error propagation:
    %     dark method : T = -I_D           Var(T) = Var(I_D)
    %     light method: T = DC*t - I_B     Var(T) = t^2*Var(DC) + Var(I_B)
    %   where S_dark(t) = DC*t + I_D and S_bright(int) = R*int + I_B (all
    %   bright frames at t = ExpSen), and the two fits are independent.
    %   Electrons = ADU / PTC gain; positive threshold = electrons lost.
    %   The spread of the threshold over pixels is an offset fixed pattern,
    %   so it enters the noise budget (see noiseBudget) and is reported both
    %   raw and deconvolved from the fit noise.
    % Input  : * ...,key,val,...
    %            'DarkFit', 'BrightFit' - perPixelFits outputs to reuse
    %                          ([] = computed here).
    %            'Pattern' - stepFixedPattern('B') output to reuse ([] =
    %                          computed here); supplies the PRNU.
    %            'Select', 'LinLimit', 'Mask', 'Robust' - passed on / used
    %                          in the summaries (see perPixelFits).
    % Output : - Structure with GainUsed, ExpSen, the per-pixel maps DarkADU,
    %            DarkE, LightADU, LightE, VarDarkADU, VarLightADU,
    %            DCADU, DCE, VarDCADU, and All / Even / Odd summaries with
    %            DarkSpread, LightSpread, DCSpread (paramSpread structures,
    %            in ADU and in e-), the medians in electrons, PRNU (from the
    %            fixed pattern of the top bright step used) with its 95%
    %            upper limit and significance, and PRNU_slope (the response
    %            slope spread, kept for comparison only).
    % Example: P.run;  T = P.perPixelThreshold;  T.All.DarkSpread.RelIntr
    arguments
        Obj
        Args.DarkFit   = [];
        Args.BrightFit = [];
        Args.Pattern   = [];
        Args.Select = 'fitrange';
        Args.LinLimit (1,1) double = 2900;
        Args.Mask = [];
        Args.Robust logical = true;
    end
    FD = Args.DarkFit;
    FB = Args.BrightFit;
    if isempty(FD)
        FD = Obj.perPixelFits('D', 'Select',Args.Select, 'LinLimit',Args.LinLimit, 'Mask',Args.Mask, 'Robust',Args.Robust);
    end
    if isempty(FB)
        FB = Obj.perPixelFits('B', 'Select',Args.Select, 'LinLimit',Args.LinLimit, 'Mask',Args.Mask, 'Robust',Args.Robust);
    end
    Pt = Args.Pattern;
    if isempty(Pt)
        Pt = Obj.stepFixedPattern('B', 'Mask',Args.Mask, 'Robust',Args.Robust);
    end
    G = Obj.PTC.GainUsed;
    T = Obj.ExpSen;
    S = struct('GainUsed',G, 'GainSource',Obj.PTC.GainSource, 'ExpSen',T, ...
               'Select',Args.Select, 'LinLimit',Args.LinLimit, ...
               'DarkSteps',FD.Steps, 'BrightSteps',FB.Steps);
    S.DCADU      = FD.Slope;
    S.VarDCADU   = FD.VarSlope;
    S.DCE        = FD.Slope./G;
    S.DarkADU    = -FD.Intercept;
    S.VarDarkADU = FD.VarIntercept;
    S.DarkE      = S.DarkADU./G;
    S.LightADU    = FD.Slope.*T - FB.Intercept;
    S.VarLightADU = T.^2.*FD.VarSlope + FB.VarIntercept;
    S.LightE      = S.LightADU./G;
    S.RespADU     = FB.Slope;
    S.VarRespADU  = FB.VarSlope;

    Mask = Args.Mask;
    if isempty(Mask)
        Mask = true(size(S.DarkADU));
    end
    S.PatternStep   = Pt.Step(end);
    S.PatternX      = Pt.X(end);
    S.PatternNsteps = Pt.All.PatternNsteps;
    S.All = local_sum(Mask, 'All');
    if ~isempty(Obj.ParityMap)
        S.Even = local_sum(Mask & ~Obj.ParityMap, 'Even');
        S.Odd  = local_sum(Mask &  Obj.ParityMap,  'Odd');
    end

    function Q = local_sum(M, Pn)
        Q = struct('Npix',nnz(M));
        if nnz(M)<3
            return
        end
        Q.DarkSpread  = ultrasat.lab.PTCAnalysis.paramSpread(S.DarkADU(M),  S.VarDarkADU(M),  'Robust',Args.Robust);
        Q.LightSpread = ultrasat.lab.PTCAnalysis.paramSpread(S.LightADU(M), S.VarLightADU(M), 'Robust',Args.Robust);
        Q.DCSpread    = ultrasat.lab.PTCAnalysis.paramSpread(S.DCADU(M),    S.VarDCADU(M),    'Robust',Args.Robust);
        Q.RespSpread  = ultrasat.lab.PTCAnalysis.paramSpread(S.RespADU(M),  S.VarRespADU(M),  'Robust',Args.Robust);
        Q.MedianDarkE  = Q.DarkSpread.Median./G;
        Q.MedianLightE = Q.LightSpread.Median./G;
        Q.MedianDCE    = Q.DCSpread.Median./G;
        Q.StdDarkIntrE  = Q.DarkSpread.StdIntr./G;      % offset fixed pattern [e-]
        Q.StdLightIntrE = Q.LightSpread.StdIntr./G;
        Q.StdDCIntrE    = Q.DCSpread.StdIntr./G;        % DSNU [e-/s]
        % PRNU from the fixed pattern of the top bright step used, NOT from
        % the spread of the 3-step response slope (which is all fit noise)
        Q.PRNU_slope = Q.RespSpread.RelIntr;
        Q.PRNU       = NaN;   Q.PRNU_UL95 = NaN;   Q.PRNU_Err = NaN;
        Q.OffsetFPN_ADU = NaN;  Q.OffsetFPN_e = NaN;  Q.OffsetFPN_Err = NaN;
        if isfield(Pt, Pn)
            % PRNU and the additive offset pattern from the whole bright
            % ladder (sigma_fixed^2 = a^2 + (b*S)^2), not from one step
            Q.PRNU          = Pt.(Pn).Multiplicative;
            Q.PRNU_Err      = Pt.(Pn).MultiplicativeErr;
            Q.PRNU_UL95     = Q.PRNU + 1.645.*Q.PRNU_Err;
            Q.PRNU_step     = Pt.(Pn).RelFixed(end);
            Q.OffsetFPN_ADU = Pt.(Pn).Additive;
            Q.OffsetFPN_Err = Pt.(Pn).AdditiveErr;
            Q.OffsetFPN_e   = Q.OffsetFPN_ADU./G;
        end
    end
end
