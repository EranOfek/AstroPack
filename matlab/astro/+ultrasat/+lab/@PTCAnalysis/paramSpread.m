function S = paramSpread(P, VarFit, Args)
    % Intrinsic pixel-to-pixel spread of a FITTED parameter, with the
    % analytic fit noise removed.
    %   The observed spread of a per-pixel fit parameter over the pixels is
    %   (true spread) (+) (fit noise), the latter being the propagated
    %   photon and read noise of the ladder points. It depends on the lever
    %   arm of the fit, so it differs from setup to setup and must be
    %   subtracted before spreads are compared between setups.
    % Input  : - Per-pixel parameter map (any shape).
    %          - Per-pixel analytic variance of that parameter (same shape).
    %          * ...,key,val,...
    %            'Robust' - use 1.4826*MAD instead of std for the observed
    %                       spread (default true: hot pixels and cosmic rays
    %                       inflate std).
    %   The observed spread and the fit noise must be estimated the same
    %   way or the subtraction is biased: a noisy pixel has a LARGER fitted
    %   variance (smaller weight, wider parameter error), so a mean-based
    %   StdFit against a MAD-based StdObs subtracts too much and makes every
    %   fixed pattern look smaller than it is. With 'Robust' the median of
    %   VarFit is used, with 'Robust',false its mean; both are reported.
    %   Sigma is computed with the Gaussian sampling error of a variance,
    %   so with 'Robust' (MAD is ~37% efficient) it is optimistic by ~1.6x.
    % Output : - Structure: Npix, Median, Mean, StdObs, StdRobust, StdFit,
    %            StdFitRobust, StdIntr (deconvolved, 0 if not detected),
    %            RelIntr (relative to |Median|), RelUL95, Sigma.
    % Example: S = ultrasat.lab.PTCAnalysis.paramSpread(F.Intercept, F.VarIntercept)
    arguments
        P
        VarFit
        Args.Robust logical = true;
    end
    P = double(P(:));  VarFit = double(VarFit(:));
    Ok = isfinite(P) & isfinite(VarFit);
    P  = P(Ok);  VarFit = VarFit(Ok);
    N  = numel(P);
    S  = struct('Npix',N, 'Median',NaN, 'Mean',NaN, 'StdObs',NaN, 'StdRobust',NaN, ...
                'StdFit',NaN, 'StdIntr',NaN, 'RelIntr',NaN, 'RelUL95',NaN, 'Sigma',NaN);
    if N<2
        return
    end
    S.Median    = median(P);
    S.Mean      = mean(P);
    S.StdObs    = std(P);
    S.StdRobust = 1.4826.*median(abs(P - S.Median));
    S.StdFit       = sqrt(mean(VarFit));
    S.StdFitRobust = sqrt(median(VarFit));
    if Args.Robust
        Obs = S.StdRobust;   Fit = S.StdFitRobust;
    else
        Obs = S.StdObs;      Fit = S.StdFit;
    end
    VarT      = Obs.^2 - Fit.^2;
    S.StdIntr = sqrt(max(VarT, 0));
    SE        = Obs.^2.*sqrt(2./(N-1));                 % sampling error of Obs^2
    S.RelIntr = S.StdIntr./abs(S.Median);
    S.RelUL95 = sqrt(max(VarT + 1.645.*SE, 0))./abs(S.Median);
    S.Sigma   = VarT./SE;
end
