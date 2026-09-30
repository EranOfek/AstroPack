function S = varSpread(V, Dof)
    % Intrinsic pixel-to-pixel spread of a per-pixel VARIANCE estimate.
    %   A per-pixel variance measured from Dof+1 frames is V_i = T_i*X/Dof
    %   with X ~ chi2(Dof) and T_i the true variance of that pixel, so
    %     E[V]   = E[T]
    %     Var[V] = Var[T]*(1+2/Dof) + (2/Dof)*E[T]^2
    %   and the sampling scatter of the estimator (the second term) must be
    %   removed before the spread of V over pixels can be read as a spread
    %   of the true variance. With few frames it dominates: sd/mean = 1 for
    %   Dof = 2 even when every pixel is identical.
    % Input  : - Per-pixel variances (any shape; non-finite entries dropped).
    %          - Degrees of freedom of each estimate (Nframes-1).
    % Output : - Structure (same units as V unless noted):
    %            Npix, Dof; MeanVar - mean over pixels;
    %            StdObs  - observed spread over pixels;
    %            StdNoise - spread expected if all pixels were identical;
    %            StdIntr - deconvolved intrinsic spread (0 if not detected);
    %            RelIntr - StdIntr/MeanVar; RelUL95 - 95% upper limit on it;
    %            Sigma   - significance of the intrinsic component.
    % Reference: sampling error of the sample variance of a Gamma(Dof/2)
    %            variate, Var(s^2) = (2/K^2 + 6/K^3)*MeanVar^4/Npix, K=Dof/2.
    % Example: S = ultrasat.lab.PTCAnalysis.varSpread(P.ZeroNoise.^2, P.NZero-1)
    arguments
        V
        Dof (1,1) double
    end
    V = double(V(:));
    V = V(isfinite(V));
    N = numel(V);
    K = Dof./2;
    S = struct('Npix',N, 'Dof',Dof, 'MeanVar',NaN, 'StdObs',NaN, 'StdNoise',NaN, ...
               'StdIntr',NaN, 'RelIntr',NaN, 'RelUL95',NaN, 'Sigma',NaN);
    if N<2 || Dof<1
        return
    end
    S.MeanVar  = mean(V);
    S.StdObs   = std(V);
    S.StdNoise = sqrt(2./Dof).*S.MeanVar;
    VarT       = (S.StdObs.^2 - S.StdNoise.^2)./(1 + 2./Dof);
    S.StdIntr  = sqrt(max(VarT, 0));
    % sampling error of StdObs^2 under the all-pixels-identical model
    SE       = sqrt((2./K.^2 + 6./K.^3).*S.MeanVar.^4./N)./(1 + 2./Dof);
    S.RelIntr = S.StdIntr./S.MeanVar;
    S.RelUL95 = sqrt(max(VarT + 1.645.*SE, 0))./S.MeanVar;
    S.Sigma   = VarT./SE;
end
