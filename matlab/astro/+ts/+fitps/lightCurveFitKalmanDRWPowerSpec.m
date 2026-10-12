function [L, Info] = lightCurveFitKalmanDRWPowerSpec(Time, Flux, FluxErr, Args)
% lightCurveFitKalmanDRWPowerSpec - Damped-random-walk (DRW) fit via an O(N) Kalman filter.
%
% Description:
%   Fit a damped random walk (DRW) -- i.e. an Ornstein-Uhlenbeck process --
%   plus independent Gaussian measurement noise to an irregularly sampled
%   light curve, in the TIME DOMAIN. The source has the exponential
%   (full-frequency) covariance
%
%       C(dt) = A .* exp(-|dt|/tau),         A = sigma_DRW^2,
%
%   which is the Wiener-Khinchin transform of the Lorentzian DRW PSD
%
%       P(f) = 4 A tau / (1 + (2 pi f tau)^2)
%
%   integrated over ALL frequencies (0 <= f < inf). This is the "full-
%   covariance" DRW: unlike the band-limited models fitted by
%   lightCurveFitPowerLawPowerSpec, it carries no [f_low,f_high] cutoffs.
%
%   Because the DRW is Markovian, its Gaussian log-likelihood
%   (Equation 1 below) can be evaluated by a forward Kalman filter in O(N)
%   operations, instead of the O(N^3) of a direct Cholesky factorization of
%   the N-by-N covariance. The O(N) evaluation of the exponential-covariance
%   likelihood originates with Rybicki & Press (1992); the Kalman-filter form
%   used here follows Kelly et al. (2009). See also celerite
%   (Foreman-Mackey et al. 2017) for the semiseparable generalization.
%
%       ln L = -1/2 [ r' C^-1 r + ln det(C) + N ln(2 pi) ]            (Eq. 1)
%
%   The amplitude A (= sigma_DRW^2) and an unknown constant mean flux are
%   profiled: the mean is profiled exactly inside the filter (via two
%   co-propagated channels -- the data and a constant -- that yield the
%   generalized-least-squares normal equations), and A is optimized per tau
%   by a 1-D line search. The damping timescale tau is scanned on a grid.
%
% Input arguments:
%   Time    - Observation times [N x 1]. Arbitrary (irregular) sampling.
%   Flux    - Flux measurements [N x 1].
%   FluxErr - 1-sigma independent Gaussian measurement errors. Scalar or [N x 1].
%   Args    - Optional key/value arguments:
%       .Tau           - Vector of DRW damping timescales to scan [same units
%                        as Time]. Default: [] -> NTau log-spaced values from
%                        0.5 to 5*(time span).
%       .NTau          - Number of log-spaced tau values when .Tau is empty.
%                        Default: 50.
%       .FitMean       - Profile an unknown constant mean flux. Default: true.
%       .FixNorm       - If non-empty, hold A=sigma_DRW^2 fixed at this scalar
%                        value for every tau (no amplitude optimization, and
%                        the A=0 boundary is not tested). Useful for evaluating
%                        the likelihood on a (tau,A) grid. Default: [] (optimize).
%       .NormRangeFact - Optimize A over Aguess*[1/F,F]. Default: 1e8.
%       .NormTol       - Tolerance (in ln A) of the amplitude line search.
%                        Default: 1e-4.
%
% Output:
%   L    - Profiled log-likelihood at each scanned tau [numel(Tau) x 1].
%   Info - Structure with fields:
%          .Tau           - Scanned tau grid [1 x nTau].
%          .LogLikelihood - Profiled ln L at each tau [1 x nTau].
%          .PowerSpecNorm - Optimized A = sigma_DRW^2 at each tau [1 x nTau].
%          .SigmaDRW      - sqrt(A), the DRW rms amplitude [1 x nTau].
%          .SFinf         - sqrt(2 A), the structure-function asymptote.
%          .MeanFlux      - Profiled mean at each tau [1 x nTau].
%          .NData         - Number of valid points used.
%          .Best          - Sub-structure for the maximum-likelihood tau:
%                           .LogLikelihood .Tau .PowerSpecNorm .SigmaDRW
%                           .SFinf .MeanFlux .Index .RailLow .RailHigh
%
% References:
%   Rybicki, G.B. & Press, W.H. 1992, ApJ, 398, 169.
%   Kelly, B.C., Bechtold, J. & Siemiginowska, A. 2009, ApJ, 698, 895.
%   Foreman-Mackey, D., et al. 2017, AJ, 154, 220 (celerite).
%
% Example:
%   % Simulate a DRW and recover its parameters:
%   rng(1); N=600; T=sort(1000*rand(N,1)); tau0=120; sig0=0.3; mu0=5;
%   phi=exp(-diff(T)/tau0); x=zeros(N,1); x(1)=sig0*randn;
%   for i=2:N, x(i)=phi(i-1)*x(i-1)+sig0*sqrt(1-phi(i-1)^2)*randn; end
%   E=0.03*ones(N,1); F=mu0+x+E.*randn(N,1);
%   [L,Info]=ts.fitps.lightCurveFitKalmanDRWPowerSpec(T,F,E,'Tau',logspace(0,3,60));
%   [Info.Best.Tau, Info.Best.SigmaDRW, Info.Best.MeanFlux]
%
% Author: (Claude) for E. Ofek (Oct 2026)
% See also: ts.fitps.lightCurveFitPowerLawPowerSpec

    arguments
        Time (:,1) double
        Flux (:,1) double
        FluxErr double
        Args.Tau double = []
        Args.NTau (1,1) double {mustBeInteger,mustBePositive} = 50
        Args.FitMean (1,1) logical = true
        Args.FixNorm double = []
        Args.NormRangeFact (1,1) double = 1e8
        Args.NormTol (1,1) double = 1e-4
    end

    % ---- clean, sort, validate ----
    Ninput = numel(Time);
    if isscalar(FluxErr)
        FluxErr = repmat(FluxErr, Ninput, 1);
    else
        FluxErr = FluxErr(:);
    end
    Flux = Flux(:);
    Good = isfinite(Time) & isfinite(Flux) & isfinite(FluxErr) & FluxErr>0;
    Time = Time(Good);  Flux = Flux(Good);  FluxErr = FluxErr(Good);
    [Time, SI] = sort(Time);  Flux = Flux(SI);  FluxErr = FluxErr(SI);
    N = numel(Time);
    if N<3
        error('lightCurveFitKalmanDRWPowerSpec:tooFewPoints', ...
              'At least three valid measurements are required.');
    end
    Span = Time(end) - Time(1);

    % ---- tau grid ----
    if isempty(Args.Tau)
        TauVec = logspace(log10(0.5), log10(5*Span), Args.NTau);
    else
        TauVec = Args.Tau(:).';
    end
    TauVec = TauVec(isfinite(TauVec) & TauVec>0);
    nT = numel(TauVec);

    % ---- precompute per-point quantities ----
    Dt   = [Inf; diff(Time)];          % Dt(1) is unused by the filter
    Err2 = FluxErr.^2;
    VarData  = var(Flux, 1);
    VarNoise = mean(Err2);
    VarGuess = max(VarData - VarNoise, 0.01.*max(VarData, realmin));
    FixA = ~isempty(Args.FixNorm);

    % ---- scan tau; profile A (and the mean, inside the filter) ----
    L       = -Inf(nT,1);
    NormVec = NaN(nT,1);
    MeanVec = NaN(nT,1);
    for It = 1:nT
        Tau = TauVec(It);
        if FixA
            Ab = Args.FixNorm(1);
            [NLLb, Mub] = localOU_NLL(Ab, Tau, Dt, Flux, Err2, N, Args.FitMean);
        else
            Obj  = @(LogA) localOU_NLL(exp(LogA), Tau, Dt, Flux, Err2, N, Args.FitMean);
            Amin = max(VarGuess./Args.NormRangeFact, realmin);
            Amax = VarGuess.*Args.NormRangeFact;
            Opt  = optimset('Display','off','TolX',Args.NormTol);
            [LogAb, NLLb] = fminbnd(Obj, log(Amin), log(Amax), Opt);
            Ab = exp(LogAb);
            [~, Mub] = localOU_NLL(Ab, Tau, Dt, Flux, Err2, N, Args.FitMean);
            % A=0 boundary (pure white noise); quad/logdet are analytic there
            [NLL0, Mu0] = localWhiteNLL(Flux, Err2, N, Args.FitMean);
            if NLL0 < NLLb
                NLLb = NLL0;  Ab = 0;  Mub = Mu0;
            end
        end
        L(It) = -NLLb;  NormVec(It) = Ab;  MeanVec(It) = Mub;
    end

    % ---- assemble Info ----
    Info = struct;
    Info.Tau           = TauVec;
    Info.LogLikelihood = L(:).';
    Info.PowerSpecNorm = NormVec(:).';
    Info.SigmaDRW      = sqrt(max(NormVec(:).', 0));
    Info.SFinf         = sqrt(2.*max(NormVec(:).', 0));
    Info.MeanFlux      = MeanVec(:).';
    Info.NData         = N;
    [BestL, Bi] = max(L);
    Info.Best = struct('LogLikelihood', BestL);
    if isfinite(BestL)
        Info.Best.Tau           = TauVec(Bi);
        Info.Best.PowerSpecNorm = NormVec(Bi);
        Info.Best.SigmaDRW      = sqrt(max(NormVec(Bi),0));
        Info.Best.SFinf         = sqrt(2.*max(NormVec(Bi),0));
        Info.Best.MeanFlux      = MeanVec(Bi);
        Info.Best.Index         = Bi;
        Info.Best.RailLow       = (Bi==1);
        Info.Best.RailHigh      = (Bi==nT);
    else
        [Info.Best.Tau, Info.Best.PowerSpecNorm, Info.Best.SigmaDRW, ...
         Info.Best.SFinf, Info.Best.MeanFlux, Info.Best.Index] = deal(NaN);
        Info.Best.RailLow = false;  Info.Best.RailHigh = false;
    end
end

% ======================================================================= %
function [NLL, Mu] = localOU_NLL(A, Tau, Dt, Y, Err2, N, FitMean)
    % Profiled-mean negative log-likelihood of an OU (DRW) process + white
    % noise, via a forward Kalman filter (O(N)). Two channels are propagated
    % together: the data Y and a constant 1, sharing the innovation variance
    % F. Accumulating the GLS sums (Saa,Sab,Sbb) lets the mean be profiled
    % exactly: Mu = Sab/Sbb, and the mean-subtracted quadratic form is
    % quad = Saa - Sab^2/Sbb. The state is the latent OU value x(t).
    if A<=0 || ~isfinite(A)
        NLL = Inf;  Mu = NaN;  return
    end
    XY = 0;  X1 = 0;  P = 0;
    Saa = 0; Sab = 0; Sbb = 0;  LogDet = 0;
    for I = 1:N
        if I==1
            Pp = A;  XYp = 0;  X1p = 0;              % stationary prior, Var=A
        else
            Phi = exp(-Dt(I)/Tau);
            Pp  = Phi*Phi*P + A*(1-Phi*Phi);         % OU state prediction
            XYp = Phi*XY;   X1p = Phi*X1;
        end
        F = Pp + Err2(I);                            % innovation variance
        a = Y(I) - XYp;   b = 1 - X1p;               % data / constant innovations
        Saa = Saa + a*a/F;  Sab = Sab + a*b/F;  Sbb = Sbb + b*b/F;
        LogDet = LogDet + log(F);
        K = Pp/F;                                    % Kalman gain
        XY = XYp + K*a;  X1 = X1p + K*b;  P = Pp - K*Pp;
    end
    if FitMean
        if ~(isfinite(Sbb) && Sbb>0), NLL = Inf; Mu = NaN; return; end
        Mu   = Sab/Sbb;
        Quad = Saa - Sab*Sab/Sbb;
    else
        Mu   = 0;
        Quad = Saa;
    end
    if Quad<0 && Quad>-1e-10*max(Saa,1), Quad = 0; end
    if ~(isfinite(Quad) && Quad>=0), NLL = Inf; Mu = NaN; return; end
    NLL = 0.5*(LogDet + Quad + N*log(2*pi));
end

% ======================================================================= %
function [NLL, Mu] = localWhiteNLL(Y, Err2, N, FitMean)
    % A=0 limit: pure white noise. Covariance is diagonal, so the quadratic
    % form and log-determinant are analytic and the (optional) mean is the
    % inverse-variance-weighted mean.
    if FitMean
        W  = 1./Err2;
        Mu = sum(Y.*W)/sum(W);
    else
        Mu = 0;
    end
    R    = Y - Mu;
    Quad = sum(R.*R./Err2);
    NLL  = 0.5*(sum(log(Err2)) + Quad + N*log(2*pi));
end
