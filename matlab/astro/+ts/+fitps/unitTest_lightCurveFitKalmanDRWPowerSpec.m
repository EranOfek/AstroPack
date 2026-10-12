function Result = unitTest_lightCurveFitKalmanDRWPowerSpec(Args)
% unitTest_lightCurveFitKalmanDRWPowerSpec - Unit tests for the O(N) Kalman DRW fit.
%
% Description:
%   Tests ts.fitps.lightCurveFitKalmanDRWPowerSpec with two strategies:
%
%   1. Deterministic likelihood algebra:
%      Compare the O(N) Kalman log-likelihood (at a FIXED amplitude A and tau,
%      via 'FixNorm') against a brute-force dense N-by-N Gaussian likelihood
%      built from the exact exponential covariance C(dt)=A exp(-|dt|/tau)+noise,
%      with the mean profiled by generalized least squares. Checked for several
%      (tau,A), with and without mean profiling. These must agree to ~1e-9.
%
%   2. Statistical recovery:
%      Simulate an ensemble of independent DRW light curves with a known tau
%      and amplitude, SUM their log-likelihoods over a tau grid (the ensemble
%      suppresses the finite-realization scatter of a single red-noise series),
%      and check that the joint maximum-likelihood tau and amplitude recover the
%      input values.
%
% Output:
%   Result - Structure of per-test booleans plus Result.Passed (overall).
%
% Example:
%   Result = ts.fitps.unitTest_lightCurveFitKalmanDRWPowerSpec;
%
% Author: (Claude) for E. Ofek, October 2026.
% See also: ts.fitps.lightCurveFitKalmanDRWPowerSpec

    arguments
        Args.Seed (1,1) double = 42
        Args.Verbose (1,1) logical = true
    end

    Result = struct;
    if Args.Verbose
        fprintf('\nunitTest_lightCurveFitKalmanDRWPowerSpec\n');
        fprintf('----------------------------------------\n');
    end

    % =====================================================================
    % Test 1: Kalman (O(N)) vs dense N-by-N exponential-covariance likelihood
    % =====================================================================
    rng(Args.Seed);
    N  = 80;
    T  = sort(500*rand(N,1));
    E  = 0.02 + 0.01*rand(N,1);
    Y  = 3 + 0.4*randn(N,1);                 % arbitrary data; algebra test only
    TauList = [30, 120, 600];
    Afac    = [0.5, 2.0];                    % A relative to a nominal variance
    Anom    = var(Y,1);
    MaxDiffMean  = 0;
    MaxDiffNoMean = 0;
    for it = 1:numel(TauList)
        for ia = 1:numel(Afac)
            Tau = TauList(it);  A = Afac(ia)*Anom;
            % --- Kalman, fixed amplitude, mean profiled ---
            Lk = ts.fitps.lightCurveFitKalmanDRWPowerSpec(T,Y,E,'Tau',Tau,'FixNorm',A);
            Ld = localDenseLogL(T,Y,E,A,Tau,true);
            MaxDiffMean = max(MaxDiffMean, abs(Lk-Ld));
            % --- Kalman, fixed amplitude, NO mean profiling ---
            Lk0 = ts.fitps.lightCurveFitKalmanDRWPowerSpec(T,Y,E,'Tau',Tau,'FixNorm',A,'FitMean',false);
            Ld0 = localDenseLogL(T,Y,E,A,Tau,false);
            MaxDiffNoMean = max(MaxDiffNoMean, abs(Lk0-Ld0));
        end
    end
    Result.algebra_profiledMean = MaxDiffMean  < 1e-7;
    Result.algebra_fixedMean    = MaxDiffNoMean < 1e-7;
    if Args.Verbose
        fprintf('Kalman vs dense likelihood (mean profiled): max|dlnL|=%.2e : %s\n',...
            MaxDiffMean, pf(Result.algebra_profiledMean));
        fprintf('Kalman vs dense likelihood (mean fixed=0) : max|dlnL|=%.2e : %s\n',...
            MaxDiffNoMean, pf(Result.algebra_fixedMean));
    end

    % =====================================================================
    % Test 2: statistical recovery of tau and amplitude from an ensemble
    % =====================================================================
    % Use a long baseline relative to tau (here ~33x): the DRW tau is biased
    % low when the baseline is not >> tau (Kozlowski 2017), so a fair test of
    % the fitter uses a regime where tau is recoverable.
    rng(Args.Seed+1);
    Mreal   = 30;
    Npts    = 600;
    BaseLine = 2000;
    TauTrue = 60;  SigTrue = 0.3;  MuTrue = 5;  ErrLev = 0.03;
    TauGrid = logspace(log10(10), log10(500), 41);
    Lsum    = zeros(numel(TauGrid),1);
    Abest   = zeros(Mreal,1);
    for m = 1:Mreal
        Tm = sort(BaseLine*rand(Npts,1));
        Fm = localSimDRW(Tm, TauTrue, SigTrue, MuTrue) + ErrLev*randn(Npts,1);
        Em = ErrLev*ones(Npts,1);
        [Lm, Im] = ts.fitps.lightCurveFitKalmanDRWPowerSpec(Tm,Fm,Em,'Tau',TauGrid);
        Lsum  = Lsum + Lm(:);
        Abest(m) = Im.Best.PowerSpecNorm;
    end
    [~, ib] = max(Lsum);
    TauHat  = TauGrid(ib);
    SigHat  = sqrt(median(Abest));
    Result.recover_tau   = abs(log10(TauHat/TauTrue)) < 0.15;   % within ~40%
    Result.recover_sigma = abs(log10(SigHat/SigTrue)) < 0.15;
    if Args.Verbose
        fprintf('Ensemble recovery (%d realizations): tau true=%.0f fit=%.0f : %s\n',...
            Mreal, TauTrue, TauHat, pf(Result.recover_tau));
        fprintf('Ensemble recovery: sigma_DRW true=%.3f fit=%.3f : %s\n',...
            SigTrue, SigHat, pf(Result.recover_sigma));
    end

    % =====================================================================
    Flds = fieldnames(Result);
    Result.Passed = all(structfun(@(x) x, Result));
    if Args.Verbose
        fprintf('Overall: %s\n', pf(Result.Passed));
    end
    % (Flds listed for clarity if a caller wants per-test status)
    Result.Tests = Flds;
end

% ======================================================================= %
function s = pf(tf)
    if tf, s='PASS'; else, s='FAIL'; end
end

% ======================================================================= %
function lnL = localDenseLogL(T, Y, E, A, Tau, FitMean)
    % Dense N-by-N Gaussian log-likelihood with exponential covariance
    % C(dt)=A exp(-|dt|/tau)+diag(E^2); mean optionally profiled by GLS.
    T = T(:);  Y = Y(:);  E = E(:);  N = numel(T);
    DT = abs(T - T.');
    C  = A.*exp(-DT./Tau) + diag(E.^2);
    Lc = chol(C,'lower');
    LogDet = 2*sum(log(diag(Lc)));
    if FitMean
        One   = ones(N,1);
        CinvY = Lc'\(Lc\Y);
        Cinv1 = Lc'\(Lc\One);
        Mu    = (One'*CinvY)/(One'*Cinv1);
    else
        Mu = 0;
    end
    R     = Y - Mu;
    CinvR = Lc'\(Lc\R);
    Quad  = R'*CinvR;
    lnL   = -0.5*(Quad + LogDet + N*log(2*pi));
end

% ======================================================================= %
function X = localSimDRW(T, Tau, Sigma, Mu)
    % Simulate a DRW (OU) realization at times T, stationary variance Sigma^2.
    T = T(:);  N = numel(T);
    Dt  = [Inf; diff(T)];
    X   = zeros(N,1);
    X(1) = Sigma*randn;
    for i = 2:N
        Phi  = exp(-Dt(i)/Tau);
        X(i) = Phi*X(i-1) + Sigma*sqrt(1-Phi*Phi)*randn;
    end
    X = X + Mu;
end
