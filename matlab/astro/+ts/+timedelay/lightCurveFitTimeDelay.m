function [L, Info] = lightCurveFitTimeDelay(Time, Flux, FluxErr, Args)
% lightCurveFitTimeDelay - Fit a time delay + power-law PSD to an unresolved light curve (time domain).
%
% Description:
%   Fit the Springer & Ofek (2021) model of a combined (unresolved) light
%   curve made of two time-shifted copies of the same stochastic source
%   light curve, using the full Gaussian likelihood in the TIME DOMAIN:
%
%       F(t) = mu + alpha1*f(t) + alpha2*f(t+Tau) + noise,
%
%   where f(t) is a stationary Gaussian process with one-sided PSD Pf(f).
%   The covariance of F is exactly the covariance of a stationary process
%   whose PSD is
%
%       P(f) = A .* Shape(f) .* [1 + r^2 + 2*r*cos(2*pi*f*Tau)],
%
%   with r = alpha2/alpha1 and A = alpha1^2 * Pf(FRef). Hence
%
%       C(dt) = integral P(f).*cos(2*pi*f*dt) df ,   LowFreqCut<=f<=HighFreqCut.
%
%   Shape(f) is a single power law, (f/FRef).^(-Gamma1), or a continuous
%   broken power law normalized so that Shape(FRef)=1 for every trial break:
%
%       B(f) = (f/fb).^(-Gamma1), f<=fb ;  (f/fb).^(-Gamma2), f>fb ;
%       Shape(f) = B(f)./B(FRef).
%
%   The PSD normalization A and the constant mean flux mu are profiled (or
%   marginalized, see MeanMethod) analytically/cheaply at every grid point
%   of (cutoffs, break, slopes, Tau, r). The null hypothesis r=0 (a single,
%   non-delayed light curve) is also fitted, and the log-likelihood ratio is
%   returned.
%
%   IDENTIFIABILITY: the covariance depends on Tau only through
%   cos(2*pi*f*Tau), and r -> 1/r only rescales A. Therefore only |Tau| and
%   min(r,1/r) are measurable from the combined flux: Tau>0 and 0<r<=1.
%   The data cannot tell which image leads.
%
% Numerical method (efficiency):
%   1. The covariance integral is represented by a fixed Fourier basis
%      Phi=[cos(2*pi*t*f_k), sin(2*pi*t*f_k)], so that
%          K = diag(sig^2) + A * Phi*diag([w;w].*W(Tau,r))*Phi' ,
%      where w_k is the EXACT integral of Shape(f) over frequency bin k and
%      W = 1+r^2+2r cos(2*pi*f_k*Tau). The basis is built once.
%   2. The frequency grid is uniform with df ~ 1/(FreqOversample*Span)
%      (needed for the oscillatory integrand at long lags), and in addition
%      the first LowRefineBins bins above every trial LowFreqCut are split
%      into LowRefineFactor sub-bins. For red noise, almost all of the
%      quadrature error comes from these bins; refining them gives
%      ~0.3% covariance accuracy for Gamma=1.5-3.5 at FreqOversample=2.
%      All LowFreqCut, HighFreqCut and BreakFreq values are exact bin
%      edges, so cutoffs are not quantized by the grid.
%   3. After whitening by the noise, for each model we diagonalize the
%      smaller of the basis-space (R x R, using the precomputed Gram matrix
%      G=X'X) or the data-space (N x N) signal covariance. Given the
%      eigenvalues, A is found by a 1-D bounded search whose every step
%      costs O(rank), and the mean is solved analytically.
%   4. In data space K_signal(r) = (1+r^2)*S0 + 2r*S1(Tau), so S0 and
%      S1(Tau) are formed once per (shape,Tau) and reused for all r.
%   5. The r=0 model does not depend on Tau and is computed once per shape.
%   6. Optional parfor over (shape,Tau) jobs (Args.UseParallel).
%   7. Optional continuous refinement (Nelder-Mead) of (Tau, r, slopes,
%      break) around the best RefineNPeaks peaks of the Tau profile, so the
%      grids can be coarse.
%
% Input arguments:
%   Time       - Observation times [N x 1]. Arbitrary sampling is allowed.
%   Flux       - Combined flux measurements [N x 1].
%   FluxErr    - 1-sigma independent Gaussian errors. Scalar or [N x 1].
%   Args       - Optional key/value arguments:
%       .TimeDelay      - Vector of trial time delays (>0, same units as
%                         Time). Default: [] -> linspace(2*dt, Span/3, n),
%                         dt=median sampling interval, n<=200.
%       .FluxRatio      - Vector of trial r=alpha2/alpha1 in (0,1].
%                         Default: [0.05, 0.1:0.1:1]. (r=0 is always
%                         computed separately as the null hypothesis.)
%       .LowFreqCut     - Scalar/vector lower cutoff frequencies.
%                         Default: 1/(max(Time)-min(Time)).
%       .HighFreqCut    - Scalar/vector upper cutoff frequencies.
%                         Default: 0.5/median(diff(sort(Time))).
%       .BreakFreq      - NaN for a single power law, or scalar/vector break
%                         frequencies. Default: NaN.
%       .PowerLawInd    - Scalar/vector low-frequency slopes Gamma1.
%                         Default: 1.5:0.5:3.5 (coarse; see Refine).
%       .PowerLawInd2   - Scalar/vector high-frequency slopes Gamma2.
%                         Default: PowerLawInd. Ignored for a single PL.
%       .FRef           - Reference frequency at which A is defined.
%                         Default: geometric mean of global fitted band.
%       .NFreq          - Number of base uniform frequency bins (before
%                         low-frequency refinement). 0=automatic. Default: 0.
%       .FreqOversample - Automatic rule: NFreq>=Oversample*(fmax-fmin)*Span.
%                         Default: 2.
%       .MinNFreq       - Minimum automatic NFreq. Default: 64.
%       .LowRefineBins  - Number of base bins above each LowFreqCut that are
%                         refined. 0 disables. Default: 8.
%       .LowRefineFactor- Sub-bins per refined bin. Default: 8.
%       .MeanMethod     - 'reml' (marginalize a flat-prior constant mean;
%                         recommended for red noise), 'ml' (profile the
%                         mean), or 'fixed' (use Args.FixedMean).
%                         Default: 'reml'.
%       .FixedMean      - Mean flux used when MeanMethod='fixed'. Default: 0.
%       .ExtraNoise     - Extra white noise (1-sigma) added in quadrature to
%                         FluxErr (e.g., microlensing/calibration jitter).
%                         Default: 0.
%       .Space          - 'auto' | 'basis' | 'data'. Force the eigen-space.
%                         Default: 'auto' (cheaper one).
%       .Refine         - Continuous refinement of the best peaks. It is a
%                         local polish: Tau stays inside the TimeDelay grid,
%                         slopes within 0.5 of the slope grids, and the break
%                         within x2 of the BreakFreq grid. Default: true.
%       .RefineShape    - If true, refinement also varies the slopes (and
%                         break); if false only (Tau,r) are refined and the
%                         PSD shape is kept at its best grid value (use this
%                         to keep a fixed power-law index). Default: true.
%       .RefineNPeaks   - Number of Tau-profile peaks to refine. Default: 3.
%       .RefineMaxIter  - Max Nelder-Mead iterations per peak. Default: 300.
%       .NormRangeFact  - Search A over Aguess*[1/F,F]. Default: 1e8.
%       .NormTol        - fminbnd tolerance in log(A). Default: 1e-4.
%       .EigTol         - Relative eigenvalue cutoff. Default: 1e-12.
%       .UseParallel    - Use parfor over (shape,Tau) jobs. Default: false.
%       .Verbose        - Print progress. Default: false.
%
% Output arguments:
%   L          - Log-likelihood grid (A and mean profiled/marginalized):
%                L(iLow,iHigh,iBreak,iGamma1,iGamma2,iTau,iRatio)
%                For a single PL the iBreak and iGamma2 dimensions are 1.
%   Info       - Structure with grids, the null-hypothesis grid (Info.L0),
%                fitted PSD normalizations and means, Tau and r profiles,
%                best grid and refined solutions, log-likelihood ratio, the
%                frequency grid and diagnostics. Main fields:
%       .Best          - Best H1 (time-delay) solution (grid or refined).
%       .BestH0        - Best r=0 solution (grid or refined).
%       .LogLR         - Best.LogLikelihood - BestH0.LogLikelihood.
%       .ProfileTau    - max of L over all parameters except Tau.
%       .ProfileRatio  - max of L over all parameters except r.
%       .Refine        - Struct array with the refined peaks.
%
% Notes:
%   1. L values are directly comparable between models (same data, same
%      mean treatment). For MeanMethod='reml' the likelihood is the
%      restricted likelihood (the constant mean is integrated out).
%   2. The r=0 hypothesis does not constrain Tau, so 2*LogLR does NOT follow
%      a chi^2 distribution (Davies problem / look-elsewhere effect).
%      Calibrate the false-alarm probability with simulations of r=0 light
%      curves with the best-fit H0 PSD.
%   3. Microlensing is not modeled; it can partly be absorbed by
%      ExtraNoise and by the PSD shape.
%   4. PSD convention: one-sided PSD in cycles per unit Time. The
%      Springer & Ofek angular-frequency convention sigma^2(omega)=|omega|^-g
%      corresponds to P(f) = 2*(2*pi)^(-g) * f^(-g).
%   5. Cost per model: O(min(R,N)^3), R=2*(number of active frequency
%      bins). Keep HighFreqCut well below the sampling Nyquist rate when N
%      is large, since R grows linearly with HighFreqCut*Span.
%   6. Resolution in Tau: likelihood features in Tau have widths down to
%      ~1/HighFreqCut, so the TimeDelay grid step should be comparable to
%      the typical sampling interval; Refine polishes the best peaks.
%
% Example:
%   [L,Info] = lightCurveFitTimeDelay(T,F,E, ...
%       'TimeDelay',5:2:300, ...
%       'FluxRatio',[0.1:0.1:1], ...
%       'PowerLawInd',1.5:0.5:3.5, ...
%       'HighFreqCut',0.1);
%   plot(Info.TimeDelay, Info.ProfileTau-max(Info.ProfileTau));
%   Info.Best, Info.LogLR
%
% Reference:
%   Springer, O. M., & Ofek, E. O. 2021, MNRAS, 506, 864.
%
% Author: Claude (Anthropic); based on lightCurveFitPowerLawPowerSpec.m

arguments
    Time (:,1) double
    Flux (:,1) double
    FluxErr double
    Args.TimeDelay double = []
    Args.FluxRatio double = [0.05, 0.1:0.1:1]
    Args.LowFreqCut double = NaN
    Args.HighFreqCut double = NaN
    Args.BreakFreq double = NaN
    Args.PowerLawInd double = 1.5:0.5:3.5
    Args.PowerLawInd2 double = []
    Args.FRef (1,1) double = NaN
    Args.NFreq (1,1) double = 0
    Args.FreqOversample (1,1) double {mustBePositive} = 2
    Args.MinNFreq (1,1) double {mustBeInteger,mustBePositive} = 64
    Args.LowRefineBins (1,1) double {mustBeInteger,mustBeNonnegative} = 8
    Args.LowRefineFactor (1,1) double {mustBeInteger,mustBePositive} = 8
    Args.MeanMethod char {mustBeMember(Args.MeanMethod,{'reml','ml','fixed'})} = 'reml'
    Args.FixedMean (1,1) double = 0
    Args.ExtraNoise (1,1) double {mustBeNonnegative} = 0
    Args.Space char {mustBeMember(Args.Space,{'auto','basis','data'})} = 'auto'
    Args.Refine (1,1) logical = true
    Args.RefineShape (1,1) logical = true
    Args.RefineNPeaks (1,1) double {mustBeInteger,mustBePositive} = 3
    Args.RefineMaxIter (1,1) double {mustBeInteger,mustBePositive} = 300
    Args.NormRangeFact (1,1) double {mustBePositive} = 1e8
    Args.NormTol (1,1) double {mustBePositive} = 1e-4
    Args.EigTol (1,1) double {mustBePositive} = 1e-12
    Args.UseParallel (1,1) logical = false
    Args.Verbose (1,1) logical = false
end

%--------------------------
% Input preparation
%--------------------------
Ninput = numel(Time);
if isscalar(FluxErr)
    FluxErr = repmat(FluxErr,Ninput,1);
else
    FluxErr = FluxErr(:);
end
Flux = Flux(:);

if numel(Flux)~=Ninput || numel(FluxErr)~=Ninput
    error('Time, Flux, and FluxErr must have compatible lengths.');
end

Good = isfinite(Time) & isfinite(Flux) & isfinite(FluxErr) & FluxErr>0;
Time    = Time(Good);
Flux    = Flux(Good);
FluxErr = FluxErr(Good);

[Time,SI] = sort(Time);
Flux      = Flux(SI);
FluxErr   = FluxErr(SI);
N         = numel(Time);

if N<5
    error('At least five valid measurements are required.');
end

Span = Time(end)-Time(1);
if ~(isfinite(Span) && Span>0)
    error('Time must span a non-zero interval.');
end
Dt = diff(Time);
Dt = Dt(Dt>0 & isfinite(Dt));
if isempty(Dt)
    error('Time must contain at least two distinct epochs.');
end
MedDt = median(Dt);

% Extra white noise in quadrature
FluxErr = sqrt(FluxErr.^2 + Args.ExtraNoise.^2);

MeanMethod = lower(Args.MeanMethod);
if strcmp(MeanMethod,'fixed')
    Flux = Flux - Args.FixedMean;
end

% --- frequency cutoffs
LowVec = Args.LowFreqCut(:).';
if any(isnan(LowVec))
    if numel(LowVec)>1
        error('LowFreqCut cannot mix NaN and finite values.');
    end
    LowVec = 1./Span;
end
HighVec = Args.HighFreqCut(:).';
if any(isnan(HighVec))
    if numel(HighVec)>1
        error('HighFreqCut cannot mix NaN and finite values.');
    end
    HighVec = 0.5./MedDt;
end
if any(~isfinite(LowVec) | LowVec<=0)
    error('LowFreqCut values must be finite and >0.');
end
if any(~isfinite(HighVec) | HighVec<=0)
    error('HighFreqCut values must be finite and >0.');
end

% --- break frequency and slopes
BreakVec = Args.BreakFreq(:).';
IsSingle = all(isnan(BreakVec));
if ~IsSingle && any(~isfinite(BreakVec) | BreakVec<=0)
    error('BreakFreq must be NaN, or contain only finite values >0.');
end
Gamma1Vec = Args.PowerLawInd(:).';
if IsSingle || isempty(Args.PowerLawInd2)
    Gamma2Vec = Gamma1Vec;
else
    Gamma2Vec = Args.PowerLawInd2(:).';
end
if IsSingle
    BreakVec  = NaN;
    Gamma2Vec = NaN;
end
if isempty(Gamma1Vec) || any(~isfinite(Gamma1Vec)) || (~IsSingle && any(~isfinite(Gamma2Vec)))
    error('Power-law indices must be finite and non-empty.');
end

% --- time delays and flux ratios
if isempty(Args.TimeDelay)
    TauMin = 2.*MedDt;
    TauMax = Span./3;
    NTau   = min(200, max(10, ceil((TauMax-TauMin)./MedDt)));
    TauVec = linspace(TauMin,TauMax,NTau);
else
    TauVec = unique(Args.TimeDelay(:).');
end
if any(~isfinite(TauVec) | TauVec<=0)
    error('TimeDelay values must be finite and >0 (only |Tau| is measurable).');
end
if any(TauVec>=Span)
    error('TimeDelay values must be smaller than the time span of the data.');
end
if any(TauVec>Span./2)
    warning('lightCurveFitTimeDelay:LongDelay', ...
        'Some TimeDelay values exceed Span/2; such delays are weakly constrained.');
end

RVec = unique(Args.FluxRatio(:).');
RVec = RVec(RVec~=0);
if isempty(RVec) || any(~isfinite(RVec) | RVec<0 | RVec>1)
    error('FluxRatio values must be in (0,1] (r and 1/r are degenerate).');
end

if Args.NormRangeFact<=1
    error('NormRangeFact must be larger than 1.');
end
if Args.NFreq<0 || Args.NFreq~=round(Args.NFreq)
    error('NFreq must be a non-negative integer; use 0 for automatic.');
end

GlobalLow  = min(LowVec);
GlobalHigh = max(HighVec);
if GlobalLow>=GlobalHigh
    error('The global minimum LowFreqCut must be smaller than the global maximum HighFreqCut.');
end
if isnan(Args.FRef)
    FRef = sqrt(GlobalLow.*GlobalHigh);
else
    FRef = Args.FRef;
    if ~(isfinite(FRef) && FRef>0)
        error('FRef must be finite and >0.');
    end
end

%--------------------------
% Frequency grid: uniform base + refinement above every LowFreqCut
%--------------------------
if Args.NFreq==0
    NF0 = max(Args.MinNFreq, ceil(Args.FreqOversample.*(GlobalHigh-GlobalLow).*Span));
else
    NF0 = Args.NFreq;
    if NF0 < ceil((GlobalHigh-GlobalLow).*Span)
        warning('lightCurveFitTimeDelay:UnderResolvedFrequencyGrid', ...
            'NFreq=%d gives df>1/TimeSpan. The covariance may be inaccurate.',NF0);
    end
end
dF0   = (GlobalHigh-GlobalLow)./NF0;
Edges = linspace(GlobalLow,GlobalHigh,NF0+1);
if Args.LowRefineBins>0 && Args.LowRefineFactor>1
    for Il=1:numel(LowVec)
        Top   = min(LowVec(Il) + Args.LowRefineBins.*dF0, GlobalHigh);
        Edges = [Edges, linspace(LowVec(Il),Top,Args.LowRefineBins.*Args.LowRefineFactor+1)]; %#ok<AGROW>
    end
end
Edges = [Edges, LowVec, HighVec];
if ~IsSingle
    Edges = [Edges, BreakVec(BreakVec>GlobalLow & BreakVec<GlobalHigh)];
end
Edges = sort(Edges(Edges>=GlobalLow & Edges<=GlobalHigh));
Edges = Edges([true, diff(Edges) > 1e-9.*dF0]);
EdgeLo = Edges(1:end-1);
EdgeHi = Edges(2:end);
Freq   = 0.5.*(EdgeLo+EdgeHi);
NF     = numel(Freq);

%--------------------------
% Fixed time-domain basis, whitened by the noise
%--------------------------
T0    = mean(Time);
Phase = 2.*pi.*((Time-T0)*Freq);
InvErr = 1./FluxErr;
X = [cos(Phase), sin(Phase)].*InvErr;       % N x 2NF
clear Phase

yw = Flux.*InvErr;
uw = InvErr;

Ctx = struct;
Ctx.N        = N;
Ctx.NF       = NF;
Ctx.Freq     = Freq(:);
Ctx.X        = X;
Ctx.yw       = yw;
Ctx.uw       = uw;
Ctx.y2       = yw.'*yw;
Ctx.yu       = yw.'*uw;
Ctx.u2       = uw.'*uw;
Ctx.hY       = X.'*yw;
Ctx.hU       = X.'*uw;
Ctx.LogDetNoise = 2.*sum(log(FluxErr));
Ctx.Log2Pi   = log(2.*pi);
Ctx.MeanMethod = MeanMethod;
VarData  = var(Flux,1);
VarNoise = mean(FluxErr.^2);
Ctx.VarIntrinsicGuess = max(VarData-VarNoise, 0.01.*max(VarData,realmin));
Ctx.NormRangeFact = Args.NormRangeFact;
Ctx.NormTol  = Args.NormTol;
Ctx.EigTol   = Args.EigTol;

%--------------------------
% PSD-shape list (exact bin weights)
%--------------------------
nL  = numel(LowVec);
nH  = numel(HighVec);
nB  = numel(BreakVec);
nG1 = numel(Gamma1Vec);
nG2 = numel(Gamma2Vec);
nT  = numel(TauVec);
nR  = numel(RVec);
ShapeSize = [nL,nH,nB,nG1,nG2];
NShape    = prod(ShapeSize);

ShapeW   = cell(NShape,1);    % active bin weights  [Nact x 1]
ShapeIF  = cell(NShape,1);    % active bin indices  [Nact x 1]
ShapeOK  = false(NShape,1);
ShapeR   = zeros(NShape,1);
for Is=1:NShape
    [iL,iH,iB,iG1,iG2] = ind2sub(ShapeSize,Is);
    FLow  = LowVec(iL);
    FHigh = HighVec(iH);
    if FLow>=FHigh
        continue;
    end
    if IsSingle
        FB = NaN;  G2 = NaN;
    else
        FB = BreakVec(iB);  G2 = Gamma2Vec(iG2);
        if FB<=FLow || FB>=FHigh
            continue;
        end
    end
    [W,IFr] = localShapeWeights(EdgeLo,EdgeHi,FLow,FHigh,FB,Gamma1Vec(iG1),G2,FRef);
    if numel(IFr)<2 || ~all(isfinite(W)) || sum(W)<=0
        continue;
    end
    ShapeW{Is}  = W;
    ShapeIF{Is} = IFr;
    ShapeOK(Is) = true;
    ShapeR(Is)  = 2.*numel(IFr);
end
if ~any(ShapeOK)
    error('No valid PSD-shape model (check cutoffs and break frequencies).');
end

% Choose eigen-space per shape. Basis space costs ~R^3 per model; data space
% costs ~N^3 per model (+ 2 N^2 R per Tau, shared by all r).
switch lower(Args.Space)
    case 'basis'
        ShapeUseBasis = ShapeOK;
    case 'data'
        ShapeUseBasis = false(NShape,1);
    otherwise
        ShapeUseBasis = ShapeOK & (ShapeR <= N);
end
Ctx.HaveG = any(ShapeUseBasis) || (Args.Refine && strcmpi(Args.Space,'auto') && min(ShapeR(ShapeOK))<=N) ...
            || strcmpi(Args.Space,'basis');
if Ctx.HaveG
    Ctx.G = X.'*X;                              % 2NF x 2NF Gram matrix, computed once
    Ctx.G = 0.5.*(Ctx.G+Ctx.G.');
else
    Ctx.G = [];
end
Ctx.Space = lower(Args.Space);

%--------------------------
% Null hypothesis (r=0): once per shape
%--------------------------
L0v   = -Inf(NShape,1);
A0v   = NaN(NShape,1);
Mu0v  = NaN(NShape,1);
for Is=find(ShapeOK).'
    Sh = localShapeStruct(ShapeW{Is},ShapeIF{Is},ShapeUseBasis(Is));
    [Lr,Ar,Mr] = localJob(Sh,0,0,Ctx);
    L0v(Is) = Lr;  A0v(Is) = Ar;  Mu0v(Is) = Mr;
end

%--------------------------
% H1 grid: jobs over (shape,Tau), inner loop over r
%--------------------------
ShapeList = find(ShapeOK);
NJob      = numel(ShapeList).*nT;
JobL  = -Inf(NJob,nR);
JobA  = NaN(NJob,nR);
JobMu = NaN(NJob,nR);
JobRk = zeros(NJob,nR);

if Args.UseParallel
    NW = Inf;
else
    NW = 0;
end
if Args.Verbose
    fprintf('lightCurveFitTimeDelay: N=%d, NFreq=%d, shapes=%d, Tau=%d, r=%d -> %d models\n', ...
        N,NF,numel(ShapeList),nT,nR,NJob.*nR);
    Tic = tic;
end

nSL = numel(ShapeList);
parfor (Ij=1:NJob, NW)
    Iss = mod(Ij-1,nSL)+1;           % shape index (fast)
    It  = floor((Ij-1)./nSL)+1;      % tau index   (slow)
    Is  = ShapeList(Iss);
    Sh  = localShapeStruct(ShapeW{Is},ShapeIF{Is},ShapeUseBasis(Is)); %#ok<PFBNS>
    [Lr,Ar,Mr,Rk] = localJob(Sh,TauVec(It),RVec,Ctx);
    JobL(Ij,:)  = Lr;
    JobA(Ij,:)  = Ar;
    JobMu(Ij,:) = Mr;
    JobRk(Ij,:) = Rk;
end
if Args.Verbose
    fprintf('  grid done in %.1f s\n',toc(Tic));
end

% Scatter jobs into the full grid [shape..., tau, r]
FullSize = [ShapeSize, nT, nR];
Lfull  = -Inf(NShape,nT,nR);
Afull  = NaN(NShape,nT,nR);
Mufull = NaN(NShape,nT,nR);
Rkfull = zeros(NShape,nT,nR);
for Ij=1:NJob
    Iss = mod(Ij-1,nSL)+1;
    It  = floor((Ij-1)./nSL)+1;
    Is  = ShapeList(Iss);
    Lfull(Is,It,:)  = reshape(JobL(Ij,:),1,1,nR);
    Afull(Is,It,:)  = reshape(JobA(Ij,:),1,1,nR);
    Mufull(Is,It,:) = reshape(JobMu(Ij,:),1,1,nR);
    Rkfull(Is,It,:) = reshape(JobRk(Ij,:),1,1,nR);
end
L = reshape(Lfull,FullSize);

%--------------------------
% Profiles and grid best solutions
%--------------------------
ProfileTau   = max(reshape(permute(Lfull,[2 1 3]),nT,[]),[],2).';
ProfileRatio = max(reshape(permute(Lfull,[3 1 2]),nR,[]),[],2).';

[BestL,BestLin] = max(Lfull(:));
[IsB,ItB,IrB]   = ind2sub([NShape,nT,nR],BestLin);
Best = localParamStruct(IsB,ShapeSize,LowVec,HighVec,BreakVec,Gamma1Vec,Gamma2Vec,IsSingle);
Best.TimeDelay     = TauVec(ItB);
Best.FluxRatio     = RVec(IrB);
Best.LogLikelihood = BestL;
Best.PowerSpecNorm = Afull(BestLin);
Best.MeanFlux      = Mufull(BestLin);
Best.EffectiveRank = Rkfull(BestLin);
Best.Source        = 'grid';

[BestL0,IsB0] = max(L0v);
BestH0 = localParamStruct(IsB0,ShapeSize,LowVec,HighVec,BreakVec,Gamma1Vec,Gamma2Vec,IsSingle);
BestH0.TimeDelay     = NaN;
BestH0.FluxRatio     = 0;
BestH0.LogLikelihood = BestL0;
BestH0.PowerSpecNorm = A0v(IsB0);
BestH0.MeanFlux      = Mu0v(IsB0);
BestH0.Source        = 'grid';

%--------------------------
% Continuous refinement (Nelder-Mead) of the best Tau peaks and of H0
%--------------------------
RefineOut = struct([]);
if Args.Refine && isfinite(BestL)
    % Refinement is a local polish: slopes may move <=0.5 beyond the grid,
    % the break <=x2 beyond the BreakFreq grid, Tau stays inside its grid.
    Geo = struct('EdgeLo',EdgeLo,'EdgeHi',EdgeHi,'FRef',FRef,'IsSingle',IsSingle, ...
                 'TauLim',[min(TauVec), max(TauVec)], ...
                 'G1Lim',[max(0.01,min(Gamma1Vec)-0.5), max(Gamma1Vec)+0.5], ...
                 'G2Lim',[max(0.01,min(Gamma2Vec)-0.5), max(Gamma2Vec)+0.5], ...
                 'FBLim',[min(BreakVec)./2, max(BreakVec).*2], ...
                 'RefineShape',Args.RefineShape);
    if nT>1
        dTau = median(diff(TauVec));
    else
        dTau = 0.05.*TauVec(1);
    end

    % Peaks of the Tau profile
    Pk = localFindPeaks(ProfileTau);
    Pk = Pk(1:min(numel(Pk),Args.RefineNPeaks));

    RefineOut = repmat(localEmptyRefine(),numel(Pk),1);
    for Ip=1:numel(Pk)
        It = Pk(Ip);
        Sub = squeeze(Lfull(:,It,:));
        Sub = reshape(Sub,NShape,nR);
        [~,Lin] = max(Sub(:));
        [IsP,IrP] = ind2sub([NShape,nR],Lin);
        P0 = localParamStruct(IsP,ShapeSize,LowVec,HighVec,BreakVec,Gamma1Vec,Gamma2Vec,IsSingle);
        rStart = min(max(RVec(IrP),0.02),0.98);
        % parameters: [Tau, logit(r), Gamma1, (Gamma2, log fb)]
        p0 = [TauVec(It); log(rStart./(1-rStart))];
        sc = [10.*dTau; 10];
        if Args.RefineShape
            p0 = [p0; P0.PowerLawInd1]; %#ok<AGROW>
            sc = [sc; 2];               %#ok<AGROW>
        end
        if Args.RefineShape && ~IsSingle
            p0 = [p0; P0.PowerLawInd2; log(P0.BreakFreq)]; %#ok<AGROW>
            sc = [sc; 2; 2];                                %#ok<AGROW>
        end
        Obj = @(th) localRefineNLL(p0+(th-1).*sc, P0, Geo, Ctx, true);
        Opt = optimset('Display','off','TolX',1e-3,'TolFun',1e-3, ...
                       'MaxIter',Args.RefineMaxIter,'MaxFunEvals',2.*Args.RefineMaxIter);
        [Th,Fval,ExitFlag,Output] = fminsearch(Obj,ones(size(p0)),Opt);
        p = p0+(Th-1).*sc;
        [~,Ar,Mr,Pp] = localRefineNLL(p,P0,Geo,Ctx,true);

        R1 = P0;
        R1.TimeDelay     = Pp.Tau;
        R1.FluxRatio     = Pp.r;
        R1.PowerLawInd1  = Pp.G1;
        R1.PowerLawInd2  = Pp.G2;
        R1.BreakFreq     = Pp.FB;
        R1.LogLikelihood = -Fval;
        R1.PowerSpecNorm = Ar;
        R1.MeanFlux      = Mr;
        R1.StartTimeDelay = TauVec(It);
        R1.StartLogLikelihood = ProfileTau(It);
        R1.ExitFlag      = ExitFlag;
        R1.NFunEval      = Output.funcCount;
        RefineOut(Ip)    = localFillRefine(R1);

        if -Fval > Best.LogLikelihood
            Best = localBestFromRefine(R1);
        end
    end

    % Refine H0 slopes/break (so that the LR is not biased by the grid)
    P0 = BestH0;
    if Args.RefineShape
        p0 = P0.PowerLawInd1;  sc = 2;
        if ~IsSingle
            p0 = [p0; P0.PowerLawInd2; log(P0.BreakFreq)];
            sc = [sc; 2; 2];
        end
        Obj = @(th) localRefineNLL(p0+(th-1).*sc, P0, Geo, Ctx, false);
        Opt = optimset('Display','off','TolX',1e-3,'TolFun',1e-3, ...
                       'MaxIter',Args.RefineMaxIter,'MaxFunEvals',2.*Args.RefineMaxIter);
        [Th,Fval] = fminsearch(Obj,ones(size(p0)),Opt);
        if -Fval > BestH0.LogLikelihood
            p = p0+(Th-1).*sc;
            [~,Ar,Mr,Pp] = localRefineNLL(p,P0,Geo,Ctx,false);
            BestH0.PowerLawInd1  = Pp.G1;
            BestH0.PowerLawInd2  = Pp.G2;
            BestH0.BreakFreq     = Pp.FB;
            BestH0.LogLikelihood = -Fval;
            BestH0.PowerSpecNorm = Ar;
            BestH0.MeanFlux      = Mr;
            BestH0.Source        = 'refine';
        end
    end
    if Args.Verbose
        fprintf('  refinement done in %.1f s\n',toc(Tic));
    end
end

%--------------------------
% Output
%--------------------------
Info = struct;
Info.TimeDelay      = TauVec;
Info.FluxRatio      = RVec;
Info.LowFreqCut     = LowVec;
Info.HighFreqCut    = HighVec;
Info.BreakFreq      = BreakVec;
Info.PowerLawInd1   = Gamma1Vec;
Info.PowerLawInd2   = Gamma2Vec;
Info.IsSinglePowerLaw = IsSingle;
Info.GridOrder      = {'LowFreqCut','HighFreqCut','BreakFreq','PowerLawInd1', ...
                       'PowerLawInd2','TimeDelay','FluxRatio'};
Info.GridSize       = FullSize;
Info.L0             = reshape(L0v,[ShapeSize,1]);
Info.PowerSpecNorm  = reshape(Afull,FullSize);
Info.PowerSpecNorm0 = reshape(A0v,[ShapeSize,1]);
Info.MeanFlux       = reshape(Mufull,FullSize);
Info.MeanFlux0      = reshape(Mu0v,[ShapeSize,1]);
Info.EffectiveRank  = reshape(Rkfull,FullSize);
Info.ProfileTau     = ProfileTau;
Info.ProfileRatio   = ProfileRatio;
Info.Best           = Best;
Info.BestH0         = BestH0;
Info.LogLR          = Best.LogLikelihood - BestH0.LogLikelihood;
Info.Refine         = RefineOut;
Info.FRef           = FRef;
Info.NFreqBase      = NF0;
Info.NFreq          = NF;
Info.Frequency      = Freq;
Info.FrequencyEdge  = Edges;
Info.MeanMethod     = MeanMethod;
Info.ExtraNoise     = Args.ExtraNoise;
Info.NData          = N;
Info.NInput         = Ninput;
Info.NRemoved       = Ninput-N;
Info.UsedBasisSpace = reshape(ShapeUseBasis,[ShapeSize,1]);
Info.Method         = ['Full time-domain Gaussian likelihood; F=mu+a1 f(t)+a2 f(t+Tau); ', ...
                       'uniform+low-refined Fourier covariance quadrature with exact bin weights'];
Info.PSDConvention  = 'One-sided PSD in cycles per unit Time: C(dt)=int P(f) cos(2*pi*f*dt) df';
Info.PowerSpecNormDefinition = 'alpha1^2 * P_source(FRef)';

end


%==========================================================================
% Local functions
%==========================================================================

function Sh = localShapeStruct(W,IFr,UseBasis)
% Pack per-shape quantities.
Sh = struct('W',W,'IFr',IFr,'UseBasis',UseBasis);
end


function [Lrow,Arow,Murow,Rkrow] = localJob(Sh,Tau,RVec,Ctx)
% Log-likelihood for one PSD shape and one Tau over all flux ratios RVec.
nR    = numel(RVec);
Lrow  = -Inf(1,nR);
Arow  = NaN(1,nR);
Murow = NaN(1,nR);
Rkrow = zeros(1,nR);

IFr = Sh.IFr(:);
W   = Sh.W(:);
Col = [IFr; IFr+Ctx.NF];
W2  = [W; W];
Cs2 = cos(2.*pi.*Ctx.Freq(IFr).*Tau);
Cs2 = [Cs2; Cs2];
SumW  = sum(W);
SumWC = sum(W.*Cs2(1:numel(W)));

if Sh.UseBasis
    Gc  = Ctx.G(Col,Col);
    hYc = Ctx.hY(Col);
    hUc = Ctx.hU(Col);
    for Ir=1:nR
        r = RVec(Ir);
        D = W2.*(1 + r.^2 + 2.*r.*Cs2);
        SumD = (1+r.^2).*SumW + 2.*r.*SumWC;
        [Lam,zY,zU] = localEigBasis(D,Gc,hYc,hUc,Ctx.EigTol);
        [NLL,A,Mu] = localProfileA(Lam,zY,zU,SumD,Ctx);
        Lrow(Ir) = -NLL;  Arow(Ir) = A;  Murow(Ir) = Mu;  Rkrow(Ir) = numel(Lam);
    end
else
    Xc = Ctx.X(:,Col);
    S0 = (Xc.*W2.')*Xc.';
    if any(RVec~=0)
        S1 = (Xc.*(W2.*Cs2).')*Xc.';
    else
        S1 = zeros(size(S0));
    end
    for Ir=1:nR
        r = RVec(Ir);
        Cd = (1+r.^2).*S0 + (2.*r).*S1;
        SumD = (1+r.^2).*SumW + 2.*r.*SumWC;
        [Lam,zY,zU] = localEigData(Cd,Ctx.yw,Ctx.uw,Ctx.EigTol);
        [NLL,A,Mu] = localProfileA(Lam,zY,zU,SumD,Ctx);
        Lrow(Ir) = -NLL;  Arow(Ir) = A;  Murow(Ir) = Mu;  Rkrow(Ir) = numel(Lam);
    end
end
end


function [Lam,zY,zU] = localEigBasis(D,Gc,hYc,hUc,EigTol)
% Eigen-decomposition in basis space: D^1/2 G D^1/2.
D   = max(D,0);
Nz  = D>0;
Ds  = sqrt(D(Nz));
Cs  = (Ds.*Gc(Nz,Nz)).*Ds.';
Cs  = 0.5.*(Cs+Cs.');
[V,Lam] = eig(Cs,'vector');
Lam = real(Lam);
[Lam,V] = localKeep(Lam,V,EigTol);
if isempty(Lam)
    zY = [];  zU = [];
    return;
end
SqL = sqrt(Lam);
zY  = (V.'*(Ds.*hYc(Nz)))./SqL;
zU  = (V.'*(Ds.*hUc(Nz)))./SqL;
end


function [Lam,zY,zU] = localEigData(Cd,yw,uw,EigTol)
% Eigen-decomposition in data space.
Cd = 0.5.*(Cd+Cd.');
[U,Lam] = eig(Cd,'vector');
Lam = real(Lam);
[Lam,U] = localKeep(Lam,U,EigTol);
if isempty(Lam)
    zY = [];  zU = [];
    return;
end
zY = U.'*yw;
zU = U.'*uw;
end


function [Lam,V] = localKeep(Lam,V,EigTol)
MaxLam = max(Lam);
if isempty(Lam) || ~(isfinite(MaxLam) && MaxLam>0)
    Lam = [];  V = [];
    return;
end
Keep = Lam > EigTol.*MaxLam;
Lam  = Lam(Keep);
V    = V(:,Keep);
end


function [NLL,A,Mu] = localProfileA(Lam,zY,zU,SumD,Ctx)
% Profile the PSD normalization A (1-D bounded search in log A, plus A=0).
if isempty(Lam) || ~(isfinite(SumD) && SumD>0)
    [NLL,Mu] = localNLL(0,[],[],[],Ctx);
    A = 0;
    return;
end
Aguess = max(Ctx.VarIntrinsicGuess./SumD, realmin('double'));
Amin   = max(Aguess./Ctx.NormRangeFact, realmin('double'));
Amax   = Aguess.*Ctx.NormRangeFact;
if ~(isfinite(Amax) && Amax>Amin)
    Amax = realmax('double').^(1/8);
end
Obj = @(LogA) localNLL(exp(LogA),Lam,zY,zU,Ctx);
Opt = optimset('Display','off','TolX',Ctx.NormTol);
[LogA,NLL] = fminbnd(Obj,log(Amin),log(Amax),Opt);
A = exp(LogA);
[NLL0,Mu0] = localNLL(0,Lam,zY,zU,Ctx);
if NLL0 < NLL
    NLL = NLL0;  A = 0;  Mu = Mu0;
else
    [~,Mu] = localNLL(A,Lam,zY,zU,Ctx);
end
end


function [NLL,Mu] = localNLL(A,Lam,zY,zU,Ctx)
% Negative log (restricted) likelihood for PSD amplitude A, given the
% eigen-decomposition of the whitened signal covariance.
if A<0 || ~isfinite(A)
    NLL = Inf;  Mu = NaN;
    return;
end
if isempty(Lam)
    AL = 0;  c = 0;  zY = 0;  zU = 0;
else
    AL = A.*Lam;
    c  = AL./(1+AL);
end
Qyy = Ctx.y2 - sum(c.*(zY.^2));
switch Ctx.MeanMethod
    case 'fixed'
        Mu   = 0;
        Quad = Qyy;
        LogQuu = 0;
        NEff = Ctx.N;
    otherwise
        Qyu = Ctx.yu - sum(c.*zY.*zU);
        Quu = Ctx.u2 - sum(c.*(zU.^2));
        if ~(isfinite(Quu) && Quu>0)
            NLL = Inf;  Mu = NaN;
            return;
        end
        Mu   = Qyu./Quu;
        Quad = Qyy - (Qyu.^2)./Quu;
        if strcmp(Ctx.MeanMethod,'reml')
            LogQuu = log(Quu);
            NEff   = Ctx.N-1;
        else
            LogQuu = 0;
            NEff   = Ctx.N;
        end
end
if Quad<0 && Quad>-1e-10.*max(Ctx.y2,1)
    Quad = 0;
end
if ~(isfinite(Quad) && Quad>=0)
    NLL = Inf;
    return;
end
LogDet = Ctx.LogDetNoise + sum(log1p(AL));
NLL = 0.5.*(LogDet + Quad + LogQuu + NEff.*Ctx.Log2Pi);
end


function [W,IFr] = localShapeWeights(EdgeLo,EdgeHi,FLow,FHigh,FB,G1,G2,FRef)
% Exact integral of Shape(f) over every frequency bin, clipped to
% [FLow,FHigh]. Shape(FRef)=1. Works for any FB (also inside a bin).
a = max(EdgeLo,FLow);
b = min(EdgeHi,FHigh);
IFr = find(b>a);
a = a(IFr);  b = b(IFr);
if isnan(FB)
    W = localPLInt(a,b,FRef,G1);
else
    if FRef<=FB
        RefShape = (FRef./FB).^(-G1);
    else
        RefShape = (FRef./FB).^(-G2);
    end
    aL = a;              bL = min(b,FB);
    aH = max(a,FB);      bH = b;
    W  = zeros(size(a));
    IL = bL>aL;
    IH = bH>aH;
    W(IL) = W(IL) + localPLInt(aL(IL),bL(IL),FB,G1);
    W(IH) = W(IH) + localPLInt(aH(IH),bH(IH),FB,G2);
    W = W./RefShape;
end
W   = W(:);
IFr = IFr(:);
end


function I = localPLInt(a,b,f0,g)
% integral_a^b (f/f0).^(-g) df
if abs(g-1) < 1e-8
    I = f0.*log(b./a);
else
    I = f0./(1-g).*((b./f0).^(1-g) - (a./f0).^(1-g));
end
end


function [NLL,A,Mu,Pp] = localRefineNLL(p,P0,Geo,Ctx,WithDelay)
% Objective for continuous refinement. P0 carries the fixed cutoffs.
NLL = Inf;  A = NaN;  Mu = NaN;
k = 1;
if WithDelay
    Pp.Tau = p(1);
    Pp.r   = 1./(1+exp(-p(2)));
    k = 3;
else
    Pp.Tau = NaN;
    Pp.r   = 0;
end
if Geo.RefineShape
    Pp.G1 = p(k);
    if Geo.IsSingle
        Pp.G2 = NaN;
        Pp.FB = NaN;
    else
        Pp.G2 = p(k+1);
        Pp.FB = exp(p(k+2));
    end
else
    Pp.G1 = P0.PowerLawInd1;
    Pp.G2 = P0.PowerLawInd2;
    Pp.FB = P0.BreakFreq;
end
% bounds
if WithDelay && (Pp.Tau<Geo.TauLim(1) || Pp.Tau>Geo.TauLim(2) || Pp.Tau<=0)
    return;
end
if Pp.G1<Geo.G1Lim(1) || Pp.G1>Geo.G1Lim(2) || (~Geo.IsSingle && ( ...
        Pp.G2<Geo.G2Lim(1) || Pp.G2>Geo.G2Lim(2) || ...
        Pp.FB<Geo.FBLim(1) || Pp.FB>Geo.FBLim(2) || ...
        Pp.FB<=P0.LowFreqCut || Pp.FB>=P0.HighFreqCut))
    return;
end
[W,IFr] = localShapeWeights(Geo.EdgeLo,Geo.EdgeHi,P0.LowFreqCut,P0.HighFreqCut, ...
                            Pp.FB,Pp.G1,Pp.G2,Geo.FRef);
if numel(IFr)<2 || ~all(isfinite(W)) || sum(W)<=0
    return;
end
R = 2.*numel(IFr);
switch Ctx.Space
    case 'basis'
        UseBasis = true;
    case 'data'
        UseBasis = false;
    otherwise
        UseBasis = Ctx.HaveG && R<=Ctx.N;
end
if UseBasis && ~Ctx.HaveG
    UseBasis = false;
end
Sh = localShapeStruct(W,IFr,UseBasis);
if WithDelay
    [Lr,A,Mu] = localJob(Sh,Pp.Tau,Pp.r,Ctx);
else
    [Lr,A,Mu] = localJob(Sh,0,0,Ctx);
end
NLL = -Lr;
end


function S = localParamStruct(Is,ShapeSize,LowVec,HighVec,BreakVec,Gamma1Vec,Gamma2Vec,IsSingle)
[iL,iH,iB,iG1,iG2] = ind2sub(ShapeSize,Is);
S = struct;
S.LowFreqCut   = LowVec(iL);
S.HighFreqCut  = HighVec(iH);
if IsSingle
    S.BreakFreq    = NaN;
    S.PowerLawInd2 = NaN;
else
    S.BreakFreq    = BreakVec(iB);
    S.PowerLawInd2 = Gamma2Vec(iG2);
end
S.PowerLawInd1 = Gamma1Vec(iG1);
end


function Pk = localFindPeaks(Prof)
% Indices of local maxima of a profile, sorted by decreasing value.
n  = numel(Prof);
P  = Prof(:).';
P(~isfinite(P)) = -Inf;
if n==1
    Pk = 1;
    return;
end
Left  = [-Inf, P(1:end-1)];
Right = [P(2:end), -Inf];
Pk = find(P>=Left & P>=Right & isfinite(P));
% remove plateau duplicates
if numel(Pk)>1
    Pk = Pk([true, diff(Pk)>1]);
end
[~,SI] = sort(P(Pk),'descend');
Pk = Pk(SI);
end


function R = localEmptyRefine()
R = struct('LowFreqCut',NaN,'HighFreqCut',NaN,'BreakFreq',NaN, ...
           'PowerLawInd1',NaN,'PowerLawInd2',NaN,'TimeDelay',NaN,'FluxRatio',NaN, ...
           'LogLikelihood',-Inf,'PowerSpecNorm',NaN,'MeanFlux',NaN, ...
           'StartTimeDelay',NaN,'StartLogLikelihood',NaN,'ExitFlag',NaN,'NFunEval',0);
end


function R = localFillRefine(S)
R = localEmptyRefine();
F = fieldnames(R);
for i=1:numel(F)
    if isfield(S,F{i})
        R.(F{i}) = S.(F{i});
    end
end
end


function B = localBestFromRefine(R1)
B = struct;
B.LowFreqCut    = R1.LowFreqCut;
B.HighFreqCut   = R1.HighFreqCut;
B.BreakFreq     = R1.BreakFreq;
B.PowerLawInd1  = R1.PowerLawInd1;
B.PowerLawInd2  = R1.PowerLawInd2;
B.TimeDelay     = R1.TimeDelay;
B.FluxRatio     = R1.FluxRatio;
B.LogLikelihood = R1.LogLikelihood;
B.PowerSpecNorm = R1.PowerSpecNorm;
B.MeanFlux      = R1.MeanFlux;
B.EffectiveRank = NaN;
B.Source        = 'refine';
end
