function [L, Info] = lightCurveFitPowerLawPowerSpec(Time, Flux, FluxErr, Args)
% lightCurveFitPowerLawPowerSpec - Fit a power-law PSD using a time-domain likelihood.
%
% Description:
%   Fit a stationary Gaussian stochastic-process model to an irregularly
%   sampled light curve in the TIME DOMAIN. The source PSD is
%
%       P(f) = A .* Shape(f),      LowFreqCut <= f <= HighFreqCut,
%
%   with zero power outside the fitted band. The time-domain covariance is
%
%       C(dt) = integral P(f).*cos(2*pi*f*dt) df .
%
%   A is the PSD value at Args.FRef and is profiled separately at every
%   PSD-shape grid point. An unknown constant mean flux is also profiled.
%
%   For a single power law:
%       Shape(f) = (f/FRef).^(-Gamma1).
%
%   For a continuous broken power law the shape is normalized so that
%   Shape(FRef)=1 for EVERY trial break frequency:
%
%       B(f) = (f/fb).^(-Gamma1),   f <= fb
%            = (f/fb).^(-Gamma2),   f >  fb
%
%       Shape(f) = B(f)./B(FRef).
%
%   Hence Info.PowerSpecNorm always has the same meaning: P(FRef).
%
%   The covariance integral is represented by a uniformly spaced Fourier
%   quadrature. Uniform spacing is important: to represent the covariance
%   over a data span T, the frequency spacing must be approximately <=1/T.
%   Args.NFreq=0 selects the number of frequencies automatically from this
%   criterion.
%
% Input arguments:
%   Time       - Observation times [N x 1]. Arbitrary sampling is allowed.
%   Flux       - Flux measurements [N x 1].
%   FluxErr    - 1-sigma independent Gaussian errors. Scalar or [N x 1].
%   Args       - Optional key/value arguments:
%       .LowFreqCut     - Scalar/vector lower cutoff frequencies.
%                         Default: 1/(max(Time)-min(Time)).
%       .HighFreqCut    - Scalar/vector upper cutoff frequencies.
%                         Default: 0.5/median(diff(sort(Time))).
%       .BreakFreq      - NaN for a single power law, or scalar/vector break
%                         frequencies. Default: NaN.
%       .PowerLawInd    - Scalar/vector low-frequency slopes Gamma1.
%                         Default: 1.5:0.1:3.5.
%       .PowerLawInd2   - Scalar/vector high-frequency slopes Gamma2.
%                         Default: PowerLawInd. Ignored for a single PL.
%       .FRef           - Fixed reference frequency at which A=P(FRef).
%                         Default: geometric mean of global fitted band.
%       .NFreq          - Number of positive-frequency quadrature bins.
%                         0 means automatic. Default: 0.
%       .FreqOversample - Automatic frequency-resolution factor. The auto
%                         rule is NFreq >= Oversample*(fmax-fmin)*TimeSpan.
%                         Default: 2.
%       .MinNFreq       - Minimum automatic NFreq. Default: 128.
%       .FitMean        - Profile an unknown constant mean. Default: true.
%       .NormRangeFact  - Search A over Aguess*[1/F,F]. Default: 1e8.
%       .NormTol        - fminbnd tolerance in log(A). Default: 1e-4.
%       .EigTol         - Relative eigenvalue cutoff. Default: 1e-12.
%       .WarnUnderResolved - Warn if user NFreq gives df>1/TimeSpan.
%                         Default: true.
%
% Output arguments:
%   L          - Profile log-likelihood grid.
%                Single PL:
%                   L(iLow,iHigh,1,iGamma1)
%                Broken PL:
%                   L(iLow,iHigh,iBreak,iGamma1,iGamma2)
%   Info       - Structure with grids, fitted PSD normalization and mean,
%                best-fit parameters, frequency grid and diagnostics.
%
% Notes:
%   1. This is the full Gaussian time-domain likelihood discussed by
%      Springer & Ofek (2021, MNRAS, 506, 864), with the covariance obtained
%      from the supplied PSD family.
%   2. The Fourier basis is only a numerical representation of the covariance
%      integral. The likelihood is evaluated on the time-domain flux vector;
%      it is not a periodogram/Whittle likelihood.
%   3. A logarithmic frequency grid is NOT used because it can badly
%      under-resolve the oscillatory covariance integral at long time lags.
%   4. For each PSD shape the code diagonalizes the smaller of the basis-space
%      and data-space whitened signal covariance. This allows the PSD
%      normalization and constant mean to be profiled cheaply afterwards.
%   5. If the requested frequency band approaches the sampling Nyquist range,
%      an accurate time-domain covariance may require O(N) Fourier nodes. In
%      that regime the calculation is intrinsically no longer low rank.
%
% Example:
%   [L,Info] = lightCurveFitPowerLawPowerSpec(T,F,E, ...
%       'LowFreqCut',1e-3, ...
%       'HighFreqCut',0.2, ...
%       'BreakFreq',NaN, ...
%       'PowerLawInd',1.5:0.1:3.0);
%
% Reference:
%   Springer, O. M., & Ofek, E. O. 2021, MNRAS, 506, 864.
%
% Author: ChatGPT

arguments
    Time (:,1) double
    Flux (:,1) double
    FluxErr double
    Args.LowFreqCut double = NaN
    Args.HighFreqCut double = NaN
    Args.BreakFreq double = NaN
    Args.PowerLawInd double = 1.5:0.1:3.5
    Args.PowerLawInd2 double = []
    Args.FRef (1,1) double = NaN
    Args.NFreq (1,1) double = 0
    Args.FreqOversample (1,1) double {mustBePositive} = 2
    Args.MinNFreq (1,1) double {mustBeInteger,mustBePositive} = 128
    Args.FitMean (1,1) logical = true
    Args.NormRangeFact (1,1) double {mustBePositive} = 1e8
    Args.NormTol (1,1) double {mustBePositive} = 1e-4
    Args.EigTol (1,1) double {mustBePositive} = 1e-12
    Args.WarnUnderResolved (1,1) logical = true
    Args.FlatBelowFlow (1,1) logical = true
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

if N<3
    error('At least three valid measurements are required.');
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
    HighVec = 0.5./median(Dt);
end

BreakVec = Args.BreakFreq(:).';
IsSingle = all(isnan(BreakVec));
if ~IsSingle && any(isnan(BreakVec))
    error('BreakFreq must be either NaN or contain only finite values.');
end

Gamma1Vec = Args.PowerLawInd(:).';
if isempty(Args.PowerLawInd2)
    Gamma2Vec = Gamma1Vec;
else
    Gamma2Vec = Args.PowerLawInd2(:).';
end

if isempty(LowVec) || isempty(HighVec) || isempty(Gamma1Vec)
    error('LowFreqCut, HighFreqCut, and PowerLawInd cannot be empty.');
end
if Args.NormRangeFact<=1
    error('NormRangeFact must be larger than 1.');
end
if Args.NFreq<0 || Args.NFreq~=round(Args.NFreq)
    error('NFreq must be a non-negative integer; use 0 for automatic.');
end
if any(~isfinite(LowVec) | LowVec<=0)
    error('LowFreqCut values must be finite and >0.');
end
if any(~isfinite(HighVec) | HighVec<=0)
    error('HighFreqCut values must be finite and >0.');
end
if ~IsSingle && any(~isfinite(BreakVec) | BreakVec<=0)
    error('Finite BreakFreq values must be >0.');
end
if any(~isfinite(Gamma1Vec)) || (~IsSingle && any(~isfinite(Gamma2Vec)))
    error('Power-law indices must be finite.');
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

% The reported normalization is P(FRef). If FRef falls outside the fitted
% band, P(FRef) is an extrapolation of the fitted power law rather than an
% in-band power. Record this and warn (unless warnings are suppressed).
FRefInBand = (FRef>=GlobalLow) && (FRef<=GlobalHigh);
if ~FRefInBand && Args.WarnUnderResolved
    warning('lightCurveFitPowerLawPowerSpec:FRefOutsideBand', ...
        ['FRef=%g is outside the fitted frequency band [%g, %g]. The reported ', ...
         'PowerSpecNorm=P(FRef) is an extrapolation of the fitted power law, ', ...
         'not an in-band power.'], FRef, GlobalLow, GlobalHigh);
end

% Flat (white) PSD floor below FLow: when enabled, extend the quadrature down to
% zero frequency and hold the PSD constant at its FLow value for f<FLow. This is a
% convergent, DRW-like low-frequency continuation (finite low-f variance) instead
% of a sharp zero cutoff, which reduces the upward bias of the recovered slope
% caused by real power on timescales longer than the baseline.
FlatFloor = Args.FlatBelowFlow;
if FlatFloor
    QuadLow = 0;
else
    QuadLow = GlobalLow;
end

%--------------------------
% Frequency quadrature
%--------------------------
% Two requirements set the number of quadrature nodes:
%   (a) Covariance resolution. The covariance contains cos(2*pi*f*dt), with
%       |dt| up to Span, so the grid must resolve oscillations on ~1/Span.
%   (b) Cutoff separation. Distinct LowFreqCut/HighFreqCut grid values must
%       select DISTINCT sets of active nodes; otherwise two nearby cutoffs
%       fall in the same quadrature bin, select an identical band, and return
%       identical (spuriously degenerate) likelihoods. This requires df to be
%       smaller than the smallest gap between distinct cutoff values.
CutVals = unique([LowVec, HighVec]);
CutVals = CutVals(isfinite(CutVals));
if numel(CutVals)>=2
    MinCutGap   = min(diff(CutVals));
    NFreqCutoff = ceil((GlobalHigh-GlobalLow)./MinCutGap)+1;
else
    MinCutGap   = Inf;
    NFreqCutoff = 0;
end
NFreqCov = ceil((GlobalHigh-QuadLow).*Span);
MinRecommendedNFreq = max([2, NFreqCov, NFreqCutoff]);
AutoNFreq = max([Args.MinNFreq, ...
                 ceil(Args.FreqOversample.*(GlobalHigh-QuadLow).*Span), ...
                 NFreqCutoff]);

if Args.NFreq==0
    NF = AutoNFreq;
else
    NF = Args.NFreq;
    if Args.WarnUnderResolved && NF<MinRecommendedNFreq
        warning('lightCurveFitPowerLawPowerSpec:UnderResolvedFrequencyGrid', ...
            ['NFreq=%d is smaller than the approximate minimum %d required to ', ...
             'resolve the covariance (df<=1/TimeSpan) and to separate the ', ...
             'distinct frequency cutoffs. The PSD-shape/cutoff likelihood may ', ...
             'be biased or degenerate.'], NF,MinRecommendedNFreq);
    end
end

% Uniform midpoint quadrature. A uniform grid is required for stable
% convergence of the oscillatory cosine transform over long time lags.
Edges = linspace(QuadLow,GlobalHigh,NF+1);
Freq  = 0.5.*(Edges(1:end-1)+Edges(2:end));
dFreq = diff(Edges);

% Fixed time-domain basis. Phi*diag(Pshape duplicated)*Phi' approximates
% integral Pshape(f) cos(2*pi*f*(ti-tj)) df.
T0 = mean(Time);
TT = Time-T0;
Phase = 2.*pi.*(TT*Freq);
Wsqrt = sqrt(dFreq);
Phi = [cos(Phase).*Wsqrt, sin(Phase).*Wsqrt];
clear Phase

% Whiten by measurement noise once. X has covariance contribution
% X*D*X' for unit PSD normalization.
InvErr = 1./FluxErr;
X  = Phi.*InvErr;
clear Phi

yw = Flux.*InvErr;
uw = InvErr;

y2 = yw.'*yw;
yu = yw.'*uw;
u2 = uw.'*uw;
hY = X.'*yw;
hU = X.'*uw;

LogDetNoise = 2.*sum(log(FluxErr));
Log2PiTerm  = N.*log(2.*pi);

VarData = var(Flux,1);
VarNoise = mean(FluxErr.^2);
VarIntrinsicGuess = max(VarData-VarNoise,0.01.*max(VarData,realmin));

%--------------------------
% Parameter grid
%--------------------------
nL  = numel(LowVec);
nH  = numel(HighVec);
nP1 = numel(Gamma1Vec);

if IsSingle
    GridSize = [nL,nH,1,nP1];
    [IL,IH,IB,IP1] = ndgrid(1:nL,1:nH,1,1:nP1); %#ok<ASGLU>
    IndTable = [IL(:),IH(:),IB(:),IP1(:)];
else
    nB  = numel(BreakVec);
    nP2 = numel(Gamma2Vec);
    GridSize = [nL,nH,nB,nP1,nP2];
    [IL,IH,IB,IP1,IP2] = ndgrid(1:nL,1:nH,1:nB,1:nP1,1:nP2);
    IndTable = [IL(:),IH(:),IB(:),IP1(:),IP2(:)];
end

NModel  = size(IndTable,1);
Lvec    = -Inf(NModel,1);
NormVec = NaN(NModel,1);
MeanVec = NaN(NModel,1);
RankVec = zeros(NModel,1,'uint32');

%--------------------------
% Scan PSD-shape grid
%--------------------------
for Imod=1:NModel
    iL  = IndTable(Imod,1);
    iH  = IndTable(Imod,2);
    iP1 = IndTable(Imod,4);

    FLow   = LowVec(iL);
    FHigh  = HighVec(iH);
    Gamma1 = Gamma1Vec(iP1);

    if FLow>=FHigh
        continue;
    end

    if FlatFloor
        ActiveF = Freq<=FHigh;            % include the flat floor [0,FLow]
        FreqEff = max(Freq,FLow);         % flat (white) PSD below FLow
    else
        ActiveF = Freq>=FLow & Freq<=FHigh;
        FreqEff = Freq;
    end
    IFr = find(ActiveF);
    if numel(IFr)<2
        continue;
    end

    if IsSingle
        Pshape = (FreqEff./FRef).^(-Gamma1);
    else
        iB  = IndTable(Imod,3);
        iP2 = IndTable(Imod,5);
        FBreak = BreakVec(iB);
        Gamma2 = Gamma2Vec(iP2);

        if FBreak<=FLow || FBreak>=FHigh
            continue;
        end

        % Continuous broken PL written around fb and then normalized at the
        % FIXED FRef. This keeps A=P(FRef) for all trial break frequencies.
        Pshape = zeros(1,NF);
        LowSide = FreqEff<=FBreak;
        Pshape(LowSide)  = (FreqEff(LowSide)./FBreak).^(-Gamma1);
        Pshape(~LowSide) = (FreqEff(~LowSide)./FBreak).^(-Gamma2);

        if FRef<=FBreak
            RefShape = (FRef./FBreak).^(-Gamma1);
        else
            RefShape = (FRef./FBreak).^(-Gamma2);
        end
        Pshape = Pshape./RefShape;
    end

    IFr = IFr(:).';
    Col = [IFr,IFr+NF];
    Dsqrt = sqrt([Pshape(IFr),Pshape(IFr)]).';
    R = numel(Col);

    % Non-zero eigenvalues/eigenvectors of the whitened signal covariance.
    % Work in whichever space is smaller: basis space (R x R) or data space
    % (N x N). This is exact for the adopted Fourier quadrature.
    if R<=N
        Xc = X(:,Col);
        Gm = Xc.'*Xc;
        Csmall = (Dsqrt.*Gm).*Dsqrt.';
        Csmall = 0.5.*(Csmall+Csmall.');

        [V,Lam] = eig(Csmall,'vector');
        Lam = real(Lam);
        if isempty(Lam)
            continue;
        end
        MaxLam = max(Lam);
        if ~(isfinite(MaxLam) && MaxLam>0)
            continue;
        end
        Keep = Lam>Args.EigTol.*MaxLam;
        Lam = Lam(Keep);
        V   = V(:,Keep);
        if isempty(Lam)
            continue;
        end

        rY = Dsqrt.*hY(Col);
        rU = Dsqrt.*hU(Col);
        zY = (V.'*rY)./sqrt(Lam);
        zU = (V.'*rU)./sqrt(Lam);
    else
        Xc = X(:,Col);
        B  = Xc.*Dsqrt.';
        Cdata = B*B.';
        Cdata = 0.5.*(Cdata+Cdata.');
        clear B Xc

        [U,Lam] = eig(Cdata,'vector');
        Lam = real(Lam);
        if isempty(Lam)
            continue;
        end
        MaxLam = max(Lam);
        if ~(isfinite(MaxLam) && MaxLam>0)
            continue;
        end
        Keep = Lam>Args.EigTol.*MaxLam;
        Lam = Lam(Keep);
        U   = U(:,Keep);
        if isempty(Lam)
            continue;
        end

        zY = U.'*yw;
        zU = U.'*uw;
        clear U Cdata
    end

    RankVec(Imod) = uint32(numel(Lam));

    % Initial PSD normalization from the intrinsic sample variance.
    S0 = sum(Pshape(ActiveF).*dFreq(ActiveF));
    if ~(isfinite(S0) && S0>0)
        continue;
    end
    Aguess = max(VarIntrinsicGuess./S0,realmin('double'));

    Fact = Args.NormRangeFact;
    Amin = max(Aguess./Fact,realmin('double'));
    Amax = Aguess.*Fact;
    if ~(isfinite(Amax) && Amax>Amin)
        Amax = realmax('double').^(1/8);
    end

    Obj = @(LogA) localNLL(exp(LogA),Lam,zY,zU,y2,yu,u2, ...
                           LogDetNoise,Log2PiTerm,Args.FitMean);
    Opt = optimset('Display','off','TolX',Args.NormTol);
    [LogAbest,NLLbest] = fminbnd(Obj,log(Amin),log(Amax),Opt);
    Abest = exp(LogAbest);

    % Include the A=0 boundary explicitly.
    [NLLzero,MeanZero] = localNLL(0,Lam,zY,zU,y2,yu,u2, ...
                                  LogDetNoise,Log2PiTerm,Args.FitMean);
    [~,MeanBest] = localNLL(Abest,Lam,zY,zU,y2,yu,u2, ...
                            LogDetNoise,Log2PiTerm,Args.FitMean);
    if NLLzero<NLLbest
        NLLbest  = NLLzero;
        Abest    = 0;
        MeanBest = MeanZero;
    end

    Lvec(Imod)    = -NLLbest;
    NormVec(Imod) = Abest;
    MeanVec(Imod) = MeanBest;
end

%--------------------------
% Output
%--------------------------
L = reshape(Lvec,GridSize);

Info = struct;
Info.LowFreqCut     = LowVec;
Info.HighFreqCut    = HighVec;
Info.BreakFreq      = BreakVec;
Info.PowerLawInd1   = Gamma1Vec;
Info.PowerLawInd2   = iff(IsSingle,[],Gamma2Vec);
Info.FRef           = FRef;
Info.FRefInBand     = FRefInBand;
Info.NFreq          = NF;
Info.NFreqWasAuto   = Args.NFreq==0;
Info.FreqOversample = Args.FreqOversample;
Info.MinRecommendedNFreq = MinRecommendedNFreq;
Info.Frequency      = Freq;
Info.FrequencyEdge  = Edges;
Info.FrequencyStep  = dFreq;
Info.PowerSpecNorm  = reshape(NormVec,GridSize);
Info.MeanFlux       = reshape(MeanVec,GridSize);
Info.EffectiveRank  = reshape(RankVec,GridSize);
Info.GridSize       = GridSize;
Info.GridOrder      = iff(IsSingle, ...
    {'LowFreqCut','HighFreqCut','BreakFreq(singleton)','PowerLawInd1'}, ...
    {'LowFreqCut','HighFreqCut','BreakFreq','PowerLawInd1','PowerLawInd2'});
Info.FitMean        = Args.FitMean;
Info.NData          = N;
Info.NInput         = Ninput;
Info.NRemoved       = Ninput-N;
Info.Method         = 'Full time-domain Gaussian likelihood; uniformly sampled Fourier covariance quadrature';
Info.PSDConvention  = 'One-sided PSD in cycles per unit Time: C(dt)=int P(f) cos(2*pi*f*dt) df';
Info.PowerSpecNormDefinition = 'P(FRef)';

[BestL,BestLin] = max(Lvec);
Info.Best = struct;
Info.Best.LogLikelihood = BestL;
if isfinite(BestL)
    if IsSingle
        [iL,iH,~,iP1] = ind2sub(GridSize,BestLin);
        Info.Best.LowFreqCut   = LowVec(iL);
        Info.Best.HighFreqCut  = HighVec(iH);
        Info.Best.BreakFreq    = NaN;
        Info.Best.PowerLawInd1 = Gamma1Vec(iP1);
        Info.Best.PowerLawInd2 = NaN;
    else
        [iL,iH,iB,iP1,iP2] = ind2sub(GridSize,BestLin);
        Info.Best.LowFreqCut   = LowVec(iL);
        Info.Best.HighFreqCut  = HighVec(iH);
        Info.Best.BreakFreq    = BreakVec(iB);
        Info.Best.PowerLawInd1 = Gamma1Vec(iP1);
        Info.Best.PowerLawInd2 = Gamma2Vec(iP2);
    end
    Info.Best.PowerSpecNorm = NormVec(BestLin);
    Info.Best.MeanFlux      = MeanVec(BestLin);
    Info.Best.EffectiveRank = RankVec(BestLin);
    Info.Best.LinearIndex   = BestLin;
else
    Info.Best.LowFreqCut    = NaN;
    Info.Best.HighFreqCut   = NaN;
    Info.Best.BreakFreq     = NaN;
    Info.Best.PowerLawInd1  = NaN;
    Info.Best.PowerLawInd2  = NaN;
    Info.Best.PowerSpecNorm = NaN;
    Info.Best.MeanFlux      = NaN;
    Info.Best.EffectiveRank = uint32(0);
    Info.Best.LinearIndex   = NaN;
end

end


function [NLL,Mu] = localNLL(A,Lam,zY,zU,y2,yu,u2,LogDetNoise,Log2PiTerm,FitMean)
% Evaluate the profiled Gaussian negative log likelihood for PSD amplitude A.

if A<0 || ~isfinite(A)
    NLL = Inf;
    Mu  = NaN;
    return;
end

AL = A.*Lam;
Den = 1+AL;
if any(~isfinite(Den) | Den<=0)
    NLL = Inf;
    Mu  = NaN;
    return;
end

c = AL./Den;
Qyy = y2-sum(c.*(zY.^2));

if FitMean
    Qyu = yu-sum(c.*zY.*zU);
    Quu = u2-sum(c.*(zU.^2));
    if ~(isfinite(Quu) && Quu>0)
        NLL = Inf;
        Mu  = NaN;
        return;
    end
    Mu = Qyu./Quu;
    Quad = Qyy-(Qyu.^2)./Quu;
else
    Mu = 0;
    Quad = Qyy;
end

if Quad<0 && Quad>-1e-10.*max(y2,1)
    Quad = 0;
end
if ~(isfinite(Quad) && Quad>=0)
    NLL = Inf;
    return;
end

LogDet = LogDetNoise+sum(log1p(AL));
NLL = 0.5.*(LogDet+Quad+Log2PiTerm);

end


function Out = iff(Condition,A,B)
if Condition
    Out = A;
else
    Out = B;
end
end
