function [FreqVec, Power, Info] = cleanestPowerSpec(T, Y, FreqVec, Args)
% cleanestPowerSpec  CLEANest-style spectrum for unevenly sampled data.
%
% [FreqVec,Power,Info] = cleanestPowerSpec(T,Y,FreqVec,...)
%
% The algorithm is a CLEANest-style hybrid:
%   1. compute a residual spectrum and select one candidate peak;
%   2. append that frequency to the active model;
%   3. jointly refine ALL active frequencies by nonlinear optimization;
%   4. for every trial set of frequencies, solve all sine/cosine amplitudes
%      simultaneously by weighted linear least squares (variable projection);
%   5. accept the new component only if the chosen stopping criterion improves;
%   6. repeat on the residual of the full joint model.
%
% This avoids the main weakness of ordinary CLEAN/prewhitening: previously
% selected frequencies and amplitudes are not frozen after later components
% are discovered.
%
% Name-value arguments
%   FirstPeakToSub  - Rank of first residual peak to try. 1=highest. Default 1.
%   SpectrumMethod  - "lombscargle" (default) or "fourier" for peak discovery.
%   CalcMethod      - "loop" (default, memory efficient) or "matrix".
%   MaxComponents   - Maximum number of accepted frequencies. Default 12.
%   MeanSubtract    - Remove weighted mean before processing. Default true.
%   Weights         - Nx1 non-negative weights. Default ones(N,1).
%   Refine          - Jointly refine frequencies continuously. Default true.
%   RefineHalfWidth - Bound each frequency during one refinement, in frequency
%                     units. [] -> 2/Tspan. Default [].
%   RefineMaxIter   - fminsearch iteration limit per outer step. Default 400.
%   RefineTolX      - fminsearch TolX. Default 1e-7.
%   RefineTolFun    - fminsearch TolFun. Default 1e-8.
%   MinSeparation   - Minimum allowed component separation. [] -> 0.5/Tspan.
%   StopMethod      - "bic" (default), "fraction", or "none".
%   MinBICImprove   - Required decrease in BIC to accept component. Default 2.
%   StopFraction    - For StopMethod="fraction", stop when max residual
%                     amplitude / initial maximum <= value. Default 1e-3.
%   RestoreFWHM     - Gaussian restoring FWHM. [] -> sampling-window estimate.
%   ReturnComplex   - Store complex spectra in Info. Default true.
%
% The final linear model is
%   y(t) = sum_k [C_k cos(2*pi*f_k*t) + S_k sin(2*pi*f_k*t)] + residual.
% For each frequency, the positive-frequency complex amplitude is
%   a_k = (C_k - i*S_k)/2.
%
% Power is a CLEAN-style restored spectrum: Gaussian-restored joint-fit
% components plus the final complex residual spectrum, squared in magnitude.
%
% Eran Ofek / ChatGPT, 2026-10-03

arguments
    T (:,1) double
    Y (:,1) double
    FreqVec (:,1) double
    Args.FirstPeakToSub (1,1) double {mustBeInteger,mustBePositive} = 1
    Args.SpectrumMethod (1,1) string = "lombscargle"
    Args.CalcMethod (1,1) string = "loop"
    Args.MaxComponents (1,1) double {mustBeInteger,mustBePositive} = 12
    Args.MeanSubtract (1,1) logical = true
    Args.Weights (:,1) double = []
    Args.Refine (1,1) logical = true
    Args.RefineHalfWidth double {mustBeNonnegative} = []
    Args.RefineMaxIter (1,1) double {mustBeInteger,mustBePositive} = 400
    Args.RefineTolX (1,1) double {mustBePositive} = 1e-7
    Args.RefineTolFun (1,1) double {mustBePositive} = 1e-8
    Args.MinSeparation double {mustBeNonnegative} = []
    Args.StopMethod (1,1) string = "bic"
    Args.MinBICImprove (1,1) double {mustBeNonnegative} = 2
    Args.StopFraction (1,1) double {mustBeNonnegative} = 1e-3
    Args.RestoreFWHM double {mustBeNonnegative} = []
    Args.ReturnComplex (1,1) logical = true
end

SpectrumMethod = lower(Args.SpectrumMethod);
CalcMethod = lower(Args.CalcMethod);
StopMethod = lower(Args.StopMethod);
if ~ismember(SpectrumMethod,["fourier","lombscargle"])
    error('cleanestPowerSpec:BadSpectrumMethod', ...
        'SpectrumMethod must be "fourier" or "lombscargle".');
end
if ~ismember(CalcMethod,["loop","matrix"])
    error('cleanestPowerSpec:BadCalcMethod', ...
        'CalcMethod must be "loop" or "matrix".');
end
if ~ismember(StopMethod,["bic","fraction","none"])
    error('cleanestPowerSpec:BadStopMethod', ...
        'StopMethod must be "bic", "fraction", or "none".');
end

T = T(:); Y = Y(:); FreqVec = FreqVec(:);
N = numel(T); M = numel(FreqVec);
if numel(Y) ~= N
    error('cleanestPowerSpec:SizeMismatch','T and Y must have the same length.');
end
if N < 4 || M < 3
    error('cleanestPowerSpec:TooFewData','Need at least 4 samples and 3 trial frequencies.');
end
if any(~isfinite(T)) || any(~isfinite(Y)) || any(~isfinite(FreqVec))
    error('cleanestPowerSpec:NonFinite','T, Y, and FreqVec must be finite.');
end
if any(diff(FreqVec) <= 0)
    error('cleanestPowerSpec:FreqOrder','FreqVec must be strictly increasing.');
end

if isempty(Args.Weights)
    Wgt = ones(N,1);
else
    Wgt = Args.Weights(:);
    if numel(Wgt) ~= N
        error('cleanestPowerSpec:WeightSize','Weights must have the same length as T.');
    end
    if any(~isfinite(Wgt)) || any(Wgt < 0) || ~any(Wgt > 0)
        error('cleanestPowerSpec:BadWeights','Weights must be finite, non-negative, and not all zero.');
    end
end
Wsum = sum(Wgt);
Tspan = max(T)-min(T);
if Tspan <= 0
    error('cleanestPowerSpec:ZeroSpan','Time samples must span a non-zero interval.');
end
if isempty(Args.RefineHalfWidth)
    RefineHalfWidth = 2/Tspan;
else
    RefineHalfWidth = Args.RefineHalfWidth;
end
if isempty(Args.MinSeparation)
    MinSeparation = 0.5/Tspan;
else
    MinSeparation = Args.MinSeparation;
end

if Args.MeanSubtract
    mu = sum(Wgt.*Y)/Wsum;
    Y0 = Y-mu;
else
    mu = 0;
    Y0 = Y;
end

% Initial null model.
ResidualY = Y0;
[DirtyComplex,DirtyPower] = localSpectrum(T,ResidualY,Wgt,FreqVec,SpectrumMethod,CalcMethod);
initialMax = max(sqrt(max(DirtyPower,0)));
RSS0 = sum(Wgt.*ResidualY.^2);
BIC0 = localBIC(RSS0,N,0);

ActiveFreq = zeros(0,1);
ActiveCoef = complex(zeros(0,1));
ModelY = zeros(N,1);
RSS = RSS0;
BIC = BIC0;

CandidateGridFreq = nan(Args.MaxComponents,1);
Accepted = false(Args.MaxComponents,1);
RSSHistory = nan(Args.MaxComponents+1,1); RSSHistory(1)=RSS0;
BICHistory = nan(Args.MaxComponents+1,1); BICHistory(1)=BIC0;
BICImprove = nan(Args.MaxComponents,1);
ResidualMax = nan(Args.MaxComponents,1);
RefinedFreqHistory = cell(Args.MaxComponents,1);
ExitFlag = nan(Args.MaxComponents,1);
FuncCount = nan(Args.MaxComponents,1);
IterCount = nan(Args.MaxComponents,1);

ResidualComplex = DirtyComplex;
ResidualPower = DirtyPower;

for outer = 1:Args.MaxComponents
    amp = sqrt(max(ResidualPower,0));
    pk = localPeakIndices(amp);
    if isempty(pk)
        [~,pk] = max(amp);
    end
    [~,ord] = sort(amp(pk),'descend');
    pk = pk(ord);

    % Do not propose a frequency already represented by an active component.
    if ~isempty(ActiveFreq)
        keep = true(size(pk));
        for j=1:numel(pk)
            keep(j) = all(abs(FreqVec(pk(j))-ActiveFreq) >= MinSeparation);
        end
        pk = pk(keep);
    end
    if isempty(pk)
        break;
    end

    if outer==1
        if Args.FirstPeakToSub > numel(pk)
            error('cleanestPowerSpec:PeakRankTooLarge', ...
                'FirstPeakToSub=%d, but only %d eligible peaks were found.', ...
                Args.FirstPeakToSub,numel(pk));
        end
        k = pk(Args.FirstPeakToSub);
    else
        k = pk(1);
    end
    fNew = FreqVec(k);
    CandidateGridFreq(outer)=fNew;

    TrialFreq0 = [ActiveFreq; fNew];
    [TrialFreq,TrialCoef,TrialModel,TrialRSS,optInfo] = localJointRefit( ...
        T,Y0,Wgt,TrialFreq0,FreqVec(1),FreqVec(end),RefineHalfWidth, ...
        MinSeparation,Args.Refine,Args.RefineMaxIter,Args.RefineTolX,Args.RefineTolFun);

    TrialBIC = localBIC(TrialRSS,N,numel(TrialFreq));
    dBIC = BIC-TrialBIC;
    BICImprove(outer)=dBIC;
    ExitFlag(outer)=optInfo.ExitFlag;
    FuncCount(outer)=optInfo.FuncCount;
    IterCount(outer)=optInfo.Iterations;
    RefinedFreqHistory{outer}=TrialFreq;

    switch StopMethod
        case "bic"
            accept = (dBIC >= Args.MinBICImprove);
        case {"fraction","none"}
            accept = true;
    end

    if ~accept
        break;
    end

    Accepted(outer)=true;
    ActiveFreq = TrialFreq;
    ActiveCoef = TrialCoef;
    ModelY = TrialModel;
    RSS = TrialRSS;
    BIC = TrialBIC;
    ResidualY = Y0-ModelY;
    [ResidualComplex,ResidualPower] = localSpectrum( ...
        T,ResidualY,Wgt,FreqVec,SpectrumMethod,CalcMethod);
    ResidualMax(outer)=max(sqrt(max(ResidualPower,0)));
    RSSHistory(outer+1)=RSS;
    BICHistory(outer+1)=BIC;

    if StopMethod=="fraction" && ResidualMax(outer) <= Args.StopFraction*initialMax
        break;
    end
end

nTried = find(~isnan(CandidateGridFreq),1,'last');
if isempty(nTried), nTried=0; end
nComp = numel(ActiveFreq);

% Final simultaneous linear refit at the final refined frequencies.
if nComp>0
    [ActiveCoef,ModelY,RSS] = localLinearFit(T,Y0,Wgt,ActiveFreq);
    ResidualY = Y0-ModelY;
    [ResidualComplex,ResidualPower] = localSpectrum( ...
        T,ResidualY,Wgt,FreqVec,SpectrumMethod,CalcMethod);
    BIC = localBIC(RSS,N,nComp);
end

% CLEAN restoring beam.
if isempty(Args.RestoreFWHM)
    RestoreFWHM = localEstimateWindowFWHM(T,Wgt,Wsum,FreqVec,CalcMethod);
else
    RestoreFWHM = Args.RestoreFWHM;
end
sigmaF = RestoreFWHM/(2*sqrt(2*log(2)));
RestoredComplex = ResidualComplex;
if nComp>0
    for j=1:nComp
        if sigmaF>0
            g = exp(-0.5*((FreqVec-ActiveFreq(j))/sigmaF).^2);
        else
            [~,kk]=min(abs(FreqVec-ActiveFreq(j)));
            g=zeros(M,1); g(kk)=1;
        end
        RestoredComplex = RestoredComplex + ActiveCoef(j).*g;
    end
end
Power = abs(RestoredComplex).^2;

Info = struct;
Info.Mean = mu;
Info.SpectrumMethod = char(SpectrumMethod);
Info.CalcMethod = char(CalcMethod);
Info.StopMethod = char(StopMethod);
Info.FirstPeakToSub = Args.FirstPeakToSub;
Info.MaxComponents = Args.MaxComponents;
Info.Ncomponents = nComp;
Info.Ntried = nTried;
Info.ComponentFreq = ActiveFreq;
Info.ComponentCoef = ActiveCoef;
Info.ComponentAmplitude = 2*abs(ActiveCoef);
Info.ComponentPhase = angle(ActiveCoef);
Info.ModelY = ModelY;
Info.ResidualY = ResidualY;
Info.RSS = RSS;
Info.BIC = BIC;
Info.NullRSS = RSS0;
Info.NullBIC = BIC0;
Info.CandidateGridFreq = CandidateGridFreq(1:nTried);
Info.Accepted = Accepted(1:nTried);
Info.BICImprove = BICImprove(1:nTried);
Info.RSSHistory = RSSHistory(1:min(nComp+1,numel(RSSHistory)));
Info.BICHistory = BICHistory(1:min(nComp+1,numel(BICHistory)));
Info.ResidualMax = ResidualMax(1:nTried);
Info.RefinedFreqHistory = RefinedFreqHistory(1:nTried);
Info.RefineExitFlag = ExitFlag(1:nTried);
Info.RefineFuncCount = FuncCount(1:nTried);
Info.RefineIterations = IterCount(1:nTried);
Info.RefineHalfWidth = RefineHalfWidth;
Info.MinSeparation = MinSeparation;
Info.RestoreFWHM = RestoreFWHM;
Info.DirtyPower = DirtyPower;
Info.ResidualPower = ResidualPower;
if Args.ReturnComplex
    Info.DirtyComplex = DirtyComplex;
    Info.ResidualComplex = ResidualComplex;
    Info.RestoredComplex = RestoredComplex;
end
end


function [Freq,Coef,Model,RSS,Opt] = localJointRefit(T,Y,Wgt,Freq0,fmin,fmax,halfWidth,minSep,doRefine,maxIter,tolX,tolFun)
Freq0 = sort(Freq0(:));
K = numel(Freq0);
if ~doRefine || K==0
    Freq = Freq0;
    [Coef,Model,RSS] = localLinearFit(T,Y,Wgt,Freq);
    Opt=struct('ExitFlag',0,'FuncCount',0,'Iterations',0);
    return;
end

lo = max(fmin,Freq0-halfWidth);
hi = min(fmax,Freq0+halfWidth);
% If a bound collapses numerically, open it by a tiny amount.
small = max(eps(max(abs(Freq0))),1e-12);
hi = max(hi,lo+small);

% Unconstrained coordinate z mapped through logistic into [lo,hi].
r = (Freq0-lo)./(hi-lo);
r = min(max(r,1e-6),1-1e-6);
z0 = log(r./(1-r));

obj = @(z) localProfileObjective(z,T,Y,Wgt,lo,hi,minSep);
opts = optimset('Display','off','MaxIter',maxIter,'MaxFunEvals',max(2000,20*maxIter), ...
    'TolX',tolX,'TolFun',tolFun);
[z,~,exitflag,out] = fminsearch(obj,z0,opts);
Freq = lo + (hi-lo)./(1+exp(-z(:)));
Freq = sort(Freq);
[Coef,Model,RSS] = localLinearFit(T,Y,Wgt,Freq);
Opt=struct('ExitFlag',exitflag,'FuncCount',out.funcCount,'Iterations',out.iterations);
end


function val = localProfileObjective(z,T,Y,Wgt,lo,hi,minSep)
f = lo + (hi-lo)./(1+exp(-z(:)));
f = sort(f);
if numel(f)>1
    d = diff(f);
    if any(d < minSep)
        % Smooth-ish large penalty for nearly duplicate frequencies.
        pen = sum((max(minSep-d,0)./max(minSep,eps)).^2);
    else
        pen = 0;
    end
else
    pen = 0;
end
[~,~,rss] = localLinearFit(T,Y,Wgt,f);
scale = max(sum(Wgt.*Y.^2),eps);
val = rss/scale + 1e3*pen;
end


function [Coef,Model,RSS] = localLinearFit(T,Y,Wgt,Freq)
K=numel(Freq);
if K==0
    Coef=complex(zeros(0,1)); Model=zeros(size(Y)); RSS=sum(Wgt.*Y.^2); return;
end
ang = 2*pi*(T*Freq(:).');
X = [cos(ang), sin(ang)];
sw = sqrt(Wgt);
Xw = X.*sw;
yw = Y.*sw;
beta = Xw\yw;
Model = X*beta;
RSS = sum(Wgt.*(Y-Model).^2);
C = beta(1:K);
S = beta(K+1:end);
Coef = 0.5*complex(C,-S);
end


function bic = localBIC(RSS,N,K)
% Three parameters/component: frequency, cosine coeff, sine coeff.
kpar = 3*K;
bic = N*log(max(RSS/N,realmin)) + kpar*log(N);
end


function [A,P] = localSpectrum(T,Y,Wgt,FreqVec,SpectrumMethod,CalcMethod)
Nf=numel(FreqVec); Wsum=sum(Wgt); A=complex(zeros(Nf,1));
switch SpectrumMethod
    case "fourier"
        switch CalcMethod
            case "loop"
                WY=Wgt.*Y;
                for k=1:Nf
                    A(k)=sum(WY.*exp(-2i*pi*FreqVec(k).*T))/Wsum;
                end
            case "matrix"
                E=exp(-2i*pi*(T*FreqVec.'));
                A=((Wgt.*Y).'*E).'/Wsum;
        end
    case "lombscargle"
        switch CalcMethod
            case "loop"
                WY=Wgt.*Y;
                for k=1:Nf
                    a=2*pi*FreqVec(k).*T; c=cos(a); s=sin(a);
                    CC=sum(Wgt.*c.*c); SS=sum(Wgt.*s.*s); CS=sum(Wgt.*c.*s);
                    CY=sum(WY.*c); SY=sum(WY.*s); detG=CC*SS-CS^2;
                    if detG>eps(max(CC*SS,1))
                        bc=(CY*SS-SY*CS)/detG; bs=(SY*CC-CY*CS)/detG;
                        A(k)=0.5*complex(bc,-bs);
                    end
                end
            case "matrix"
                a=2*pi*(T*FreqVec.'); C=cos(a); S=sin(a);
                WC=Wgt.*C; WS=Wgt.*S;
                CC=sum(C.*WC,1).'; SS=sum(S.*WS,1).'; CS=sum(C.*WS,1).';
                WY=Wgt.*Y; CY=C.'*WY; SY=S.'*WY; detG=CC.*SS-CS.^2;
                good=detG>eps(max(CC.*SS,1)); bc=zeros(Nf,1); bs=zeros(Nf,1);
                bc(good)=(CY(good).*SS(good)-SY(good).*CS(good))./detG(good);
                bs(good)=(SY(good).*CC(good)-CY(good).*CS(good))./detG(good);
                A=0.5*complex(bc,-bs);
        end
end
P=abs(A).^2;
end


function Ind=localPeakIndices(A)
A=A(:); n=numel(A); mask=false(n,1);
if n==1, Ind=1; return; end
mask(1)=A(1)>=A(2); mask(n)=A(n)>=A(n-1);
if n>2
    mask(2:n-1)=A(2:n-1)>=A(1:n-2) & A(2:n-1)>=A(3:n);
end
Ind=find(mask);
end


function fwhm=localEstimateWindowFWHM(T,Wgt,Wsum,FreqVec,CalcMethod)
span=max(T)-min(T);
if span<=0, fwhm=median(diff(FreqVec)); return; end
dfMax=max(5/span,5*median(diff(FreqVec))); df=linspace(0,dfMax,4000).';
switch CalcMethod
    case "loop"
        p=zeros(size(df));
        for k=1:numel(df)
            ww=sum(Wgt.*exp(-2i*pi*df(k).*T))/Wsum; p(k)=abs(ww)^2;
        end
    case "matrix"
        E=exp(-2i*pi*(T*df.')); ww=(Wgt.'*E).'/Wsum; p=abs(ww).^2;
end
idx=find(p<=0.5,1,'first');
if isempty(idx)||idx==1
    fwhm=1/span;
else
    x1=df(idx-1); x2=df(idx); y1=p(idx-1); y2=p(idx);
    xh=x1+(0.5-y1)*(x2-x1)/(y2-y1); fwhm=2*xh;
end
end
