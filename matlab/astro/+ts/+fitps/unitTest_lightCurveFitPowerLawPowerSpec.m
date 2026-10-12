function Result = unitTest_lightCurveFitPowerLawPowerSpec(Args)
% unitTest_lightCurveFitPowerLawPowerSpec - Unit tests for time-domain PSD fitting.
%
% Description:
%   Test lightCurveFitPowerLawPowerSpec using two complementary strategies:
%
%   1. Numerical likelihood test:
%      Compare the fast eigen/Woodbury implementation against a brute-force
%      N-by-N Gaussian likelihood using exactly the same covariance quadrature.
%      This is a deterministic test of the likelihood algebra.
%
%   2. Statistical recovery tests:
%      Generate several independent Gaussian-process light curves with known
%      PSDs and SUM their log likelihoods. Using an ensemble is intentional:
%      the ML PSD slope/break from one finite red-noise realization can differ
%      substantially from the generating population parameters because of
%      realization (sample/cosmic) variance. Requiring one realization to
%      recover the exact input slope is therefore not a valid unit test.
%
%   The statistical tests include:
%      a. Single power law: select the injected slope from three alternatives.
%      b. Low/high cutoff grid: verify vector-grid behavior and finiteness.
%      c. Broken power law: separately recover the break, low-frequency slope,
%         and high-frequency slope while holding the other two at their true
%         values. This isolates each broken-PSD branch without constructing an
%         unnecessarily expensive 3-D cross-product grid.
%
% Input arguments:
%   Args - Optional key/value arguments:
%       .Seed           - Random-number seed. Default: 314159.
%       .Nobs           - Number of epochs per realization. Default: 160.
%       .TimeSpan       - Time span. Default: 1200.
%       .NReal          - Number of independent realizations accumulated in
%                         the statistical likelihood tests. Default: 10.
%       .NFreqSim       - Frequency bins used to simulate the GP. Default: 2048.
%       .NFreqFit       - Frequency bins passed to the fitter. 0 means use the
%                         fitter's automatic resolution. Default: 0.
%       .Plot           - Plot ensemble likelihood profiles. Default: false.
%       .Verbose        - Print summary. Default: true.
%       .ThrowOnFailure - Throw an error on failure. Default: true.
%
% Output arguments:
%   Result - Structure containing numerical-equivalence and statistical tests.
%
% Example:
%   Result = unitTest_lightCurveFitPowerLawPowerSpec;
%
%   Result = unitTest_lightCurveFitPowerLawPowerSpec('Plot',true,...
%                                                     'ThrowOnFailure',false);
%
% See also: lightCurveFitPowerLawPowerSpec
%
% Author: ChatGPT

arguments
    Args.Seed (1,1) double {mustBeInteger} = 314159
    Args.Nobs (1,1) double {mustBeInteger,mustBePositive} = 160
    Args.TimeSpan (1,1) double {mustBePositive} = 1200
    Args.NReal (1,1) double {mustBeInteger,mustBePositive} = 10
    Args.NFreqSim (1,1) double {mustBeInteger,mustBePositive} = 2048
    Args.NFreqFit (1,1) double = 0
    Args.Plot (1,1) logical = false
    Args.Verbose (1,1) logical = true
    Args.ThrowOnFailure (1,1) logical = true
end

if Args.Nobs<80
    error('Nobs should be at least 80 for the statistical recovery tests.');
end
if Args.NReal<3
    error('NReal should be at least 3 for the statistical recovery tests.');
end
if Args.Seed<0
    error('Seed must be non-negative.');
end
if Args.NFreqFit<0 || Args.NFreqFit~=round(Args.NFreqFit)
    error('NFreqFit must be a non-negative integer; use 0 for automatic.');
end

rng(Args.Seed,'twister');

%=========================================================================%%
% Test 1: deterministic equivalence with brute-force full covariance.
%=========================================================================%%
Nsmall = 70;
Tsmall = sort(300.*rand(Nsmall,1));
Esmall = 0.025.*(0.8+0.4.*rand(Nsmall,1));
Ysmall = 1.3 + 0.08.*randn(Nsmall,1);

FlowCore = 0.025;
FhighCore = 0.16;
GammaCore = 1.8;
FrefCore = sqrt(FlowCore.*FhighCore);

[Lcore,Icore] = ts.fitps.lightCurveFitPowerLawPowerSpec(Tsmall,Ysmall,Esmall,...
    'LowFreqCut',FlowCore,...
    'HighFreqCut',FhighCore,...
    'BreakFreq',NaN,...
    'PowerLawInd',GammaCore,...
    'FRef',FrefCore,...
    'NFreq',Args.NFreqFit);

[Ldirect,Adirect,Mudirect] = localDirectSinglePL(Tsmall,Ysmall,Esmall,...
    FlowCore,FhighCore,GammaCore,FrefCore,Icore.Frequency,Icore.FrequencyStep);

CoreDeltaL = abs(Lcore(1)-Ldirect);
CoreDeltaA = abs(Icore.Best.PowerSpecNorm-Adirect)./max(abs(Adirect),realmin);
CoreDeltaMu = abs(Icore.Best.MeanFlux-Mudirect);

PassCore = CoreDeltaL<1e-6 && CoreDeltaA<5e-4 && CoreDeltaMu<1e-6;

%=========================================================================%%
% Test 1b: deterministic equivalence for the BASIS-SPACE branch (R<=N).
% The Test-1 configuration exercises only the data-space branch (R>N). Here a
% larger N with a small frequency grid forces R=2*nActive<=N, so the basis-
% space eigen path is validated against the same brute-force covariance.
%=========================================================================%%
Nbasis = 200;
Tbasis = sort(300.*rand(Nbasis,1));
Ebasis = 0.02.*(0.8+0.4.*rand(Nbasis,1));
Ybasis = 1.2 + 0.05.*randn(Nbasis,1);
NFbasis = 40;   % 2*nActive <= Nbasis guarantees the basis-space branch.

[Lbasis,Ibasis] = ts.fitps.lightCurveFitPowerLawPowerSpec(Tbasis,Ybasis,Ebasis,...
    'LowFreqCut',FlowCore,...
    'HighFreqCut',FhighCore,...
    'BreakFreq',NaN,...
    'PowerLawInd',GammaCore,...
    'FRef',FrefCore,...
    'NFreq',NFbasis,...
    'WarnUnderResolved',false);

[LbasisDirect,~,~] = localDirectSinglePL(Tbasis,Ybasis,Ebasis,...
    FlowCore,FhighCore,GammaCore,FrefCore,Ibasis.Frequency,Ibasis.FrequencyStep);

BasisDeltaL = abs(Lbasis(1)-LbasisDirect);
PassBasis = BasisDeltaL<1e-6 && double(Ibasis.Best.EffectiveRank)<=Nbasis;

%=========================================================================%%
% Test 1c: deterministic equivalence for the BROKEN power-law covariance.
% The broken path is otherwise only checked by statistical recovery.
%=========================================================================%%
FlowBrk = 0.010; FhighBrk = 0.200; FbreakBrk = 0.060;
Gamma1Brk = 1.0; Gamma2Brk = 2.8; FrefBrk = FbreakBrk;

[Lbrk,Ibrk] = ts.fitps.lightCurveFitPowerLawPowerSpec(Tsmall,Ysmall,Esmall,...
    'LowFreqCut',FlowBrk,...
    'HighFreqCut',FhighBrk,...
    'BreakFreq',FbreakBrk,...
    'PowerLawInd',Gamma1Brk,...
    'PowerLawInd2',Gamma2Brk,...
    'FRef',FrefBrk,...
    'NFreq',Args.NFreqFit,...
    'WarnUnderResolved',false);

[LbrkDirect,~,~] = localDirectBrokenPL(Tsmall,Ysmall,Esmall,...
    FlowBrk,FhighBrk,FbreakBrk,Gamma1Brk,Gamma2Brk,FrefBrk,...
    Ibrk.Frequency,Ibrk.FrequencyStep);

BrokenDirectDeltaL = abs(Lbrk(1)-LbrkDirect);
PassBrokenDirect = BrokenDirectDeltaL<1e-6;

%=========================================================================%%
% Common irregular times for statistical tests.
%=========================================================================%%
Nobs = Args.Nobs;
Time = sort(Args.TimeSpan.*rand(Nobs,1));
MeanFlux = 1.0;

%=========================================================================%%
% Test 2: single power law, ensemble likelihood recovery.
%=========================================================================%%
P1 = struct;
P1.LowFreqCut = 0.030;
P1.HighFreqCut = 0.180;
P1.PowerLawInd = 1.75;
P1.RMS = 0.12;
P1.FRef = sqrt(P1.LowFreqCut.*P1.HighFreqCut);

GammaGrid = [1.25,1.75,2.25];
SumLSingle = zeros(size(GammaGrid));
SingleNormRatio = NaN(Args.NReal,1);

FluxOne = [];
ErrOne = [];
InfoOne = [];

for Ireal=1:Args.NReal
    [Signal,Atrue] = localSimulatePSD(Time,P1.LowFreqCut,P1.HighFreqCut,...
        NaN,P1.PowerLawInd,NaN,P1.FRef,P1.RMS,Args.NFreqSim);

    FluxErr = 0.015.*(0.8+0.4.*rand(Nobs,1));
    Flux = MeanFlux + Signal + FluxErr.*randn(Nobs,1);

    [Li,Infoi] = ts.fitps.lightCurveFitPowerLawPowerSpec(Time,Flux,FluxErr,...
        'LowFreqCut',P1.LowFreqCut,...
        'HighFreqCut',P1.HighFreqCut,...
        'BreakFreq',NaN,...
        'PowerLawInd',GammaGrid,...
        'FRef',P1.FRef,...
        'NFreq',Args.NFreqFit);

    SumLSingle = SumLSingle + reshape(Li,1,[]);

    [~,ItrueGamma] = min(abs(GammaGrid-P1.PowerLawInd));
    NormAtTrue = reshape(Infoi.PowerSpecNorm,1,[]);
    SingleNormRatio(Ireal) = NormAtTrue(ItrueGamma)./Atrue;

    if Ireal==1
        FluxOne = Flux;
        ErrOne = FluxErr;
        InfoOne = Infoi;
    end
end

[~,IBestSingle] = max(SumLSingle);
BestGamma = GammaGrid(IBestSingle);
PassSingle = BestGamma==P1.PowerLawInd;

%=========================================================================%%
% Test 3: vector low/high cutoff grid behavior.
%=========================================================================%%
LowGrid = P1.LowFreqCut.*[0.7,1.0,1.4];
HighGrid = P1.HighFreqCut.*[0.75,1.0,1.20];

[Lcut,Icut] = ts.fitps.lightCurveFitPowerLawPowerSpec(Time,FluxOne,ErrOne,...
    'LowFreqCut',LowGrid,...
    'HighFreqCut',HighGrid,...
    'BreakFreq',NaN,...
    'PowerLawInd',P1.PowerLawInd,...
    'FRef',P1.FRef,...
    'NFreq',Args.NFreqFit);

PassCutSize = size(Lcut,1)==numel(LowGrid) && ...
              size(Lcut,2)==numel(HighGrid) && ...
              size(Lcut,3)==1 && size(Lcut,4)==1;
PassCutFinite = any(isfinite(Lcut(:))) && isfinite(Icut.Best.LogLikelihood);
PassCut = PassCutSize && PassCutFinite;

%=========================================================================%%
% Test 4: broken power law. Recover each axis in ensemble likelihood.
% Use a substantial frequency lever arm below the break.  With a very
% narrow low-frequency branch (e.g. 0.03--0.075), Gamma1 is intrinsically
% weakly constrained after profiling the PSD normalization and a finite
% ensemble can legitimately prefer a neighboring slope.
%=========================================================================%%
P2 = struct;
P2.LowFreqCut = 0.010;
P2.HighFreqCut = 0.200;
P2.BreakFreq = 0.075;
P2.PowerLawInd1 = 1.0;
P2.PowerLawInd2 = 2.8;
P2.RMS = 0.14;
P2.FRef = P2.BreakFreq;

BreakGrid = [0.045,0.075,0.120];
Gamma1Grid = [0.5,1.0,1.5];
Gamma2Grid = [2.0,2.8,3.6];

SumLBreak = zeros(size(BreakGrid));
SumLGamma1 = zeros(size(Gamma1Grid));
SumLGamma2 = zeros(size(Gamma2Grid));
BrokenNormRatio = NaN(Args.NReal,1);

for Ireal=1:Args.NReal
    [Signal,Atrue] = localSimulatePSD(Time,P2.LowFreqCut,P2.HighFreqCut,...
        P2.BreakFreq,P2.PowerLawInd1,P2.PowerLawInd2,P2.FRef,P2.RMS,Args.NFreqSim);

    FluxErr = 0.012.*(0.8+0.4.*rand(Nobs,1));
    Flux = MeanFlux + Signal + FluxErr.*randn(Nobs,1);

    % Break-frequency scan with both slopes fixed to their true values.
    [Lb,~] = ts.fitps.lightCurveFitPowerLawPowerSpec(Time,Flux,FluxErr,...
        'LowFreqCut',P2.LowFreqCut,...
        'HighFreqCut',P2.HighFreqCut,...
        'BreakFreq',BreakGrid,...
        'PowerLawInd',P2.PowerLawInd1,...
        'PowerLawInd2',P2.PowerLawInd2,...
        'FRef',P2.FRef,...
        'NFreq',Args.NFreqFit);
    SumLBreak = SumLBreak + reshape(Lb,1,[]);

    % Low-frequency slope scan with break and high-frequency slope fixed.
    [Lg1,~] = ts.fitps.lightCurveFitPowerLawPowerSpec(Time,Flux,FluxErr,...
        'LowFreqCut',P2.LowFreqCut,...
        'HighFreqCut',P2.HighFreqCut,...
        'BreakFreq',P2.BreakFreq,...
        'PowerLawInd',Gamma1Grid,...
        'PowerLawInd2',P2.PowerLawInd2,...
        'FRef',P2.FRef,...
        'NFreq',Args.NFreqFit);
    SumLGamma1 = SumLGamma1 + reshape(Lg1,1,[]);

    % High-frequency slope scan with break and low-frequency slope fixed.
    [Lg2,Ig2] = ts.fitps.lightCurveFitPowerLawPowerSpec(Time,Flux,FluxErr,...
        'LowFreqCut',P2.LowFreqCut,...
        'HighFreqCut',P2.HighFreqCut,...
        'BreakFreq',P2.BreakFreq,...
        'PowerLawInd',P2.PowerLawInd1,...
        'PowerLawInd2',Gamma2Grid,...
        'FRef',P2.FRef,...
        'NFreq',Args.NFreqFit);
    SumLGamma2 = SumLGamma2 + reshape(Lg2,1,[]);

    [~,ItrueG2] = min(abs(Gamma2Grid-P2.PowerLawInd2));
    NormAtTrue = reshape(Ig2.PowerSpecNorm,1,[]);
    BrokenNormRatio(Ireal) = NormAtTrue(ItrueG2)./Atrue;
end

[~,IBreak] = max(SumLBreak);
[~,IGamma1] = max(SumLGamma1);
[~,IGamma2] = max(SumLGamma2);

BestBreak = BreakGrid(IBreak);
BestGamma1 = Gamma1Grid(IGamma1);
BestGamma2 = Gamma2Grid(IGamma2);

PassBrokenBreak = BestBreak==P2.BreakFreq;
PassBrokenGamma1 = BestGamma1==P2.PowerLawInd1;
PassBrokenGamma2 = BestGamma2==P2.PowerLawInd2;
PassBroken = PassBrokenBreak && PassBrokenGamma1 && PassBrokenGamma2;

%=========================================================================%%
% Collect results.
%=========================================================================%%
Result = struct;
Result.Seed = Args.Seed;
Result.NReal = Args.NReal;
Result.Time = Time;

Result.Core = struct;
Result.Core.LogLikelihoodFast = Lcore(1);
Result.Core.LogLikelihoodDirect = Ldirect;
Result.Core.DeltaLogLikelihood = CoreDeltaL;
Result.Core.PowerSpecNormFast = Icore.Best.PowerSpecNorm;
Result.Core.PowerSpecNormDirect = Adirect;
Result.Core.RelativeNormDifference = CoreDeltaA;
Result.Core.MeanFast = Icore.Best.MeanFlux;
Result.Core.MeanDirect = Mudirect;
Result.Core.MeanDifference = CoreDeltaMu;
Result.Core.DataSpacePass = PassCore;
Result.Core.BasisBranchDeltaLogLikelihood = BasisDeltaL;
Result.Core.BasisBranchPass = PassBasis;
Result.Core.BrokenDirectDeltaLogLikelihood = BrokenDirectDeltaL;
Result.Core.BrokenDirectPass = PassBrokenDirect;
Result.Core.Pass = PassCore && PassBasis && PassBrokenDirect;

Result.Single = struct;
Result.Single.True = P1;
Result.Single.GammaGrid = GammaGrid;
Result.Single.SumLogLikelihood = SumLSingle;
Result.Single.BestPowerLawInd = BestGamma;
Result.Single.NormRatioAtTrueSlope = SingleNormRatio;
Result.Single.MedianNormRatio = median(SingleNormRatio,'omitnan');
Result.Single.FirstRealizationInfo = InfoOne;
Result.Single.Pass = PassSingle;

Result.CutoffScan = struct;
Result.CutoffScan.LowFreqCut = LowGrid;
Result.CutoffScan.HighFreqCut = HighGrid;
Result.CutoffScan.L = Lcut;
Result.CutoffScan.Info = Icut;
Result.CutoffScan.PassSize = PassCutSize;
Result.CutoffScan.PassFinite = PassCutFinite;
Result.CutoffScan.Pass = PassCut;

Result.Broken = struct;
Result.Broken.True = P2;
Result.Broken.BreakGrid = BreakGrid;
Result.Broken.Gamma1Grid = Gamma1Grid;
Result.Broken.Gamma2Grid = Gamma2Grid;
Result.Broken.SumLogLikelihoodBreak = SumLBreak;
Result.Broken.SumLogLikelihoodGamma1 = SumLGamma1;
Result.Broken.SumLogLikelihoodGamma2 = SumLGamma2;
Result.Broken.BestBreakFreq = BestBreak;
Result.Broken.BestPowerLawInd1 = BestGamma1;
Result.Broken.BestPowerLawInd2 = BestGamma2;
Result.Broken.NormRatioAtTrueModel = BrokenNormRatio;
Result.Broken.MedianNormRatio = median(BrokenNormRatio,'omitnan');
Result.Broken.PassBreak = PassBrokenBreak;
Result.Broken.PassPowerLawInd1 = PassBrokenGamma1;
Result.Broken.PassPowerLawInd2 = PassBrokenGamma2;
Result.Broken.Pass = PassBroken;

Result.Pass = Result.Core.Pass && Result.Single.Pass && ...
              Result.CutoffScan.Pass && Result.Broken.Pass;

%=========================================================================%%
% Print summary.
%=========================================================================%%
if Args.Verbose
    fprintf('\nunitTest_lightCurveFitPowerLawPowerSpec\n');
    fprintf('---------------------------------------\n');
    fprintf('Direct-vs-fast likelihood (data-space, single PL): Delta lnL=%9.3g, Delta A/A=%9.3g : %s\n',...
        CoreDeltaL,CoreDeltaA,localPassStr(PassCore));
    fprintf('Direct-vs-fast likelihood (basis-space branch)   : Delta lnL=%9.3g : %s\n',...
        BasisDeltaL,localPassStr(PassBasis));
    fprintf('Direct-vs-fast likelihood (broken power law)      : Delta lnL=%9.3g : %s\n',...
        BrokenDirectDeltaL,localPassStr(PassBrokenDirect));
    fprintf('Single PL ensemble (%d realizations): gamma true=%4.2f, fit=%4.2f : %s\n',...
        Args.NReal,P1.PowerLawInd,BestGamma,localPassStr(Result.Single.Pass));
    fprintf('Cutoff-grid plumbing: %s\n',localPassStr(Result.CutoffScan.Pass));
    fprintf(['Broken PL ensemble (%d realizations): fb true=%6.3f fit=%6.3f; ',...
             'gamma1 true=%3.1f fit=%3.1f; gamma2 true=%3.1f fit=%3.1f : %s\n'],...
        Args.NReal,P2.BreakFreq,BestBreak,P2.PowerLawInd1,BestGamma1,...
        P2.PowerLawInd2,BestGamma2,localPassStr(Result.Broken.Pass));
    fprintf('Median Afit/Atrue: single=%6.3f, broken=%6.3f\n',...
        Result.Single.MedianNormRatio,Result.Broken.MedianNormRatio);
    fprintf('Overall: %s\n\n',localPassStr(Result.Pass));
end

if Args.Plot
    localPlotResults(GammaGrid,SumLSingle,P1.PowerLawInd,...
        BreakGrid,SumLBreak,P2.BreakFreq,...
        Gamma1Grid,SumLGamma1,P2.PowerLawInd1,...
        Gamma2Grid,SumLGamma2,P2.PowerLawInd2);
end

if Args.ThrowOnFailure && ~Result.Pass
    error('unitTest_lightCurveFitPowerLawPowerSpec:Failed',...
        ['One or more PSD-fitting tests failed. Rerun with ',...
         'ThrowOnFailure=false and Plot=true and inspect Result.']);
end

end


function [Signal,ANorm] = localSimulatePSD(Time,FLow,FHigh,FBreak,Gamma1,Gamma2,FRef,RMS,NFreq)
% Simulate a zero-mean Gaussian process from a continuous one-sided PSD.
%
% The coefficients obey
%   s(t)=sum sqrt(P(fk)*df) [a_k cos(2*pi*fk*t)+b_k sin(2*pi*fk*t)],
% with a_k,b_k independent standard normal variables, which gives
%   C(dt)=sum P(fk)*df*cos(2*pi*fk*dt).

Edges = linspace(FLow,FHigh,NFreq+1);
Freq = 0.5.*(Edges(1:end-1)+Edges(2:end));
dFreq = diff(Edges);

if isnan(FBreak)
    Shape = (Freq./FRef).^(-Gamma1);
else
    Shape = zeros(size(Freq));
    LowSide = Freq<=FBreak;
    Shape(LowSide) = (Freq(LowSide)./FBreak).^(-Gamma1);
    Shape(~LowSide) = (Freq(~LowSide)./FBreak).^(-Gamma2);

    if FRef<=FBreak
        RefShape = (FRef./FBreak).^(-Gamma1);
    else
        RefShape = (FRef./FBreak).^(-Gamma2);
    end
    Shape = Shape./RefShape;
end

VarUnit = sum(Shape.*dFreq);
ANorm = RMS.^2./VarUnit;
Weight = sqrt(ANorm.*Shape.*dFreq);

A = randn(numel(Freq),1);
B = randn(numel(Freq),1);
Phase = 2.*pi.*(Time(:)*Freq);
Signal = cos(Phase)*(Weight(:).*A) + sin(Phase)*(Weight(:).*B);

end


function [LogL,Abest,Mubest] = localDirectSinglePL(Time,Flux,FluxErr,FLow,FHigh,Gamma,FRef,Freq,dFreq)
% Brute-force N-by-N likelihood used only for the deterministic unit test.

Active = Freq>=FLow & Freq<=FHigh;
f = Freq(Active);
df = dFreq(Active);
Shape = (f./FRef).^(-Gamma);

TT = Time-mean(Time);
Phase = 2.*pi.*(TT*f);
W = sqrt(Shape.*df);
B = [cos(Phase).*W, sin(Phase).*W];
S = B*B.';
S = 0.5.*(S+S.');

VarData = var(Flux,1);
VarNoise = mean(FluxErr.^2);
VarIntrinsicGuess = max(VarData-VarNoise,0.01.*max(VarData,realmin));
S0 = sum(Shape.*df);
Aguess = max(VarIntrinsicGuess./S0,realmin('double'));

Fact = 1e8;
Amin = max(Aguess./Fact,realmin('double'));
Amax = Aguess.*Fact;

Obj = @(LogA) localDirectNLL(exp(LogA),S,Flux,FluxErr);
Opt = optimset('Display','off','TolX',1e-4);
[LogAbest,NLLbest] = fminbnd(Obj,log(Amin),log(Amax),Opt);
Abest = exp(LogAbest);
[~,Mubest] = localDirectNLL(Abest,S,Flux,FluxErr);

[NLLzero,Muzero] = localDirectNLL(0,S,Flux,FluxErr);
if NLLzero<NLLbest
    NLLbest = NLLzero;
    Abest = 0;
    Mubest = Muzero;
end

LogL = -NLLbest;

end


function [LogL,Abest,Mubest] = localDirectBrokenPL(Time,Flux,FluxErr,FLow,FHigh,FBreak,Gamma1,Gamma2,FRef,Freq,dFreq)
% Brute-force N-by-N likelihood for a continuous broken power law, used only
% for the deterministic unit test. Mirrors localDirectSinglePL.

Active = Freq>=FLow & Freq<=FHigh;
f = Freq(Active);
df = dFreq(Active);

Shape = zeros(size(f));
LowSide = f<=FBreak;
Shape(LowSide)  = (f(LowSide)./FBreak).^(-Gamma1);
Shape(~LowSide) = (f(~LowSide)./FBreak).^(-Gamma2);
if FRef<=FBreak
    RefShape = (FRef./FBreak).^(-Gamma1);
else
    RefShape = (FRef./FBreak).^(-Gamma2);
end
Shape = Shape./RefShape;

TT = Time-mean(Time);
Phase = 2.*pi.*(TT*f);
W = sqrt(Shape.*df);
B = [cos(Phase).*W, sin(Phase).*W];
S = B*B.';
S = 0.5.*(S+S.');

VarData = var(Flux,1);
VarNoise = mean(FluxErr.^2);
VarIntrinsicGuess = max(VarData-VarNoise,0.01.*max(VarData,realmin));
S0 = sum(Shape.*df);
Aguess = max(VarIntrinsicGuess./S0,realmin('double'));

Fact = 1e8;
Amin = max(Aguess./Fact,realmin('double'));
Amax = Aguess.*Fact;

Obj = @(LogA) localDirectNLL(exp(LogA),S,Flux,FluxErr);
Opt = optimset('Display','off','TolX',1e-4);
[LogAbest,NLLbest] = fminbnd(Obj,log(Amin),log(Amax),Opt);
Abest = exp(LogAbest);
[~,Mubest] = localDirectNLL(Abest,S,Flux,FluxErr);

[NLLzero,Muzero] = localDirectNLL(0,S,Flux,FluxErr);
if NLLzero<NLLbest
    NLLbest = NLLzero;
    Abest = 0;
    Mubest = Muzero;
end

LogL = -NLLbest;

end


function [NLL,Mu] = localDirectNLL(A,S,Y,E)
K = A.*S + diag(E.^2);
K = 0.5.*(K+K.');

[L,p] = chol(K,'lower');
if p~=0
    NLL = Inf;
    Mu = NaN;
    return;
end

One = ones(size(Y));
KiY = L'\(L\Y);
KiOne = L'\(L\One);
Den = One.'*KiOne;
if ~(isfinite(Den) && Den>0)
    NLL = Inf;
    Mu = NaN;
    return;
end

Mu = (One.'*KiY)./Den;
R = Y-Mu;
Z = L\R;
Quad = Z.'*Z;
LogDet = 2.*sum(log(diag(L)));
NLL = 0.5.*(Quad+LogDet+numel(Y).*log(2.*pi));
end


function localPlotResults(GammaGrid,LSingle,GammaTrue,...
                          BreakGrid,LBreak,BreakTrue,...
                          Gamma1Grid,LGamma1,Gamma1True,...
                          Gamma2Grid,LGamma2,Gamma2True)
figure;
plot(GammaGrid,LSingle-max(LSingle),'o-');
hold on;
xline(GammaTrue,'--');
xlabel('Power-law index');
ylabel('Ensemble \Delta ln L');
title('Single power-law recovery');
grid on;

figure;
plot(BreakGrid,LBreak-max(LBreak),'o-');
hold on;
xline(BreakTrue,'--');
xlabel('Break frequency');
ylabel('Ensemble \Delta ln L');
title('Broken power law: break recovery');
grid on;

figure;
plot(Gamma1Grid,LGamma1-max(LGamma1),'o-');
hold on;
xline(Gamma1True,'--');
xlabel('Low-frequency power-law index');
ylabel('Ensemble \Delta ln L');
title('Broken power law: low-frequency slope recovery');
grid on;

figure;
plot(Gamma2Grid,LGamma2-max(LGamma2),'o-');
hold on;
xline(Gamma2True,'--');
xlabel('High-frequency power-law index');
ylabel('Ensemble \Delta ln L');
title('Broken power law: high-frequency slope recovery');
grid on;
end


function Str = localPassStr(Flag)
if Flag
    Str = 'PASS';
else
    Str = 'FAIL';
end
end
