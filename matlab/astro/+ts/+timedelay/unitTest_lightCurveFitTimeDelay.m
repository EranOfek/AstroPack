function [Result, Info] = unitTest_lightCurveFitTimeDelay(Args)
% unitTest_lightCurveFitTimeDelay - Unit test of lightCurveFitTimeDelay on mock lensed-quasar data.
%
% Description:
%   Generate a mock unresolved (combined-flux) light curve of a lensed
%   quasar,
%
%       F(t) = MeanFlux + alpha1*f(t) + alpha2*f(t+Tau) + noise,
%
%   where f(t) is a stationary Gaussian red-noise process with a power-law
%   PSD, P(f) ~ f^(-Gamma), sampled at irregular (seasonal + random) epochs.
%   Then fit it with lightCurveFitTimeDelay (power-law index fixed to the
%   true value), plot the chi^2 = -2 lnL surface as a function of the time
%   delay and the flux ratio, and check that the best fit is close to the
%   true (Tau, alpha2/alpha1).
%
%   Mock data:
%     * Sampling: N epochs drawn at random within observing seasons (a
%       fraction SeasonFrac of every Period) over Span days -> non-equally
%       spaced, with seasonal gaps.
%     * Source: f(t) = sum_k sqrt(P_k df) [a_k cos(2 pi f_k t) + b_k sin(2 pi f_k t)],
%       a_k,b_k ~ N(0,1), on a fine uniform grid in [1/Span, SimHighFreq]
%       (10x oversampled). This is an exact draw of a Gaussian process
%       with the requested PSD (no periodic-boundary artifacts over the
%       data span).
%     * Variability: the combined signal alpha1 f(t)+alpha2 f(t+Tau) is
%       scaled to a total std of VarFrac*MeanFlux (default 30%).
%     * Errors: FluxErrFrac (default 3%) of the true flux, Gaussian.
%
%   Note on the flux ratio: the combined-flux likelihood is invariant to
%   r -> 1/r (and Tau -> -Tau), so only r=min(alpha2/alpha1, alpha1/alpha2)
%   in (0,1] is measurable. The surface is therefore plotted against
%   alpha2/alpha1 in (0,1]; alpha1/alpha2 = 1/r gives the identical surface.
%
% Input arguments (key/value):
%   N             - Number of epochs. Default: 500.
%   Span          - Total time span [day]. Default: 1000.
%   Period        - Season period [day]. Default: 365.
%   SeasonFrac    - Observable fraction of each season. Default: 0.67.
%   MeanFlux      - Mean flux. Default: 1.
%   FluxErrFrac   - Fractional flux error. Default: 0.03.
%   VarFrac       - Std of the combined variability / MeanFlux. Default: 0.30.
%   PowerLawInd   - PSD power-law index (fixed in the fit). Default: 2.5.
%   TimeDelay     - True time delay [day]. Default: 60.
%   FluxRatio     - True alpha2/alpha1 (<=1). Default: 0.5.
%   SimHighFreq   - Highest frequency in the simulation [1/day]. Default: 0.5.
%   FitHighFreq   - HighFreqCut used in the fit [1/day]. Default: 0.1.
%   TauGrid       - Trial time delays for the fit. Default: 4:2:300.
%   RatioGrid     - Trial alpha2/alpha1 for the fit. Default: 0.1:0.1:1.
%   TauTol        - Allowed |Tau_fit-Tau_true|. Default: NaN ->
%                   max(3*grid step, 0.1*TimeDelay).
%   RatioTol      - Allowed |r_fit-r_true|. Default: 0.2.
%   Refine        - Refine (Tau,r) continuously. Default: true.
%   Seed          - Random seed. Default: 1.
%   Plot          - Plot light curve, chi^2 surface and Tau profile.
%                   Default: true.
%   SavePlot      - If not empty, save the figure to this file. Default: ''.
%   Verbose       - Print progress and summary. Default: true.
%
% Output arguments:
%   Result        - true if the fit converged to (roughly) the true
%                   (Tau, alpha2/alpha1); false otherwise.
%   Info          - The Info structure of lightCurveFitTimeDelay, with an
%                   added field Info.Test containing the mock data, the
%                   truth, the chi^2 surface and the individual checks.
%
% Example:
%   [Result,Info] = unitTest_lightCurveFitTimeDelay;
%   [Result,Info] = unitTest_lightCurveFitTimeDelay('TimeDelay',100,'FluxRatio',0.3,'Seed',7);
%
% Author: Claude (Anthropic)

arguments
    Args.N (1,1) double {mustBeInteger,mustBePositive} = 500
    Args.Span (1,1) double {mustBePositive} = 1000
    Args.Period (1,1) double {mustBePositive} = 365
    Args.SeasonFrac (1,1) double {mustBePositive} = 0.67
    Args.MeanFlux (1,1) double {mustBePositive} = 1
    Args.FluxErrFrac (1,1) double {mustBePositive} = 0.03
    Args.VarFrac (1,1) double {mustBePositive} = 0.30
    Args.PowerLawInd (1,1) double = 2.5
    Args.TimeDelay (1,1) double {mustBePositive} = 60
    Args.FluxRatio (1,1) double {mustBePositive} = 0.2
    Args.SimHighFreq (1,1) double {mustBePositive} = 0.5
    Args.FitHighFreq (1,1) double {mustBePositive} = 0.1
    Args.TauGrid double = 4:2:300
    Args.RatioGrid double = 0.1:0.1:1
    Args.TauTol (1,1) double = NaN
    Args.RatioTol (1,1) double {mustBePositive} = 0.2
    Args.Refine (1,1) logical = true
    Args.Seed (1,1) double = 1
    Args.Plot (1,1) logical = true
    Args.SavePlot char = ''
    Args.Verbose (1,1) logical = true
end

if Args.FluxRatio>1
    error('FluxRatio must be <=1 (alpha2/alpha1 and alpha1/alpha2 are degenerate).');
end
rng(Args.Seed);

%--------------------------
% Mock sampling: seasonal gaps + random epochs (non-equally spaced)
%--------------------------
NCand = 20.*Args.N;
TCand = Args.Span.*rand(NCand,1);
Phase = mod(TCand,Args.Period)./Args.Period;
TCand = TCand(Phase < Args.SeasonFrac);
if numel(TCand) < Args.N
    error('Not enough observable epochs; increase SeasonFrac or Span.');
end
Time = sort(TCand(randperm(numel(TCand),Args.N)));

%--------------------------
% Mock red-noise source light curve (exact Gaussian process draw)
%--------------------------
TauTrue = Args.TimeDelay;
RTrue   = Args.FluxRatio;
FMin    = 1./Args.Span;
FMax    = Args.SimHighFreq;
NSimF   = ceil(10.*(FMax-FMin).*(Args.Span+TauTrue));
SimEdge = linspace(FMin,FMax,NSimF+1);
SimF    = 0.5.*(SimEdge(1:end-1)+SimEdge(2:end));
SimDf   = SimEdge(2)-SimEdge(1);
Amp     = sqrt(SimF.^(-Args.PowerLawInd).*SimDf);      % 1 x NSimF
Ca      = (randn(1,NSimF).*Amp).';
Sb      = (randn(1,NSimF).*Amp).';

Signal1 = localSource(Time,         SimF,Ca,Sb);         % alpha1 image: f(t)
Signal2 = localSource(Time+TauTrue, SimF,Ca,Sb);         % alpha2 image: f(t+Tau)
Signal  = Signal1 + RTrue.*Signal2;

% total std of the combined variability = VarFrac*MeanFlux
Scale   = Args.VarFrac.*Args.MeanFlux./std(Signal);
Alpha1  = Scale;
Alpha2  = RTrue.*Scale;
FluxTrue = Args.MeanFlux + Scale.*Signal;

FluxErr = Args.FluxErrFrac.*abs(FluxTrue);
FluxErr = max(FluxErr, 0.1.*Args.FluxErrFrac.*Args.MeanFlux);
Flux    = FluxTrue + FluxErr.*randn(Args.N,1);

%--------------------------
% Fit (power-law index fixed to the true value)
%--------------------------
if Args.Verbose
    fprintf('unitTest_lightCurveFitTimeDelay: N=%d, Span=%g d, Tau=%g d, alpha2/alpha1=%g, Gamma=%g\n', ...
        Args.N,Args.Span,TauTrue,RTrue,Args.PowerLawInd);
    fprintf('  std(var)/mean=%.3f, median err/flux=%.3f, median dt=%.2f d\n', ...
        std(FluxTrue)./mean(FluxTrue), median(FluxErr./FluxTrue), median(diff(Time)));
end
TicFit = tic;
[L, Info] = lightCurveFitTimeDelay(Time, Flux, FluxErr, ...
    'TimeDelay',   Args.TauGrid, ...
    'FluxRatio',   Args.RatioGrid, ...
    'PowerLawInd', Args.PowerLawInd, ...
    'HighFreqCut', Args.FitHighFreq, ...
    'Refine',      Args.Refine, ...
    'RefineShape', false, ...
    'Verbose',     Args.Verbose);
FitTime = toc(TicFit);

%--------------------------
% chi^2 surface  (chi^2 = -2 lnL, A and mean profiled/marginalized)
%--------------------------
TauVec = Info.TimeDelay;
RVec   = Info.FluxRatio;
LSurf  = reshape(L, numel(TauVec), numel(RVec));      % [nTau x nRatio]
Chi2   = -2.*LSurf;
Chi2Min = min(Chi2(:));
DChi2  = Chi2 - Chi2Min;
DChi2Best = -2.*Info.Best.LogLikelihood - Chi2Min;    % <=0 if refinement improved

% Delta chi^2 at the grid point nearest to the truth
[~,ItTrue] = min(abs(TauVec-TauTrue));
[~,IrTrue] = min(abs(RVec-RTrue));
DChi2AtTruth = DChi2(ItTrue,IrTrue) - min(DChi2Best,0);

%--------------------------
% Checks
%--------------------------
TauTol = Args.TauTol;
if isnan(TauTol)
    if numel(TauVec)>1
        TauTol = max(3.*median(diff(TauVec)), 0.1.*TauTrue);
    else
        TauTol = 0.1.*TauTrue;
    end
end
TauFit   = Info.Best.TimeDelay;
RFit     = Info.Best.FluxRatio;
Check = struct;
Check.Finite  = isfinite(Info.Best.LogLikelihood) && isfinite(TauFit) && isfinite(RFit);
Check.Tau     = abs(TauFit-TauTrue) <= TauTol;
Check.Ratio   = abs(RFit-RTrue)     <= Args.RatioTol;
Check.Detect  = Info.LogLR > 0;                     % informational
Result = Check.Finite && Check.Tau && Check.Ratio;

Info.Test = struct;
Info.Test.Args        = Args;
Info.Test.Time        = Time;
Info.Test.Flux        = Flux;
Info.Test.FluxErr     = FluxErr;
Info.Test.FluxTrue    = FluxTrue;
Info.Test.Alpha1      = Alpha1;
Info.Test.Alpha2      = Alpha2;
Info.Test.TrueTimeDelay = TauTrue;
Info.Test.TrueFluxRatio = RTrue;
Info.Test.FitTimeDelay  = TauFit;
Info.Test.FitFluxRatio  = RFit;
Info.Test.TauTol      = TauTol;
Info.Test.RatioTol    = Args.RatioTol;
Info.Test.Chi2        = Chi2;
Info.Test.DeltaChi2   = DChi2;
Info.Test.DeltaChi2AtTruth = DChi2AtTruth;
Info.Test.Check       = Check;
Info.Test.Result      = Result;
Info.Test.FitTime     = FitTime;

if Args.Verbose
    fprintf('  fit time %.1f s\n',FitTime);
    fprintf('  true: Tau=%7.2f  r=%5.3f\n',TauTrue,RTrue);
    fprintf('  fit : Tau=%7.2f  r=%5.3f  (%s)   2*lnLR(H1/H0)=%.2f   dChi2(truth)=%.2f\n', ...
        TauFit,RFit,Info.Best.Source,2.*Info.LogLR,DChi2AtTruth);
    if Result
        fprintf('  PASSED (|dTau|<=%.2f, |dr|<=%.2f)\n',TauTol,Args.RatioTol);
    else
        fprintf('  FAILED (Tau ok=%d, r ok=%d, finite=%d)\n',Check.Tau,Check.Ratio,Check.Finite);
    end
end

%--------------------------
% Plots
%--------------------------
if Args.Plot
    DChi2Cap = 50;                               % clip for display
    Z = min(DChi2, DChi2Cap);
    Lev = [2.30 6.18 11.83];                     % 68.3/95.4/99.7% for 2 parameters

    Fig = figure('Color','w','Position',[100 100 1500 450]);

    % (1) the mock light curve
    subplot(1,3,1);
    errorbar(Time,Flux,FluxErr,'.','Color',[0.5 0.5 0.5],'CapSize',0); hold on;
    plot(Time,FluxTrue,'k.','MarkerSize',6);
    xlabel('Time [day]'); ylabel('Combined flux');
    title(sprintf('Mock data: N=%d, \\tau=%g d, \\alpha_2/\\alpha_1=%g, \\gamma=%g', ...
        Args.N,TauTrue,RTrue,Args.PowerLawInd));
    box on;

    % (2) chi^2 surface vs Tau and flux ratio
    subplot(1,3,2);
    surf(TauVec, RVec, Z.', 'EdgeColor','none'); hold on;
    contour3(TauVec, RVec, DChi2.', Lev, 'k', 'LineWidth',1);
    plot3(TauTrue, RTrue, 0, 'wp','MarkerSize',14,'MarkerFaceColor','r');
    plot3(TauFit,  RFit,  0, 'wo','MarkerSize',9, 'MarkerFaceColor','b');
    xlabel('Time delay \tau [day]');
    ylabel('\alpha_2/\alpha_1  (\equiv (\alpha_1/\alpha_2)^{-1})');
    zlabel('\Delta\chi^2');
    title('\Delta\chi^2 = -2\Delta lnL  (contours: 2.30, 6.18, 11.8)');
    colorbar; view(2); axis tight; box on;
    legend({'\Delta\chi^2','contours','truth','best fit'},'Location','southoutside','Orientation','horizontal');

    % (3) profile over Tau
    subplot(1,3,3);
    ProfT = -2.*Info.ProfileTau;
    plot(TauVec, ProfT-min(ProfT), 'k-','LineWidth',1.2); hold on;
    yl = ylim;
    plot([TauTrue TauTrue], yl, 'r--');
    plot([TauFit TauFit],   yl, 'b:','LineWidth',1.2);
    xlabel('Time delay \tau [day]'); ylabel('\Delta\chi^2 (profiled over r)');
    if Result
        Str = 'PASSED';
    else
        Str = 'FAILED';
    end
    title(sprintf('Profile: \\tau_{fit}=%.1f, r_{fit}=%.2f  [%s]',TauFit,RFit,Str));
    legend({'profile','truth','best fit'},'Location','best');
    box on;

    if ~isempty(Args.SavePlot)
        print(Fig, Args.SavePlot, '-dpng', '-r110');
    end
end

end


%==========================================================================
function F = localSource(T, Freq, Ca, Sb)
% Evaluate the random-phase Fourier series in chunks (memory friendly).
F = zeros(numel(T),1);
Chunk = 2000;
for I1=1:Chunk:numel(Freq)
    I2 = min(I1+Chunk-1,numel(Freq));
    Ph = 2.*pi.*(T(:)*Freq(I1:I2));
    F  = F + cos(Ph)*Ca(I1:I2) + sin(Ph)*Sb(I1:I2);
end
end
