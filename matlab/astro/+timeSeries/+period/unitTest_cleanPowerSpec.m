function R = unitTest_cleanPowerSpec(Args)
% unitTest_cleanPowerSpec  Test CLEAN with Fourier/LS and loop/matrix modes.
%
% R = unitTest_cleanPowerSpec(...)
%
% Name-value arguments
%   ComplexWindow   - pathological cadence. Default true.
%   FirstPeakToSub  - first CLEAN peak rank. Default 1.
%   SpectrumMethod  - "fourier" or "lombscargle". Default "fourier".
%   CalcMethod      - "loop" or "matrix". Default "loop".
%   CompareCalc     - also run the other CalcMethod and compare. Default true.
%   RandomSeed      - RNG seed. Default 19.
%   NoiseSigma      - Gaussian noise sigma. Default 0.45.
%   Gain            - CLEAN gain. Default 0.15.
%   MaxIter         - CLEAN iterations. Default 120.
%   Plot            - diagnostic figures. Default true.

arguments
    Args.ComplexWindow (1,1) logical = true
    Args.FirstPeakToSub (1,1) double {mustBeInteger,mustBePositive} = 1
    Args.SpectrumMethod (1,1) string = "fourier"
    Args.CalcMethod (1,1) string = "loop"
    Args.CompareCalc (1,1) logical = true
    Args.RandomSeed (1,1) double {mustBeInteger} = 19
    Args.NoiseSigma (1,1) double {mustBeNonnegative} = 0.45
    Args.Gain (1,1) double = 0.15
    Args.MaxIter (1,1) double {mustBeInteger,mustBePositive} = 120
    Args.Plot (1,1) logical = true
end

rng(Args.RandomSeed);
f1=0.173; f2=0.417;
A1=1.00; A2=0.72; ph1=0.4; ph2=1.7;

if ~Args.ComplexWindow
    T0=(0:0.23:120).';
    T=T0+0.035.*randn(size(T0));
    keep=~(T>31&T<42) & ~(T>76&T<89);
    keep=keep & (rand(size(T))>0.08);
    T=sort(T(keep));
else
    blockPeriod=2.0; nBlock=70;
    within=[0.00 0.08 0.17 0.29 0.42].';
    T=[];
    for ib=0:nBlock-1
        if rand<0.18, continue; end
        tt=ib.*blockPeriod+within+0.012.*randn(size(within));
        T=[T;tt]; %#ok<AGROW>
    end
    keep=~(T>24&T<37) & ~(T>67&T<82) & ~(T>103&T<111);
    T=T(keep);
    T=T(rand(size(T))>0.12);
    T=sort(T);
end

Ytrue=A1.*sin(2*pi*f1.*T+ph1)+A2.*sin(2*pi*f2.*T+ph2);
Y=Ytrue+Args.NoiseSigma.*randn(size(T));
FreqVec=linspace(0.02,0.85,1400).';

% Direct Fourier spectrum for reference.
Y0=Y-mean(Y);
D=(Y0.'*exp(-2i*pi.*(T*FreqVec.')))./numel(T);
Pdirty=abs(D(:)).^2;

% MATLAB Lomb-Scargle for external reference when available.
hasPlomb=exist('plomb','file')==2;
if hasPlomb
    try
        Plomb=plomb(Y,T,FreqVec,'normalized');
        Plomb=Plomb(:);
    catch
        Plomb=nan(size(FreqVec));
        hasPlomb=false;
    end
else
    Plomb=nan(size(FreqVec));
end

% Requested CLEAN mode.
tMain=tic;
[FreqVec,Pclean,Info]=timeSeries.period.cleanPowerSpec(T,Y,FreqVec, ...
    FirstPeakToSub=Args.FirstPeakToSub, ...
    SpectrumMethod=Args.SpectrumMethod, ...
    CalcMethod=Args.CalcMethod, ...
    Gain=Args.Gain, ...
    MaxIter=Args.MaxIter, ...
    StopFraction=1e-3);
MainTime=toc(tMain);

% Optional loop-vs-matrix equivalence/timing check.
OtherPower=[]; OtherInfo=[]; OtherTime=NaN; RelDiff=NaN;
if Args.CompareCalc
    if lower(Args.CalcMethod)=="loop"
        OtherMethod="matrix";
    else
        OtherMethod="loop";
    end
    tt=tic;
    [~,OtherPower,OtherInfo]=timeSeries.period.cleanPowerSpec(T,Y,FreqVec, ...
        FirstPeakToSub=Args.FirstPeakToSub, ...
        SpectrumMethod=Args.SpectrumMethod, ...
        CalcMethod=OtherMethod, ...
        Gain=Args.Gain, ...
        MaxIter=Args.MaxIter, ...
        StopFraction=1e-3);
    OtherTime=toc(tt);
    RelDiff=max(abs(Pclean-OtherPower))./max(max(abs(Pclean)),eps);
else
    OtherMethod="";
end

% Normalize for visual comparison only.
PdirtyN=Pdirty./max(Pdirty);
PcleanN=Pclean./max(Pclean);
if hasPlomb, PlombN=Plomb./max(Plomb); else, PlombN=Plomb; end

fprintf('\nunitTest_cleanPowerSpec\n');
fprintf('  Complex window          : %d\n',Args.ComplexWindow);
fprintf('  N samples               : %d\n',numel(T));
fprintf('  Injected f1 / f2        : %.6f / %.6f\n',f1,f2);
fprintf('  SpectrumMethod          : %s\n',Info.SpectrumMethod);
fprintf('  CalcMethod              : %s\n',Info.CalcMethod);
fprintf('  FirstPeakToSub          : %d\n',Args.FirstPeakToSub);
fprintf('  First CLEAN subtraction : f=%.6f\n',Info.FirstPeakFrequency);
fprintf('  CLEAN iterations        : %d\n',Info.Niter);
fprintf('  Total run time          : %.4f s\n',MainTime);
fprintf('  Spectrum eval total     : %.4f s\n',Info.SpectrumTimeTotal);
if Args.CompareCalc
    fprintf('  Other CalcMethod        : %s\n',OtherMethod);
    fprintf('  Other run time          : %.4f s\n',OtherTime);
    fprintf('  Max relative difference : %.3g\n',RelDiff);
end

if Args.Plot
    figure('Name','CLEAN test: sampling');
    plot(T,ones(size(T)),'.');
    xlabel('Time'); ylabel('Sampling indicator');
    title(sprintf('Sampling window (ComplexWindow=%d)',Args.ComplexWindow)); grid on;

    figure('Name','CLEAN test: time series');
    plot(T,Y,'.-'); hold on; plot(T,Ytrue,'-','LineWidth',1.2);
    xlabel('Time'); ylabel('Y');
    legend('Observed','Noise-free injected signal','Location','best');
    title('Unevenly sampled time series'); grid on;

    figure('Name','CLEAN test: power spectra');
    plot(FreqVec,PdirtyN,'-','LineWidth',1.0); hold on;
    if hasPlomb, plot(FreqVec,PlombN,'-','LineWidth',1.0); end
    plot(FreqVec,PcleanN,'-','LineWidth',1.4);
    xline(f1,'--','f_1'); xline(f2,'--','f_2');
    xlabel('Frequency'); ylabel('Normalized power');
    if hasPlomb
        legend('Dirty Fourier','MATLAB Lomb-Scargle','CLEAN','f_1','f_2','Location','best');
    else
        legend('Dirty Fourier','CLEAN','f_1','f_2','Location','best');
    end
    title(sprintf('CLEAN: %s spectrum, %s calculation',Info.SpectrumMethod,Info.CalcMethod));
    grid on;

    df=linspace(-1.2,1.2,4000).';
    W=(ones(size(T)).'*exp(-2i*pi.*(T*df.')))./numel(T);
    figure('Name','CLEAN test: spectral window');
    plot(df,abs(W(:)).^2,'-');
    xlabel('\Delta f'); ylabel('|W(\Delta f)|^2'); title('Sampling spectral window'); grid on;

    figure('Name','CLEAN test: residual convergence');
    plot(1:Info.Niter,Info.ResidualMax,'-o','MarkerSize',3);
    xlabel('CLEAN iteration'); ylabel('Maximum residual spectral amplitude');
    title('CLEAN residual convergence'); grid on;

    figure('Name','CLEAN test: selected components');
    stem(1:Info.Niter,Info.ChosenFreq,'.'); hold on;
    yline(f1,'--','f_1'); yline(f2,'--','f_2');
    xlabel('CLEAN iteration'); ylabel('Selected frequency');
    title('Frequency selected at each CLEAN iteration'); grid on;

    if Args.CompareCalc
        figure('Name','CLEAN test: loop vs matrix');
        plot(FreqVec,Pclean./max(Pclean),'-','LineWidth',1.2); hold on;
        plot(FreqVec,OtherPower./max(OtherPower),'--','LineWidth',1.0);
        xlabel('Frequency'); ylabel('Normalized CLEAN power');
        legend(Args.CalcMethod,OtherMethod,'Location','best');
        title(sprintf('Loop/matrix agreement: max rel. diff = %.3g',RelDiff)); grid on;
    end
end

R=struct;
R.T=T; R.Y=Y; R.Ytrue=Ytrue; R.FreqVec=FreqVec;
R.DirtyPower=Pdirty; R.CleanPower=Pclean; R.LombScarglePower=Plomb;
R.Info=Info; R.HasPlomb=hasPlomb; R.InjectedFrequency=[f1 f2]; R.Args=Args;
R.MainTime=MainTime; R.OtherCalcMethod=OtherMethod; R.OtherPower=OtherPower;
R.OtherInfo=OtherInfo; R.OtherTime=OtherTime; R.LoopMatrixRelativeDifference=RelDiff;
end
