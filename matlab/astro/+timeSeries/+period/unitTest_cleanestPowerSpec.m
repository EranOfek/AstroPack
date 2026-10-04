function R = unitTest_cleanestPowerSpec(Args)
% unitTest_cleanestPowerSpec  Compare dirty, LS, CLEAN, and CLEANest spectra.
%
% R = unitTest_cleanestPowerSpec(...)
%
% Name-value arguments
%   ComplexWindow  - use pathological quasi-periodic sampling. Default true.
%   FirstPeakToSub - first peak rank for CLEAN/CLEANest. Default 1.
%   RandomSeed     - RNG seed. Default 19.
%   NoiseSigma     - Gaussian noise sigma. Default 0.45.
%   Plot           - make separate diagnostic figures. Default true.

arguments
    Args.ComplexWindow (1,1) logical = true
    Args.FirstPeakToSub (1,1) double {mustBeInteger,mustBePositive} = 1
    Args.RandomSeed (1,1) double {mustBeInteger} = 19
    Args.NoiseSigma (1,1) double {mustBeNonnegative} = 0.45
    Args.Plot (1,1) logical = true
end

rng(Args.RandomSeed);
f1=0.173; f2=0.417;
A1=1.00; A2=0.72; ph1=0.4; ph2=1.7;

if ~Args.ComplexWindow
    T0=(0:0.23:120).';
    T=T0+0.035*randn(size(T0));
    keep=~(T>31&T<42) & ~(T>76&T<89);
    keep=keep & (rand(size(T))>0.08);
    T=sort(T(keep));
else
    blockPeriod=2.0; nBlock=70;
    within=[0.00 0.08 0.17 0.29 0.42].';
    T=[];
    for ib=0:nBlock-1
        if rand<0.18, continue; end
        tt=ib*blockPeriod+within+0.012*randn(size(within));
        T=[T;tt]; %#ok<AGROW>
    end
    keep=~(T>24&T<37) & ~(T>67&T<82) & ~(T>103&T<111);
    T=T(keep);
    T=T(rand(size(T))>0.12);
    T=sort(T);
end

Ytrue=A1*sin(2*pi*f1*T+ph1)+A2*sin(2*pi*f2*T+ph2);
Y=Ytrue+Args.NoiseSigma*randn(size(T));
FreqVec=linspace(0.02,0.85,1400).';

% Direct dirty Fourier spectrum.
Y0=Y-mean(Y);
D=(Y0.'*exp(-2i*pi*(T*FreqVec.')))./numel(T);
Pdirty=abs(D(:)).^2;

% MATLAB Lomb-Scargle reference if available.
hasPlomb=exist('plomb','file')==2;
if hasPlomb
    try
        Plomb=plomb(Y,T,FreqVec,'normalized'); Plomb=Plomb(:);
    catch
        Plomb=nan(size(FreqVec)); hasPlomb=false;
    end
else
    Plomb=nan(size(FreqVec));
end

% Ordinary CLEAN for comparison.
[~,Pclean,CleanInfo]=cleanPowerSpec(T,Y,FreqVec, ...
    FirstPeakToSub=Args.FirstPeakToSub, ...
    SpectrumMethod="lombscargle", ...
    CalcMethod="loop", ...
    Gain=0.15,MaxIter=120,StopFraction=1e-3);

% Improved CLEANest-style fit.
t0=tic;
[FreqVec,Pcleanest,Info]=timeSeries.period.cleanestPowerSpec(T,Y,FreqVec, ...
    FirstPeakToSub=Args.FirstPeakToSub, ...
    SpectrumMethod="lombscargle", ...
    CalcMethod="loop", ...
    MaxComponents=10, ...
    Refine=true, ...
    StopMethod="bic", ...
    MinBICImprove=2);
RunTime=toc(t0);

% Match recovered components to injected frequencies for reporting.
rec=Info.ComponentFreq(:);
if isempty(rec)
    err1=NaN; err2=NaN;
else
    err1=min(abs(rec-f1)); err2=min(abs(rec-f2));
end

fprintf('\nunitTest_cleanestPowerSpec\n');
fprintf('  Complex window       : %d\n',Args.ComplexWindow);
fprintf('  N samples            : %d\n',numel(T));
fprintf('  Injected frequencies : %.9f, %.9f\n',f1,f2);
fprintf('  CLEANest components  : %d\n',Info.Ncomponents);
for k=1:Info.Ncomponents
    fprintf('    %2d: f=%.9f   amplitude=%.5f\n',k,Info.ComponentFreq(k),Info.ComponentAmplitude(k));
end
fprintf('  nearest |df| to f1   : %.4g\n',err1);
fprintf('  nearest |df| to f2   : %.4g\n',err2);
fprintf('  final BIC            : %.4f\n',Info.BIC);
fprintf('  run time             : %.4f s\n',RunTime);
if Info.Ntried>0
    fprintf('\n  Candidate history:\n');
    for k=1:Info.Ntried
        fprintf('    try %2d: grid f=% .7f  dBIC=% .4f  accepted=%d\n', ...
            k,Info.CandidateGridFreq(k),Info.BICImprove(k),Info.Accepted(k));
    end
end

% Normalize only for plotting.
PdirtyN=Pdirty/max(Pdirty);
PcleanN=Pclean/max(Pclean);
PcleanestN=Pcleanest/max(Pcleanest);
if hasPlomb, PlombN=Plomb/max(Plomb); else, PlombN=Plomb; end

if Args.Plot
    figure('Name','CLEANest test: sampling');
    plot(T,ones(size(T)),'.'); xlabel('Time'); ylabel('Sampling indicator');
    title(sprintf('Sampling window (ComplexWindow=%d)',Args.ComplexWindow)); grid on;

    figure('Name','CLEANest test: time series');
    plot(T,Y,'.'); hold on; plot(T,Ytrue,'-','LineWidth',1.2);
    if ~isempty(Info.ModelY), plot(T,Info.ModelY,'--','LineWidth',1.2); end
    xlabel('Time'); ylabel('Y');
    legend('Observed','Injected signal','CLEANest joint model','Location','best');
    title('Time series and final joint CLEANest model'); grid on;

    figure('Name','CLEANest test: spectra');
    plot(FreqVec,PdirtyN,'-','LineWidth',0.8); hold on;
    if hasPlomb, plot(FreqVec,PlombN,'-','LineWidth',0.9); end
    plot(FreqVec,PcleanN,'-','LineWidth',1.0);
    plot(FreqVec,PcleanestN,'-','LineWidth',1.4);
    xline(f1,'--','f_1'); xline(f2,'--','f_2');
    for k=1:numel(rec), xline(rec(k),':'); end
    xlabel('Frequency'); ylabel('Normalized power');
    if hasPlomb
        legend('Dirty Fourier','Lomb-Scargle','CLEAN','CLEANest','f_1','f_2','Location','best');
    else
        legend('Dirty Fourier','CLEAN','CLEANest','f_1','f_2','Location','best');
    end
    title('Dirty, Lomb-Scargle, CLEAN, and improved CLEANest'); grid on;

    figure('Name','CLEANest test: residual spectrum');
    plot(FreqVec,Info.ResidualPower,'-'); hold on;
    xline(f1,'--','f_1'); xline(f2,'--','f_2');
    xlabel('Frequency'); ylabel('Residual power');
    title('Residual spectrum after simultaneous CLEANest fit'); grid on;

    figure('Name','CLEANest test: BIC history');
    if Info.Ntried>0
        plot(1:Info.Ntried,Info.BICImprove,'-o'); hold on; yline(2,'--','acceptance threshold');
        xlabel('Candidate addition'); ylabel('\DeltaBIC = BIC_{old}-BIC_{trial}');
        title('CLEANest component acceptance'); grid on;
    end

    figure('Name','CLEANest test: recovered components');
    if ~isempty(rec)
        stem(rec,Info.ComponentAmplitude,'filled'); hold on;
    end
    xline(f1,'--','f_1'); xline(f2,'--','f_2');
    xlabel('Frequency'); ylabel('Fitted sinusoid amplitude');
    title('Final jointly fitted CLEANest components'); grid on;
end

R=struct;
R.T=T; R.Y=Y; R.Ytrue=Ytrue; R.FreqVec=FreqVec;
R.DirtyPower=Pdirty; R.LombScarglePower=Plomb; R.CleanPower=Pclean;
R.CleanestPower=Pcleanest; R.CleanInfo=CleanInfo; R.Info=Info;
R.InjectedFrequency=[f1 f2]; R.FrequencyError=[err1 err2];
R.RunTime=RunTime; R.HasPlomb=hasPlomb; R.Args=Args;
end
