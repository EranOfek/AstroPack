function [FreqVec, Power, Info] = cleanPowerSpec(T, Y, FreqVec, Args)
% cleanPowerSpec  CLEAN/prewhitening spectrum for unevenly sampled data.
%
% [FreqVec, Power, Info] = cleanPowerSpec(T,Y,FreqVec,...)
%
% This implementation iteratively:
%   1. computes a residual spectrum;
%   2. selects a residual peak (with a user-selected rank on iteration 1);
%   3. fits the sinusoid at that frequency by weighted least squares;
%   4. subtracts Gain times that fitted sinusoid from the residual time series.
%
% Name-value arguments
%   FirstPeakToSub - Rank of peak used on iteration 1. 1=highest. Default 1.
%   SpectrumMethod - "fourier" or "lombscargle". Default "fourier".
%   CalcMethod     - "loop" (memory efficient, default) or "matrix".
%                    "loop" uses one loop over trial frequencies.
%                    "matrix" evaluates all trial frequencies at once.
%   Gain           - CLEAN loop gain, 0<Gain<=1. Default 0.2.
%   MaxIter        - Maximum CLEAN iterations. Default 200.
%   StopFraction   - Stop when max residual spectral amplitude is below this
%                    fraction of its initial value. Default 1e-3.
%   MeanSubtract   - Remove weighted mean before processing. Default true.
%   Weights        - Nx1 non-negative weights. Default ones(N,1).
%   RestoreFWHM    - Gaussian restoring-beam FWHM in frequency units.
%                    [] estimates it from the sampling window. Default [].
%   ReturnComplex  - Store complex spectra in Info. Default true.
%
% Spectrum definitions
%   fourier:
%       A(f) = sum w_n y_n exp(-2*pi*i*f*t_n) / sum w_n
%       P(f) = |A(f)|^2.
%
%   lombscargle:
%       At every trial frequency, fit
%           y = C cos(2*pi*f*t) + S sin(2*pi*f*t)
%       by weighted least squares.  The positive-frequency complex amplitude
%       is a=(C-iS)/2, and P=|a|^2.  Thus the LS option retains phase/amplitude
%       information required for CLEAN subtraction; it does not use plomb's
%       scalar power alone.
%
% Notes
%   - The LS mode is a floating-normalization-free sinusoidal least-squares
%     periodogram after the optional global weighted mean subtraction.
%   - CalcMethod changes only how the spectrum is evaluated. Results should
%     agree to roundoff between "loop" and "matrix".
%
% Eran Ofek / ChatGPT, 2026-10-03

arguments
    T (:,1) double
    Y (:,1) double
    FreqVec (:,1) double
    Args.FirstPeakToSub (1,1) double {mustBeInteger,mustBePositive} = 1
    Args.SpectrumMethod (1,1) string = "fourier"
    Args.CalcMethod (1,1) string = "loop"
    Args.Gain (1,1) double = 0.2
    Args.MaxIter (1,1) double {mustBeInteger,mustBePositive} = 200
    Args.StopFraction (1,1) double {mustBeNonnegative} = 1e-3
    Args.MeanSubtract (1,1) logical = true
    Args.Weights (:,1) double = []
    Args.RestoreFWHM double = []
    Args.ReturnComplex (1,1) logical = true
end

if ~(Args.Gain > 0 && Args.Gain <= 1)
    error('cleanPowerSpec:BadGain','Gain must satisfy 0 < Gain <= 1.');
end
SpectrumMethod = lower(Args.SpectrumMethod);
CalcMethod = lower(Args.CalcMethod);
if ~ismember(SpectrumMethod,["fourier","lombscargle"])
    error('cleanPowerSpec:BadSpectrumMethod', ...
        'SpectrumMethod must be "fourier" or "lombscargle".');
end
if ~ismember(CalcMethod,["loop","matrix"])
    error('cleanPowerSpec:BadCalcMethod', ...
        'CalcMethod must be "loop" or "matrix".');
end

T = T(:);
Y = Y(:);
FreqVec = FreqVec(:);
N = numel(T);
M = numel(FreqVec);
if numel(Y) ~= N
    error('cleanPowerSpec:SizeMismatch','T and Y must have the same length.');
end
if M < 2
    error('cleanPowerSpec:TooFewFrequencies','FreqVec must contain at least two frequencies.');
end
if any(~isfinite(T)) || any(~isfinite(Y)) || any(~isfinite(FreqVec))
    error('cleanPowerSpec:NonFinite','T, Y, and FreqVec must be finite.');
end
if any(diff(FreqVec) <= 0)
    error('cleanPowerSpec:FreqOrder','FreqVec must be strictly increasing.');
end

if isempty(Args.Weights)
    Wgt = ones(N,1);
else
    Wgt = Args.Weights(:);
    if numel(Wgt) ~= N
        error('cleanPowerSpec:WeightSize','Weights must have the same length as T.');
    end
    if any(~isfinite(Wgt)) || any(Wgt < 0) || ~any(Wgt > 0)
        error('cleanPowerSpec:BadWeights', ...
            'Weights must be finite, non-negative, and not all zero.');
    end
end
Wsum = sum(Wgt);

if Args.MeanSubtract
    mu = sum(Wgt.*Y)./Wsum;
    Y0 = Y - mu;
else
    mu = 0;
    Y0 = Y;
end

% Initial residual spectrum.
tSpec = tic;
[DirtyComplex,DirtyPower] = localSpectrum(T,Y0,Wgt,FreqVec,SpectrumMethod,CalcMethod);
SpectrumTimeInitial = toc(tSpec);
ResidualY = Y0;
ResidualComplex = DirtyComplex;
ResidualPower = DirtyPower;

% Rank local peaks in the initial spectrum.
initialAmp = sqrt(max(DirtyPower,0));
peakInd0 = localPeakIndices(initialAmp);
if isempty(peakInd0)
    [~,peakInd0] = max(initialAmp);
end
[~,ord0] = sort(initialAmp(peakInd0),'descend');
peakInd0 = peakInd0(ord0);
if Args.FirstPeakToSub > numel(peakInd0)
    error('cleanPowerSpec:PeakRankTooLarge', ...
        'FirstPeakToSub=%d, but only %d local peaks were found.', ...
        Args.FirstPeakToSub,numel(peakInd0));
end
firstIndex = peakInd0(Args.FirstPeakToSub);

CleanComp = complex(zeros(M,1));
ResidualMax = nan(Args.MaxIter,1);
ChosenIndex = nan(Args.MaxIter,1);
ChosenFreq = nan(Args.MaxIter,1);
ChosenAmp = nan(Args.MaxIter,1);
ChosenCoef = complex(nan(Args.MaxIter,1));
SpectrumTime = zeros(Args.MaxIter,1);
initialMax = max(initialAmp);

for iter = 1:Args.MaxIter
    if iter == 1
        k = firstIndex;
    else
        amp = sqrt(max(ResidualPower,0));
        pk = localPeakIndices(amp);
        if isempty(pk)
            [~,k] = max(amp);
        else
            [~,ii] = max(amp(pk));
            k = pk(ii);
        end
    end

    f0 = FreqVec(k);
    r0amp = sqrt(max(ResidualPower(k),0));

    % Fit the selected sinusoid directly in the residual time series.
    % This gives a well-defined phase for both Fourier- and LS-selected peaks.
    aFit = localFitComplexSinusoid(T,ResidualY,Wgt,f0);
    deltaA = Args.Gain .* aFit;

    % For a real time series, y_component = a*exp(iwt)+conj(a)*exp(-iwt).
    ResidualY = ResidualY - 2.*real(deltaA .* exp(2i*pi*f0*T));
    CleanComp(k) = CleanComp(k) + deltaA;

    % Recompute the residual spectrum. This is more robust than subtracting a
    % scalar-power window because it works identically for Fourier and LS modes.
    tt = tic;
    [ResidualComplex,ResidualPower] = localSpectrum( ...
        T,ResidualY,Wgt,FreqVec,SpectrumMethod,CalcMethod);
    SpectrumTime(iter) = toc(tt);

    ResidualMax(iter) = max(sqrt(max(ResidualPower,0)));
    ChosenIndex(iter) = k;
    ChosenFreq(iter) = f0;
    ChosenAmp(iter) = r0amp;
    ChosenCoef(iter) = deltaA;

    if ResidualMax(iter) <= Args.StopFraction .* initialMax
        break;
    end
end

nIter = iter;
ResidualMax = ResidualMax(1:nIter);
ChosenIndex = ChosenIndex(1:nIter);
ChosenFreq = ChosenFreq(1:nIter);
ChosenAmp = ChosenAmp(1:nIter);
ChosenCoef = ChosenCoef(1:nIter);
SpectrumTime = SpectrumTime(1:nIter);

% CLEAN restoring beam.
if isempty(Args.RestoreFWHM)
    RestoreFWHM = localEstimateWindowFWHM(T,Wgt,Wsum,FreqVec,CalcMethod);
else
    if ~(isscalar(Args.RestoreFWHM) && isfinite(Args.RestoreFWHM) && Args.RestoreFWHM>0)
        error('cleanPowerSpec:BadRestoreFWHM', ...
            'RestoreFWHM must be empty or a positive finite scalar.');
    end
    RestoreFWHM = Args.RestoreFWHM;
end

sigmaF = RestoreFWHM ./ (2.*sqrt(2.*log(2)));
RestoredComplex = complex(zeros(M,1));
if sigmaF > 0
    nz = find(CleanComp ~= 0);
    for jj = 1:numel(nz)
        kk = nz(jj);
        g = exp(-0.5.*((FreqVec-FreqVec(kk))./sigmaF).^2);
        RestoredComplex = RestoredComplex + CleanComp(kk).*g;
    end
else
    RestoredComplex = CleanComp;
end

% Add the final residual spectrum in the same amplitude convention used for
% selecting peaks. This is the usual CLEAN restoration philosophy.
RestoredComplex = RestoredComplex + ResidualComplex;
Power = abs(RestoredComplex).^2;

Info = struct;
Info.Mean = mu;
Info.SpectrumMethod = char(SpectrumMethod);
Info.CalcMethod = char(CalcMethod);
Info.Gain = Args.Gain;
Info.MaxIter = Args.MaxIter;
Info.Niter = nIter;
Info.StopFraction = Args.StopFraction;
Info.FirstPeakToSub = Args.FirstPeakToSub;
Info.FirstPeakIndex = firstIndex;
Info.FirstPeakFrequency = FreqVec(firstIndex);
Info.RestoreFWHM = RestoreFWHM;
Info.InitialPeakIndices = peakInd0;
Info.InitialPeakFrequencies = FreqVec(peakInd0);
Info.InitialPeakAmplitudes = initialAmp(peakInd0);
Info.ChosenIndex = ChosenIndex;
Info.ChosenFreq = ChosenFreq;
Info.ChosenAmp = ChosenAmp;
Info.ChosenCoef = ChosenCoef;
Info.ResidualMax = ResidualMax;
Info.CleanComponent = CleanComp;
Info.DirtyPower = DirtyPower;
Info.ResidualPower = ResidualPower;
Info.ResidualY = ResidualY;
Info.SpectrumTimeInitial = SpectrumTimeInitial;
Info.SpectrumTimePerIteration = SpectrumTime;
Info.SpectrumTimeTotal = SpectrumTimeInitial + sum(SpectrumTime);
if Args.ReturnComplex
    Info.DirtyComplex = DirtyComplex;
    Info.ResidualComplex = ResidualComplex;
    Info.RestoredComplex = RestoredComplex;
end
end


function [A,P] = localSpectrum(T,Y,Wgt,FreqVec,SpectrumMethod,CalcMethod)
% Calculate complex spectral amplitude and linear power.
Nf = numel(FreqVec);
Wsum = sum(Wgt);
A = complex(zeros(Nf,1));

switch SpectrumMethod
    case "fourier"
        switch CalcMethod
            case "loop"
                % Memory-efficient default: exactly one loop over frequencies.
                WY = Wgt.*Y;
                for k = 1:Nf
                    A(k) = sum(WY .* exp(-2i*pi*FreqVec(k).*T)) ./ Wsum;
                end
            case "matrix"
                E = exp(-2i*pi.*(T*FreqVec.'));
                A = ((Wgt.*Y).' * E).';
                A = A ./ Wsum;
        end

    case "lombscargle"
        % Weighted sinusoidal least squares at each trial frequency.
        switch CalcMethod
            case "loop"
                WY = Wgt.*Y;
                for k = 1:Nf
                    ang = 2*pi*FreqVec(k).*T;
                    c = cos(ang);
                    s = sin(ang);
                    CC = sum(Wgt.*c.*c);
                    SS = sum(Wgt.*s.*s);
                    CS = sum(Wgt.*c.*s);
                    CY = sum(WY.*c);
                    SY = sum(WY.*s);
                    detG = CC.*SS - CS.^2;
                    if detG > eps(max(CC.*SS,1))
                        bc = (CY.*SS - SY.*CS) ./ detG;
                        bs = (SY.*CC - CY.*CS) ./ detG;
                        A(k) = 0.5.*complex(bc,-bs);
                    else
                        A(k) = 0;
                    end
                end

            case "matrix"
                ang = 2*pi.*(T*FreqVec.');
                C = cos(ang);
                S = sin(ang);
                WC = Wgt.*C;
                WS = Wgt.*S;
                CC = sum(C.*WC,1).';
                SS = sum(S.*WS,1).';
                CS = sum(C.*WS,1).';
                WY = Wgt.*Y;
                CY = (C.'*WY);
                SY = (S.'*WY);
                detG = CC.*SS - CS.^2;
                good = detG > eps(max(CC.*SS,1));
                bc = zeros(Nf,1);
                bs = zeros(Nf,1);
                bc(good) = (CY(good).*SS(good) - SY(good).*CS(good))./detG(good);
                bs(good) = (SY(good).*CC(good) - CY(good).*CS(good))./detG(good);
                A = 0.5.*complex(bc,-bs);
        end
end
P = abs(A).^2;
end


function a = localFitComplexSinusoid(T,Y,Wgt,f0)
% Weighted least-squares fit y=C*cos(wt)+S*sin(wt); a=(C-iS)/2.
ang = 2*pi*f0.*T;
c = cos(ang);
s = sin(ang);
CC = sum(Wgt.*c.*c);
SS = sum(Wgt.*s.*s);
CS = sum(Wgt.*c.*s);
CY = sum(Wgt.*Y.*c);
SY = sum(Wgt.*Y.*s);
detG = CC.*SS - CS.^2;
if detG <= eps(max(CC.*SS,1))
    a = 0;
else
    bc = (CY.*SS - SY.*CS)./detG;
    bs = (SY.*CC - CY.*CS)./detG;
    a = 0.5.*complex(bc,-bs);
end
end


function Ind = localPeakIndices(A)
A = A(:);
n = numel(A);
if n==1
    Ind = 1;
    return;
end
mask = false(n,1);
mask(1) = A(1) >= A(2);
mask(n) = A(n) >= A(n-1);
if n>2
    mask(2:n-1) = A(2:n-1) >= A(1:n-2) & A(2:n-1) >= A(3:n);
end
Ind = find(mask);
end


function fwhm = localEstimateWindowFWHM(T,Wgt,Wsum,FreqVec,CalcMethod)
span = max(T)-min(T);
if span<=0
    fwhm = median(diff(FreqVec));
    return;
end
dfMax = max(5/span,5*median(diff(FreqVec)));
df = linspace(0,dfMax,4000).';

switch CalcMethod
    case "loop"
        p = zeros(size(df));
        for k=1:numel(df)
            ww = sum(Wgt.*exp(-2i*pi*df(k).*T))./Wsum;
            p(k) = abs(ww).^2;
        end
    case "matrix"
        E = exp(-2i*pi.*(T*df.'));
        ww = (Wgt.'*E).'/Wsum;
        p = abs(ww).^2;
end

idx = find(p<=0.5,1,'first');
if isempty(idx) || idx==1
    fwhm = 1/span;
else
    x1=df(idx-1); x2=df(idx);
    y1=p(idx-1);  y2=p(idx);
    xh=x1+(0.5-y1).*(x2-x1)./(y2-y1);
    fwhm=2*xh;
end
end
