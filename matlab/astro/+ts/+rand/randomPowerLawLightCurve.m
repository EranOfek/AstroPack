function [Flux, Info] = randomPowerLawLightCurve(Time, Opt)
% Generate Gaussian correlated light curve with power-law PSD.
%   No additive white measurement noise.
% PSD: P(f) proportional to f^(-Slope)
%
% Input:
%   Time - Observation times (regular or irregular).
%
% Options:
%   Slope     - PSD power-law index (default 2).
%   FreqRange - [Fmin Fmax], default [1/Tspan, Fnyq].
%   Mean      - Ensemble mean (default 0).
%   Std       - Ensemble standard deviation (default 1).
%   Seed      - RNG seed ([] = current RNG).
%
% Output:
%   Flux - Simulated light curve (same shape as Time).
%   Info - Frequency grid, PSD, frequency spacing.
%
% Author : ChatGPT + Eran Ofek (Oct 2026)
% Example: [Flux, Info] = ts.rand.randomPowerLawLightCurve(Time)

arguments
    Time {mustBeNumeric, mustBeReal, mustBeFinite}
    Opt.Slope (1,1) double {mustBeFinite} = 2
    Opt.FreqRange (1,2) double = [NaN NaN]
    Opt.Mean (1,1) double {mustBeFinite} = 0
    Opt.Std (1,1) double ...
        {mustBeNonnegative, mustBeFinite} = 1
    Opt.Seed = []
end

OriginalSize = size(Time);
T = double(Time(:));
Ts = sort(unique(T));

if numel(Ts) < 2
    error('At least two distinct times are required.');
end

Tspan = Ts(end) - Ts(1);
Dt = median(diff(Ts));

% Frequency grid.
df = 1 ./ Tspan;

FreqRange = Opt.FreqRange;

if isnan(FreqRange(1))
    FreqRange(1) = df;
end

if isnan(FreqRange(2))
    FreqRange(2) = 1 ./ (2 .* Dt);
end

Fmin = FreqRange(1);
Fmax = FreqRange(2);

if ~(isfinite(Fmin) && isfinite(Fmax) && ...
        Fmin > 0 && Fmax >= Fmin)
    error('Invalid frequency range.');
end

% Uniform Fourier frequency grid.
% Frequencies are integer multiples of df.
Kmin = max(1, ceil(Fmin ./ df));
Kmax = floor(Fmax ./ df);

if Kmax < Kmin
    error('No Fourier frequencies in requested band.');
end

Freq = (Kmin:Kmax).' .* df;
Nfreq = numel(Freq);

% Power-law PSD.
PSD = (Freq ./ Freq(1)).^(-Opt.Slope);

% Normalize to requested ensemble variance.
Weights = PSD .* df;
Weights = Weights .* (Opt.Std.^2 ./ sum(Weights));

% Optional reproducible RNG.
if ~isempty(Opt.Seed)
    OldRNG = rng;
    Cleanup = onCleanup(@() rng(OldRNG));
    rng(Opt.Seed);
end

% Independent Gaussian Fourier coefficients.
A = randn(Nfreq,1) .* sqrt(Weights);
B = randn(Nfreq,1) .* sqrt(Weights);

% Evaluate at arbitrary observation times.
Trel = T - min(T);
X = zeros(numel(T),1);

BlockSize = max(1, floor(2e6 ./ Nfreq));

for I = 1:BlockSize:numel(T)
    Ind = I:min(I+BlockSize-1, numel(T));

    Phase = 2*pi .* (Trel(Ind) * Freq.');

    X(Ind) = cos(Phase)*A + sin(Phase)*B;
end

Flux = reshape(Opt.Mean + X, OriginalSize);

if nargout > 1
    Info.Freq = Freq;
    Info.PSD = Weights ./ df;
    Info.df = df;
    Info.Nfreq = Nfreq;
    Info.FreqRange = [Fmin Fmax];
    Info.ExpectedStd = Opt.Std;
end

end
