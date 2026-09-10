function [A, Alpha] = fit_chi2vsSN(Chi2Dof, SN)
% Fit chi2/dof vs. S/N using a power-law excess above unity.
% Input  : - Vector of chi2/dof values.
%          - Vector of signal-to-noise ratios.
% Output : - Best-fit normalization, A.
%          - Best-fit power-law index, Alpha.
% Tested : Matlab R2024a
%     By : Eran Ofek                    Sep 2026
%    URL : https://www.wis-tns.org
% Example: [A,Alpha] = fit_chi2vsSN(Chi2Dof,SN)
% Reliable: 2
%
% The fitted model is:
%
%   Chi2Dof = 1 + A.*SN.^Alpha
%
% The data are first divided into logarithmically spaced S/N bins.
% The median S/N and median chi2/dof are calculated in each bin.
% The fit is then performed on these median values, with equal
% weight for each populated bin.
%
% The minimized quantity is:
%
%   sum( [log(MedianChi2Dof) -
%         log(1 + A.*MedianSN.^Alpha)].^2 )
%
% Note that the logarithm is applied to Chi2Dof itself, and NOT to
% Chi2Dof-1. Therefore, values of Chi2Dof below unity do not cause
% any problem.
%
% A and Alpha are required to be positive.
%

arguments
    Chi2Dof
    SN
end

% convert to column vectors
Chi2Dof = Chi2Dof(:);
SN      = SN(:);

% select valid measurements
Flag = isfinite(Chi2Dof) & ...
       isfinite(SN)      & ...
       Chi2Dof>0         & ...
       SN>0;

Chi2Dof = Chi2Dof(Flag);
SN      = SN(Flag);

N = numel(SN);

if N<10
    error('fit_chi2vsSN:TooFewPoints',...
          'Not enough valid data points');
end

% number of logarithmic S/N bins
Nbin = min(20, max(5, round(sqrt(N))));

% minimum number of measurements in a bin
MinNperBin = 5;

% construct logarithmic S/N bins
LogSN = log10(SN);

MinLogSN = min(LogSN);
MaxLogSN = max(LogSN);

if MinLogSN==MaxLogSN
    error('fit_chi2vsSN:NoSNRange',...
          'All S/N values are identical');
end

Edges = linspace(MinLogSN, MaxLogSN, Nbin+1);

BinInd = discretize(LogSN, Edges);

MedianSN   = nan(Nbin,1);
MedianChi2 = nan(Nbin,1);
NperBin    = zeros(Nbin,1);

for Ibin = 1:Nbin

    FlagBin = BinInd==Ibin;

    NperBin(Ibin) = sum(FlagBin);

    if NperBin(Ibin)>=MinNperBin
        MedianSN(Ibin)   = median(SN(FlagBin));
        MedianChi2(Ibin) = median(Chi2Dof(FlagBin));
    end

end

% remove bins with too few sources
FlagBin = isfinite(MedianSN) & isfinite(MedianChi2);

MedianSN   = MedianSN(FlagBin);
MedianChi2 = MedianChi2(FlagBin);

if numel(MedianSN)<3
    error('fit_chi2vsSN:TooFewBins',...
          'Too few populated S/N bins for fitting');
end

% initial guess
Alpha0 = 2;

FlagPos = MedianChi2>1;

if any(FlagPos)
    A0 = median((MedianChi2(FlagPos)-1) ./ ...
                MedianSN(FlagPos).^Alpha0);
else
    A0 = 1e-6;
end

if ~isfinite(A0) || A0<=0
    A0 = 1e-6;
end

Par0 = [A0, Alpha0];

% fit
Fun = @(Par) objectiveFun(Par, MedianSN, MedianChi2);

Options = optimset('Display','off',...
                   'MaxFunEvals',1e4,...
                   'MaxIter',1e4,...
                   'TolX',1e-10,...
                   'TolFun',1e-10);

Par = fminsearch(Fun, Par0, Options);

A     = Par(1);
Alpha = Par(2);

end


function Val = objectiveFun(Par, SN, Chi2Dof)
% Objective function.

A     = Par(1);
Alpha = Par(2);

% require positive parameters
if ~isfinite(A) || ~isfinite(Alpha) || A<=0 || Alpha<=0
    Val = realmax;
    return;
end

Model = 1 + A.*SN.^Alpha;

if any(~isfinite(Model)) || any(Model<=0)
    Val = realmax;
    return;
end

% fit fractional rather than absolute deviations
Resid = log(Chi2Dof) - log(Model);

Val = sum(Resid.^2);

end