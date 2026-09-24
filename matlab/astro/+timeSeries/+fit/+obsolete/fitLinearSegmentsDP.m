function Result = fitLinearSegmentsDP(T, M, ErrorM, Args)
% fitLinearSegmentsDP  Fit piecewise-linear models using dynamic programming
% Package: AstroPack
% Description:
%   Fit each column of a time-series matrix M with exactly Nseg consecutive
%   linear segments. The segmentation is chosen using dynamic programming,
%   minimizing the total weighted chi-square of all segments.
%
%   For each source column, the fitted model in each segment is:
%
%       Y = Slope.*T + Intercept
%
%   The function supports scalar, vector, or matrix errors. NaN values in
%   T, M, or ErrorM are ignored independently for each source.
%
% Input:
%   T       - Time column vector [Ntime x 1].
%   M       - Data matrix [Ntime x Nsrc], one source per column.
%   ErrorM  - Measurement errors. May be:
%             scalar              - same error for all points and sources.
%             [Ntime x 1] vector   - same error vector for all sources.
%             [Ntime x Nsrc] matrix - individual error per point/source.
%
% Optional arguments:
%   Args.Nseg    - Number of linear segments. Default is 3.
%   Args.MinNpt  - Minimum number of points per segment. Default is 3.
%                  Must be >= 2 for a linear fit, but >= 3 is recommended
%                  so that each segment has at least one degree of freedom.
%   Args.SortT   - Sort valid points by time before fitting. Default is true.
%
% Output:
%   Result - Structure array of size [1, size(M,2)]. For each source:
%            .Seg     - [2 x Nseg] fitted parameters:
%                       [Slope; Intercept].
%            .SegErr  - [2 x Nseg] 1-sigma errors on fitted parameters:
%                       [SlopeErr; InterceptErr].
%            .Chi2    - [1 x Nseg] chi-square per segment.
%            .Dof     - [1 x Nseg] degrees of freedom per segment.
%            .Npt     - [1 x Nseg] number of points per segment.
%            .Ind     - [2 x Nseg] original row indices of segment start/end.
%            .Tlim    - [2 x Nseg] time limits of each segment.
%            .Flag    - true if fit succeeded, false otherwise.
%
% Example:
%   Result = fitLinearSegmentsDP(T, M, ErrorM, 'Nseg', 4, 'MinNpt', 4);
%
% Notes:
%   - The fit is weighted least squares with weights 1./ErrorM.^2.
%   - ErrorM is treated as absolute 1-sigma uncertainty.
%   - The algorithm is O(Nsrc * Nseg * Ntime^2).
%   - To improve numerical stability for large astronomical time values,
%     the internal fit uses centered time, but the returned intercept is
%     converted back to the original T system.
%
% Author:
%   Eran Ofek / ChatGPT
%
% Last update:
%   2026-05-14

arguments
    T (:,1) double
    M (:,:) double
    ErrorM double = 1
    Args.Nseg (1,1) double {mustBeInteger, mustBePositive} = 3
    Args.MinNpt (1,1) double {mustBeInteger, mustBePositive} = 3
    Args.SortT (1,1) logical = true
end

if Args.MinNpt < 2
    error('Args.MinNpt must be >= 2 for a linear fit.');
end

Ntime = numel(T);
[Nrow, Nsrc] = size(M);

if Nrow ~= Ntime
    error('Number of rows in M must be equal to numel(T).');
end

ErrorType = localErrorType(ErrorM, Ntime, Nsrc);

Template = struct( ...
    'Seg',    NaN(2, Args.Nseg), ...
    'SegErr', NaN(2, Args.Nseg), ...
    'Chi2',   NaN(1, Args.Nseg), ...
    'Dof',    NaN(1, Args.Nseg), ...
    'Npt',    zeros(1, Args.Nseg), ...
    'Ind',    NaN(2, Args.Nseg), ...
    'Tlim',   NaN(2, Args.Nseg), ...
    'Flag',   false);

Result = repmat(Template, 1, Nsrc);

for Isrc = 1:Nsrc

    Y = M(:,Isrc);
    Err = localGetErrorColumn(ErrorM, ErrorType, Isrc, Ntime);

    Valid = isfinite(T) & isfinite(Y) & isfinite(Err) & Err > 0;
    IndOrig = find(Valid);

    Tv = T(Valid);
    Yv = Y(Valid);
    Ev = Err(Valid);

    if Args.SortT
        [Tv, SortInd] = sort(Tv);
        Yv = Yv(SortInd);
        Ev = Ev(SortInd);
        IndOrig = IndOrig(SortInd);
    end

    Nvalid = numel(Tv);

    if Nvalid < Args.Nseg .* Args.MinNpt
        continue;
    end

    T0 = median(Tv, 'omitnan');
    Xv = Tv - T0;
    Wv = 1 ./ (Ev.^2);

    Cum = localCumulativeSums(Xv, Yv, Wv);

    Cost = localAllSegmentCosts(Cum, Nvalid, Args.MinNpt);

    [SegmentRange, FitFlag] = localDynamicProgramming(Cost, Args.Nseg, Args.MinNpt);

    if ~FitFlag
        continue;
    end

    for Iseg = 1:Args.Nseg
        I1 = SegmentRange(1,Iseg);
        I2 = SegmentRange(2,Iseg);

        S = localIntervalSums(Cum, I1, I2);
        [Slope, Intercept, SlopeErr, InterceptErr, Chi2] = localFitInterval(S, T0);

        Npt = I2 - I1 + 1;
        Dof = Npt - 2;

        Result(Isrc).Seg(:,Iseg) = [Slope; Intercept];
        Result(Isrc).SegErr(:,Iseg) = [SlopeErr; InterceptErr];
        Result(Isrc).Chi2(Iseg) = Chi2;
        Result(Isrc).Dof(Iseg) = Dof;
        Result(Isrc).Npt(Iseg) = Npt;
        Result(Isrc).Ind(:,Iseg) = [IndOrig(I1); IndOrig(I2)];
        Result(Isrc).Tlim(:,Iseg) = [Tv(I1); Tv(I2)];
    end

    Result(Isrc).Flag = true;
end

end


function ErrorType = localErrorType(ErrorM, Ntime, Nsrc)

if isscalar(ErrorM)
    ErrorType = 'scalar';
elseif isvector(ErrorM) && numel(ErrorM) == Ntime
    ErrorType = 'vector';
elseif isequal(size(ErrorM), [Ntime, Nsrc])
    ErrorType = 'matrix';
else
    error(['ErrorM must be scalar, a vector with numel(T) elements, ', ...
           'or a matrix with the same size as M.']);
end

end


function Err = localGetErrorColumn(ErrorM, ErrorType, Isrc, Ntime)

switch ErrorType
    case 'scalar'
        Err = ErrorM .* ones(Ntime, 1);
    case 'vector'
        Err = ErrorM(:);
    case 'matrix'
        Err = ErrorM(:,Isrc);
    otherwise
        error('Unknown ErrorType.');
end

end


function Cum = localCumulativeSums(X, Y, W)

% Cumulative sums are stored with leading zero, so interval [I1,I2] is:
% Cum(I2+1) - Cum(I1)

Cum.S0  = [0; cumsum(W)];
Cum.Sx  = [0; cumsum(W .* X)];
Cum.Sy  = [0; cumsum(W .* Y)];
Cum.Sxx = [0; cumsum(W .* X .* X)];
Cum.Sxy = [0; cumsum(W .* X .* Y)];
Cum.Syy = [0; cumsum(W .* Y .* Y)];

end

function Cost = localAllSegmentCosts(Cum, Nvalid, MinNpt)

Cost = Inf(Nvalid, Nvalid);

for I1 = 1:(Nvalid - MinNpt + 1)

    I2vec = ((I1 + MinNpt - 1):Nvalid).';   % column vector

    S0  = Cum.S0(I2vec + 1)  - Cum.S0(I1);
    Sx  = Cum.Sx(I2vec + 1)  - Cum.Sx(I1);
    Sy  = Cum.Sy(I2vec + 1)  - Cum.Sy(I1);
    Sxx = Cum.Sxx(I2vec + 1) - Cum.Sxx(I1);
    Sxy = Cum.Sxy(I2vec + 1) - Cum.Sxy(I1);
    Syy = Cum.Syy(I2vec + 1) - Cum.Syy(I1);

    Denom = Sxx .* S0 - Sx.^2;
    Good = Denom > 0 & S0 > 0;

    Slope = NaN(size(I2vec));
    InterceptCentered = NaN(size(I2vec));
    Chi2 = Inf(size(I2vec));

    Slope(Good) = (S0(Good).*Sxy(Good) - Sx(Good).*Sy(Good)) ./ Denom(Good);

    InterceptCentered(Good) = ...
        (Sxx(Good).*Sy(Good) - Sx(Good).*Sxy(Good)) ./ Denom(Good);

    Chi2(Good) = Syy(Good) ...
        - Slope(Good).*Sxy(Good) ...
        - InterceptCentered(Good).*Sy(Good);

    Chi2(Good) = max(Chi2(Good), 0);

    Cost(I1, I2vec) = Chi2;
end

end



function [SegmentRange, FitFlag] = localDynamicProgramming(Cost, Nseg, MinNpt)

Nvalid = size(Cost, 1);

DP = Inf(Nseg, Nvalid);
Back = NaN(Nseg, Nvalid);

for Iend = MinNpt:Nvalid
    DP(1,Iend) = Cost(1,Iend);
    Back(1,Iend) = 0;
end

for Iseg = 2:Nseg

    MinEnd = Iseg .* MinNpt;

    for Iend = MinEnd:Nvalid

        PrevEndVec = ((Iseg - 1).*MinNpt):(Iend - MinNpt);

        if isempty(PrevEndVec)
            continue;
        end

        SegmentCost = Cost(sub2ind(size(Cost), PrevEndVec + 1, ...
            Iend .* ones(size(PrevEndVec))));

        TotalCost = DP(Iseg - 1, PrevEndVec) + SegmentCost;

        [BestCost, BestInd] = min(TotalCost);

        if isfinite(BestCost)
            DP(Iseg, Iend) = BestCost;
            Back(Iseg, Iend) = PrevEndVec(BestInd);
        end
    end
end

FitFlag = isfinite(DP(Nseg, Nvalid));
SegmentRange = NaN(2, Nseg);

if ~FitFlag
    return;
end

Iend = Nvalid;

for Iseg = Nseg:-1:1

    PrevEnd = Back(Iseg, Iend);

    if Iseg == 1
        Istart = 1;
    else
        Istart = PrevEnd + 1;
    end

    SegmentRange(:,Iseg) = [Istart; Iend];

    Iend = PrevEnd;
end

end


function S = localIntervalSums(Cum, I1, I2)

S.S0  = Cum.S0(I2 + 1)  - Cum.S0(I1);
S.Sx  = Cum.Sx(I2 + 1)  - Cum.Sx(I1);
S.Sy  = Cum.Sy(I2 + 1)  - Cum.Sy(I1);
S.Sxx = Cum.Sxx(I2 + 1) - Cum.Sxx(I1);
S.Sxy = Cum.Sxy(I2 + 1) - Cum.Sxy(I1);
S.Syy = Cum.Syy(I2 + 1) - Cum.Syy(I1);

end


function [Slope, Intercept, SlopeErr, InterceptErr, Chi2] = localFitInterval(S, T0)

Denom = S.Sxx .* S.S0 - S.Sx.^2;

if ~(isfinite(Denom) && Denom > 0)
    Slope = NaN;
    Intercept = NaN;
    SlopeErr = NaN;
    InterceptErr = NaN;
    Chi2 = NaN;
    return;
end

Slope = (S.S0.*S.Sxy - S.Sx.*S.Sy) ./ Denom;

InterceptCentered = (S.Sxx.*S.Sy - S.Sx.*S.Sxy) ./ Denom;

Chi2 = S.Syy - Slope.*S.Sxy - InterceptCentered.*S.Sy;
Chi2 = max(Chi2, 0);

% Covariance matrix in centered coordinates:
% beta = [Slope; InterceptCentered]
VarSlope = S.S0 ./ Denom;
VarInterceptCentered = S.Sxx ./ Denom;
CovSlopeInterceptCentered = -S.Sx ./ Denom;

% Convert centered intercept to original-time intercept:
% Y = Slope*(T - T0) + InterceptCentered
%   = Slope*T + (InterceptCentered - Slope*T0)
Intercept = InterceptCentered - Slope.*T0;

VarIntercept = VarInterceptCentered ...
    + T0.^2 .* VarSlope ...
    - 2 .* T0 .* CovSlopeInterceptCentered;

SlopeErr = sqrt(max(VarSlope, 0));
InterceptErr = sqrt(max(VarIntercept, 0));

end