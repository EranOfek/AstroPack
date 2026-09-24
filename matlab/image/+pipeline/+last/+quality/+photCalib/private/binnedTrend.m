function T = binnedTrend(X, Y, Args)
    % Binned trend of Y as a function of X.
    % Description: Bins (X,Y) pairs in fixed-width X bins via
    %              timeSeries.bin.binningFast and returns the per-bin trend
    %              statistic (median or mean), count, and per-bin scatter.
    %              Only bins with at least 'MinCount' points are returned.
    %              Shared trend helper for the residual and stability plots.
    % Input  : - X - numeric array of bin-axis values.
    %          - Y - numeric array of values to summarise (same size as X).
    %          * ...,key,val,...
    %            'BinWidth' - X bin width. Default 0.5.
    %            'Range'    - [lo hi] X range. Default [] (= [floor(min X)
    %                         ceil(max X)]).
    %            'Stat'     - 'median' | 'mean' trend statistic. Default
    %                         'median'.
    %            'MinCount' - Minimum points per returned bin. Default 5.
    % Output : - T - struct with fields X (bin centres), Val (trend), Count,
    %            Std (per-bin std). Empty fields when no valid bin exists.
    % Author : photCalib package refactor (2026-05)
    % Example: T = binnedTrend(Mag, Residual, 'BinWidth', 0.5);

    arguments
        X
        Y
        Args.BinWidth (1,1) double = 0.5
        Args.Range                 = []
        Args.Stat     {mustBeMember(Args.Stat,{'median','mean'})} = 'median'
        Args.MinCount (1,1) double = 5
    end

    X = X(:);
    Y = Y(:);
    Good = isfinite(X) & isfinite(Y);
    X = X(Good);
    Y = Y(Good);

    T = struct('X', [], 'Val', [], 'Count', [], 'Std', []);
    if isempty(X)
        return;
    end

    Range = Args.Range;
    if isempty(Range)
        Range = [floor(min(X)), ceil(max(X))];
    end
    if Range(2) <= Range(1)
        Range(2) = Range(1) + Args.BinWidth;
    end

    TrendFun = str2func(['nan' Args.Stat]);
    R = timeSeries.bin.binningFast([X, Y], Args.BinWidth, Range, ...
        {'MidBin', @numel, TrendFun, @nanstd});

    Valid   = R(:,2) >= Args.MinCount;
    T.X     = R(Valid, 1);
    T.Count = R(Valid, 2);
    T.Val   = R(Valid, 3);
    T.Std   = R(Valid, 4);
end
