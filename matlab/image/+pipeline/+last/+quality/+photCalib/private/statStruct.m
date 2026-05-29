function S = statStruct(V)
    % Summary statistics of a numeric sample.
    % Description: Drops non-finite values, then returns the standard
    %              photCalib summary-statistics struct. Shared by the
    %              histogram and distribution plotters.
    % Input  : - V - numeric array (treated as a flat vector).
    % Output : - S - struct with fields N, Median, Mean, Std, P16, P84,
    %            Min, Max. All statistics are NaN when no finite value
    %            remains; N is the count of finite values.
    % Author : photCalib package refactor (2026-05)
    % Example: S = statStruct(Values);

    arguments
        V
    end

    V = V(:);
    V = V(isfinite(V));
    S = struct('N', numel(V), 'Median', NaN, 'Mean', NaN, 'Std', NaN, ...
               'P16', NaN, 'P84', NaN, 'Min', NaN, 'Max', NaN);
    if ~isempty(V)
        S.Median = median(V);
        S.Mean   = mean(V);
        S.Std    = std(V);
        S.P16    = prctile(V, 16);
        S.P84    = prctile(V, 84);
        S.Min    = min(V);
        S.Max    = max(V);
    end
end
