function S = twoSampleStats(A, B, Args)
    % Two-sample comparison statistics for two numeric samples.
    % Description: Drops non-finite values, then computes the
    %              Kolmogorov-Smirnov p-value, the Mann-Whitney (rank-sum)
    %              p-value, and Cliff's delta effect size. p-values and the
    %              effect size are NaN unless both samples have at least
    %              'MinN' finite values. Shared by the calibrator-audit and
    %              spectra-comparison tools.
    % Input  : - A - first numeric sample.
    %          - B - second numeric sample.
    %          * ...,key,val,...
    %            'MinN' - Minimum finite count in each sample required to
    %                     compute the tests. Default 5.
    % Output : - S - struct with fields N_A, N_B, Med_A, Med_B, KS_p, MW_p,
    %            CliffsDelta. CliffsDelta > 0 means A tends to exceed B.
    % Author : photCalib package refactor (2026-05)
    % Example: S = twoSampleStats(RejectedVals, KeptVals);

    arguments
        A
        B
        Args.MinN (1,1) double = 5
    end

    A = A(:); A = A(isfinite(A));
    B = B(:); B = B(isfinite(B));

    S = struct('N_A', numel(A), 'N_B', numel(B), ...
               'Med_A', NaN, 'Med_B', NaN, ...
               'KS_p', NaN, 'MW_p', NaN, 'CliffsDelta', NaN);
    if ~isempty(A); S.Med_A = median(A); end
    if ~isempty(B); S.Med_B = median(B); end

    if numel(A) >= Args.MinN && numel(B) >= Args.MinN
        try
            [~, S.KS_p] = kstest2(A, B);
        catch
        end
        try
            S.MW_p = ranksum(A, B);
        catch
        end
        S.CliffsDelta = cliffsDelta(A, B);
    end
end

% =========================================================================
function d = cliffsDelta(a, b)
    % Cliff's delta effect size; positive => a tends to exceed b.
    a = a(:); b = b(:);
    if isempty(a) || isempty(b)
        d = NaN;
        return;
    end
    R   = tiedrank([a; b]);
    Ra  = sum(R(1:numel(a)));
    n_a = numel(a);
    n_b = numel(b);
    U   = Ra - n_a * (n_a + 1) / 2;
    d   = 2 * U / (n_a * n_b) - 1;
end
