function [pLow, pHigh, muLow, muHigh] = binomial_ci(N, k, ProbLow, ProbHigh)
    % Exact Clopper-Pearson confidence interval for binomial probability
    %   [pLow, pHigh, muLow, muHigh] = BINOMIAL_CI(N, k, ProbLow, ProbHigh)
    %   returns lower/upper bounds on the binomial proportion p and on the mean
    %   mu = N*p, using exact (Clopper-Pearson) inversion.
    %
    % Input  : - (N) number of trials (non-negative integer)
    %          - (k) number of successes (integer, 0 <= k <= N)
    %          - (ProbLow) lower-tail probability α_low  (0 <= α_low <= 1)
    %          - (ProbHigh) upper-tail probability α_high (0 <= α_high <= 1)
    %            If omitted or empty, ProbHigh = ProbLow.
    % Output : - Lower confidence interval on p.
    %          - Upper confidence interval on p.
    %          - Lower confidence interval on mu=N*p
    %          - Upper confidence interval on mu=N*p
    % Notes  :
    %     * For a two-sided (1-α) interval with equal tails, set:
    %         ProbLow = ProbHigh = α/2.   (e.g., α=0.3173 -> 68.27%)
    %     * Uses betainv; handles edge cases k=0 and k=N.
    % Author : ChatGPT + Eran Ofek (Oct 2025)
    % Example: [pL, pU] = tools.math.stat.binomial_ci(9,1)
    
    arguments
        N
        k
        ProbLow  = 0.158655;
        ProbHigh = [];
    end
     
    % Defaults
    if isempty(ProbHigh)
        ProbHigh = ProbLow;
    end

    % Basic validation
    if ~(isscalar(N) && isscalar(k) && isscalar(ProbLow) && isscalar(ProbHigh))
        error('All inputs must be scalars.');
    end
    if N < 0 || k < 0 || k > N || N ~= floor(N) || k ~= floor(k)
        error('Require integer N >= 0 and integer k with 0 <= k <= N.');
    end
    if ProbLow < 0 || ProbLow > 1 || ProbHigh < 0 || ProbHigh > 1
        error('ProbLow and ProbHigh must be in [0, 1].');
    end

    % Handle degenerate N=0
    if N == 0
        % With no trials, p is undefined; return [0,1].
        pLow = 0; pHigh = 1;
        muLow = 0; muHigh = 0;  % N*p is always 0 when N=0
        return;
    end

    % Exact Clopper–Pearson via Beta quantiles
    % Lower bound (if k=0, lower bound is 0)
    if k == 0
        pLow = 0;
    else
        pLow = betainv(ProbLow, k, N - k + 1);
    end

    % Upper bound (if k=N, upper bound is 1)
    if k == N
        pHigh = 1;
    else
        pHigh = betainv(1 - ProbHigh, k + 1, N - k);
    end

    % Map to mean μ = N p
    muLow  = N * pLow;
    muHigh = N * pHigh;
end
