function Result = fitPiecewiseLinear(T, M, ErrorM, Args)
% fitPiecewiseLinear: Globally optimal weighted N-segment piecewise linear fit.
%     For each column (source) of M, finds the (Nseg-1) breakpoints that
%     globally minimise the total chi^2 = sum_i w_i*(m_i - a - b*t_i)^2,
%     where w_i = 1/sigma_i^2.  The optimisation is solved exactly with
%     dynamic programming (DP); a greedy delta-chi^2 scan would be path-
%     dependent and suboptimal.
%
%     Segment costs are computed in O(1) each via prefix sums of six weighted
%     moment arrays, giving O(n^2) cost-table construction and O(Nseg*n^2)
%     total DP per source.
%
% Input  : - T      : (N x 1) double. Time / x-coordinate vector.
%                     Sorting is applied internally; the original order is
%                     not modified in the caller's workspace.
%          - M      : (N x S) double. Data matrix.
%                     Each column is one source.
%          - ErrorM : Measurement 1-sigma errors on M.  Accepted forms:
%                     * Scalar      - same error for all points and sources.
%                     * (N x 1) or (1 x N) - per-epoch, same for all sources.
%                     * (1 x S) or (S x 1) - per-source scalar error.
%                     * (N x S)             - full error matrix.
%                     Default is 1 (unweighted fit; chi^2 equals RSS).
%          * Pairs of ...,key,val,... or Args.key = val:
%            'Nseg'        - Number of linear segments. Default is 3.
%            'MinPts'      - Minimum number of data points per segment (must be
%                           >= 2 so that 2 OLS parameters are identifiable).
%                           Default is 2.
%            'UseParallel' - If true and the Parallel Computing Toolbox is
%                           available, process sources with parfor.
%                           Default is false.
%
% Output : - Result : Structure array of size (1 x S).
%                     Each element contains the following fields:
%            .Seg    - [2 x Nseg] double. Fitted parameters per segment.
%                      Row 1: slope (b).  Row 2: intercept (a).
%                      Model for segment k:  m = Seg(2,k) + Seg(1,k)*t.
%            .SegErr - [2 x Nseg] double. 1-sigma parameter errors.
%                      Row 1: sigma_slope.  Row 2: sigma_intercept.
%                      Derived from the weighted normal-equation covariance:
%                        sigma_b = sqrt(sum_w / D),
%                        sigma_a = sqrt(sum_wt2 / D),
%                        D = sum_w*sum_wt2 - (sum_wt)^2.
%            .Chi2   - [1 x Nseg] double. chi^2 per segment.
%                      chi^2 = sum_wm2 - a*sum_wm - b*sum_wtm  (>= 0).
%            .Dof    - [1 x Nseg] double. Degrees of freedom (Npt - 2).
%            .Npt    - [1 x Nseg] double. Number of data points per segment.
%            .Ind    - [2 x Nseg] double. Start and end indices into T and M
%                      for each segment.  Row 1: start index. Row 2: end index.
%            .Tlim   - [2 x Nseg] double. Corresponding time limits.
%                      Row 1: T(start).  Row 2: T(end).
%
% Tested : MATLAB R2021b+
% Author : <author>
% Example: N=200; T=(1:N)'; sigma=0.3;
%          Msig = [1+T; -2*T+300] + sigma*randn(N,2);  % two sources
%          Res = fitPiecewiseLinear(T, Msig, sigma, 'Nseg', 3);
%          disp(Res(1).Seg)   % slopes and intercepts for source 1

    arguments
        T        (:,1) double
        M        (:,:) double
        ErrorM             = 1        % scalar | N-vec | S-vec | N×S matrix
        Args.Nseg        (1,1) double  {mustBePositive} = 3
        Args.MinPts      (1,1) double  {mustBePositive} = 2
        Args.UseParallel (1,1) logical                  = false
    end

    %======================================================================
    % 0.  Setup and input validation
    %======================================================================
    N   = numel(T);
    S   = size(M, 2);
    Ns  = round(Args.Nseg);
    Mp  = max(2, round(Args.MinPts));      % enforce hard minimum of 2

    if size(M, 1) ~= N
        error('fitPiecewiseLinear:dimMismatch', ...
              'T (length %d) and M (%d rows) must have the same number of rows.', ...
              N, size(M,1));
    end
    if N < Mp * Ns
        error('fitPiecewiseLinear:tooFewPoints', ...
              'Need at least MinPts*Nseg = %d data points, got %d.', Mp*Ns, N);
    end

    %----------------------------------------------------------------------
    % Expand ErrorM to a full (N x S) matrix of positive sigmas
    %----------------------------------------------------------------------
    ErrFull = expandErrors(ErrorM, N, S);

    %----------------------------------------------------------------------
    % Sort T and reorder M and ErrFull consistently.
    % 'stable' preserves the original relative order of tied timestamps.
    %----------------------------------------------------------------------
    [T, SortIdx] = sort(T);
    M            = M(SortIdx, :);
    ErrFull      = ErrFull(SortIdx, :);

    %======================================================================
    % 1.  Pre-allocate output structure array
    %======================================================================
    ZeroSeg  = zeros(2, Ns);
    ZeroStat = zeros(1, Ns);
    EmptyEl  = struct('Seg', ZeroSeg, 'SegErr', ZeroSeg, ...
                      'Chi2', ZeroStat, 'Dof', ZeroStat, 'Npt', ZeroStat, ...
                      'Ind',  ZeroSeg,  'Tlim', ZeroSeg);
    Result   = repmat(EmptyEl, 1, S);

    %======================================================================
    % 2.  Main loop over sources  (parallel if requested)
    %======================================================================
    if Args.UseParallel
        parfor Si = 1:S
            Result(Si) = fitOneSource(T, M(:,Si), ErrFull(:,Si), N, Ns, Mp);
        end
    else
        for Si = 1:S
            Result(Si) = fitOneSource(T, M(:,Si), ErrFull(:,Si), N, Ns, Mp);
        end
    end

end  % fitPiecewiseLinear


%==========================================================================
%  fitOneSource: all per-source computation, extracted for parfor safety
%==========================================================================
function Out = fitOneSource(T, Mv, ErrVec, N, Ns, Mp)

    %----------------------------------------------------------------------
    % Prefix sums of six weighted moment arrays
    % W_i = 1/sigma_i^2;  enables O(1) chi^2 for any segment [I,J]
    %----------------------------------------------------------------------
    Wvec  = 1 ./ ErrVec .^ 2;

    Pw   = [0; cumsum(Wvec)];
    PwT  = [0; cumsum(Wvec .* T)];
    PwT2 = [0; cumsum(Wvec .* T .^ 2)];
    PwM  = [0; cumsum(Wvec .* Mv)];
    PwTM = [0; cumsum(Wvec .* T .* Mv)];
    PwM2 = [0; cumsum(Wvec .* Mv .^ 2)];

    %----------------------------------------------------------------------
    % Build O(n^2) cost table — stored as single to halve memory.
    % CostMat(I,J) = chi^2 of weighted OLS line fit to T(I:J).
    % Vectorised over J for each fixed I.
    %
    % Upper triangle (J < I+Mp-1, segment too short) stays inf.
    % This property is exploited in the vectorised DP below.
    %----------------------------------------------------------------------
    CostMat = inf(N, N, 'single');

    for I = 1 : N - Mp + 1
        Jj  = (I + Mp - 1) : N;
        Len = numel(Jj);

        Sw   = reshape(Pw(Jj   + 1) - Pw(I),   1, Len);
        SwT  = reshape(PwT(Jj  + 1) - PwT(I),  1, Len);
        SwT2 = reshape(PwT2(Jj + 1) - PwT2(I), 1, Len);
        SwM  = reshape(PwM(Jj  + 1) - PwM(I),  1, Len);
        SwTM = reshape(PwTM(Jj + 1) - PwTM(I), 1, Len);
        SwM2 = reshape(PwM2(Jj + 1) - PwM2(I), 1, Len);

        D    = Sw .* SwT2 - SwT .^ 2;
        Ok   = D > 1e-14 * (Sw .* SwT2 + 1);

        B       = zeros(1, Len);
        A       = zeros(1, Len);
        B(Ok)   = (Sw(Ok) .* SwTM(Ok) - SwT(Ok) .* SwM(Ok)) ./ D(Ok);
        A(Ok)   = (SwM(Ok) - B(Ok) .* SwT(Ok))               ./ Sw(Ok);
        A(~Ok)  = SwM(~Ok) ./ Sw(~Ok);

        CostMat(I, Jj) = single(max(0, SwM2 - A .* SwM - B .* SwTM));
    end

    %----------------------------------------------------------------------
    % Dynamic programming  — J-loop fully vectorised
    %
    %      Dp(K,J) = minimum total chi^2 using K segments for pts 1..J
    %      Bp(K,J) = start index of the K-th segment when ending at J
    %
    %      Base  : Dp(1,J) = CostMat(1,J)          [vector assign]
    %      Step  : Dp(K,J) = min_I [Dp(K-1,I-1) + CostMat(I,J)]
    %
    %      Key insight: for fixed K, ILo = (K-1)*Mp+1 is constant.
    %      Extracting the sub-matrix
    %        H(i,j) = Dp(K-1, ILo+i-2) + CostMat(ILo+i-1, Jstart+j-1)
    %      and calling min(H,[],1) solves ALL J values simultaneously
    %      with a single C-level operation, eliminating the O(n)-iteration
    %      MATLAB J-loop.
    %
    %      The upper triangle of H (invalid short segments) is already
    %      inf in CostMat, so no masking is required.
    %----------------------------------------------------------------------
    Dp = inf(Ns, N);
    Bp = zeros(Ns, N);

    Dp(1, Mp:N) = CostMat(1, Mp:N);
    Bp(1, Mp:N) = 1;

    for K = 2:Ns
        ILo    = (K - 1) * Mp + 1;
        IHi    = N - Mp + 1;
        Jstart = K * Mp;

        if ILo > IHi, continue; end

        % Pvec(i) = accumulated cost of the first K-1 segments when the
        %           K-th segment starts at index ILo+i-1.  (nI x 1)
        Pvec = Dp(K-1, ILo-1 : IHi-1)';

        % Sub(i,j) = chi^2 of segment [ILo+i-1 .. Jstart+j-1].  (nI x nJ)
        Sub  = double(CostMat(ILo:IHi, Jstart:N));

        % Minimise total cost over all valid start indices I for each J.
        [MinVals, MinIdx] = min(Pvec + Sub, [], 1);

        Dp(K, Jstart:N) = MinVals;
        Bp(K, Jstart:N) = ILo - 1 + MinIdx;
    end

    %----------------------------------------------------------------------
    % Backtrack to recover segment boundaries
    %----------------------------------------------------------------------
    SegStart      = zeros(1, Ns);
    SegEnd        = zeros(1, Ns);
    SegEnd(Ns)    = N;
    for K = Ns:-1:2
        SegStart(K)   = Bp(K, SegEnd(K));
        SegEnd(K - 1) = SegStart(K) - 1;
    end
    SegStart(1) = 1;

    %----------------------------------------------------------------------
    % Re-derive parameters and statistics for each segment
    %----------------------------------------------------------------------
    Out = struct('Seg',  zeros(2,Ns), 'SegErr', zeros(2,Ns), ...
                 'Chi2', zeros(1,Ns), 'Dof',    zeros(1,Ns), ...
                 'Npt',  zeros(1,Ns), 'Ind',    zeros(2,Ns), ...
                 'Tlim', zeros(2,Ns));

    for K = 1:Ns
        I1   = SegStart(K);
        I2   = SegEnd(K);
        NptK = I2 - I1 + 1;

        Sw   = Pw(I2  + 1) - Pw(I1);
        SwT  = PwT(I2 + 1) - PwT(I1);
        SwT2 = PwT2(I2+ 1) - PwT2(I1);
        SwM  = PwM(I2 + 1) - PwM(I1);
        SwTM = PwTM(I2+ 1) - PwTM(I1);
        SwM2 = PwM2(I2+ 1) - PwM2(I1);

        D = Sw * SwT2 - SwT ^ 2;

        if D > 1e-14 * (Sw * SwT2 + 1)
            Slope     = (Sw  * SwTM - SwT * SwM) / D;
            Intercept = (SwM - Slope * SwT)       / Sw;
            ErrSlope  = sqrt(Sw   / D);
            ErrInter  = sqrt(SwT2 / D);
        else
            Slope     = 0;
            Intercept = SwM / Sw;
            ErrSlope  = 0;
            ErrInter  = sqrt(1 / Sw);
        end

        Chi2K = max(0, SwM2 - Intercept * SwM - Slope * SwTM);
        DofK  = max(1, NptK - 2);

        Out.Seg(:, K)    = [Slope; Intercept];
        Out.SegErr(:, K) = [ErrSlope; ErrInter];
        Out.Chi2(K)      = Chi2K;
        Out.Dof(K)       = DofK;
        Out.Npt(K)       = NptK;
        Out.Ind(:, K)    = [I1; I2];
        Out.Tlim(:, K)   = [T(I1); T(I2)];
    end

end  % fitOneSource


%==========================================================================
%  Local helper: expand ErrorM to a full (N x S) positive matrix
%==========================================================================
function ErrFull = expandErrors(ErrorM, N, S)
% expandErrors: Broadcast ErrorM to an (N x S) sigma matrix.

    E = double(ErrorM);

    if isscalar(E)
        ErrFull = repmat(abs(E), N, S);

    elseif isvector(E)
        E = E(:);                       % force column
        if numel(E) == N
            ErrFull = repmat(E, 1, S);  % same error curve for every source
        elseif numel(E) == S
            ErrFull = repmat(E', N, 1); % scalar error per source
        else
            error('fitPiecewiseLinear:badErrorSize', ...
                  'ErrorM vector has length %d; expected N=%d or S=%d.', ...
                  numel(E), N, S);
        end

    elseif isequal(size(E), [N, S])
        ErrFull = E;

    else
        error('fitPiecewiseLinear:badErrorSize', ...
              'ErrorM must be scalar, length-N vector, length-S vector, or [%d x %d] matrix.', ...
              N, S);
    end

    % Guard against non-positive errors to avoid Inf weights
    if any(ErrFull(:) <= 0)
        warning('fitPiecewiseLinear:nonPositiveError', ...
                'Some errors are <= 0; replacing with eps to avoid Inf weights.');
        ErrFull(ErrFull <= 0) = eps;
    end

end
