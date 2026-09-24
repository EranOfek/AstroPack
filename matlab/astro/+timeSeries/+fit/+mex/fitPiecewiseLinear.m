% fitPiecewiseLinear: Fast mex for Globally optimal weighted N-segment piecewise linear fit.
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
% Author : Claude code + Eran Ofek (May 2026)
% Example: N=200; T=(1:N)'; sigma=0.3;
%          Msig = [1+T; -2*T+300] + sigma*randn(N,2);  % two sources
%          Res = timeSeries.fit.mex.fitPiecewiseLinear(T, Msig, sigma, 'Nseg', 3);
%          disp(Res(1).Seg)   % slopes and intercepts for source 1