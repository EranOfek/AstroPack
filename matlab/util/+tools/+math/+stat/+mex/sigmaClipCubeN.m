% Iterative sigma-clipped (weighted) mean and variance of a cube
% Description:
%   Stacks K images stored in the third dimension of a 3-D cube by computing
%   the sigma-clipped mean and variance at every pixel position independently.
%   NaN values in the cube are silently ignored throughout.
%
%   The function is a compiled MEX file (C++/AVX2) and is significantly
%   faster than the equivalent pure-MATLAB code.  The MATLAB analogue for
%   the unweighted, 2-iteration case is:
%
%       MA = mean(A, 3, 'omitnan');
%       SA = std(A,  [], 3, 'omitnan');
%       Z  = (A - MA) ./ SA;
%       A(Z < -LowNs | Z > HighNs) = NaN;
%       MA = mean(A, 3, 'omitnan');
%       NN = sum(~isnan(A), 3);
%
% Algorithm:
%   1. INITIAL PASS  (always performed)
%      Welford's numerically stable online algorithm scans all K slices once
%      and accumulates, for each pixel, the weighted count W, the weighted
%      mean mu, and the weighted second-moment M2:
%
%        for each slice k with weight w_k > 0 and pixel value x (not NaN):
%          W    += w_k
%          d1    = x - mu_old
%          mu   += (w_k / W) * d1
%          M2   += w_k * d1 * (x - mu_new)
%          cnt  += 1          % integer frame count (for Nused and convergence)
%
%      This is West (1979) / Knuth's algorithm.  All accumulation is performed
%      in double precision even when the input cube is single, eliminating the
%      catastrophic cancellation that afflicts the classical (sum - sum^2/n)
%      formula for large pixel values (e.g. 16-bit detectors at ~50 000 ADU).
%
%   2. SIGMA-CLIP PASSES  (Niter-1 passes after the initial pass)
%      For each subsequent iteration:
%        a. Compute per-pixel standard deviation from the previous iteration:
%             sd = sqrt(M2 / W)          % weighted (biased)
%             sd = sqrt(M2 / (cnt-1))    % unweighted (Bessel-corrected)
%        b. Re-scan all K slices.  A value x at pixel p is admitted only if:
%             x >= mu[p] - LowNs  * sd[p]   AND
%             x <= mu[p] + HighNs * sd[p]   AND
%             x is not NaN
%        c. Rebuild mu, M2, W, cnt via the same weighted Welford update on
%           the surviving values.
%        d. If no pixel's integer count changed from the previous iteration,
%           exit early (converged).
%
%   3. OUTPUT
%      After the final iteration:
%        Img   = mu
%        Var   = M2 / W         (weighted, biased)
%              = M2 / (cnt-1)   (unweighted, Bessel-corrected)
%        Nused = cnt            (integer count, independent of weights)
%
% Input  : - Cube   : M x N x K array of class single or double.
%                     K images to stack; the stacking axis is dimension 3.
%          - [LowNs, HighNs] : 1x2 real vector.
%                     Lower and upper sigma-rejection thresholds (both >= 0).
%                     Example: [2.5, 2.5] rejects pixels more than 2.5 sigma
%                     from the mean on either side.
%          - Niter  : Positive integer scalar.
%                     Number of iterations:
%                       1  - No clipping.  Returns mean and variance of all
%                            valid (non-NaN) pixels.
%                       2  - One sigma-clip pass.  Equivalent to the behaviour
%                            of the previous sigma_clip_cube_fast.
%                       N  - N-1 sigma-clip passes.  Iterations stop early
%                            when no pixel count changes between passes.
%          - Weights : K-element real vector  (optional, default = []).
%                     Per-slice scalar weights.  Element k is the weight
%                     applied to every pixel in slice k.  Rules:
%                       - [] or omitted : all weights equal 1 (unweighted).
%                       - Values must be finite and >= 0.
%                       - Slices with weight 0 are skipped entirely.
%                     When weights are supplied the outputs Img and Var are
%                     the weighted mean and weighted biased variance.
%                     Nused always counts integer frames regardless of weight.
%
% Output : - Img   : M x N array (same class as Cube).
%                    Sigma-clipped (weighted) mean image.
%                    NaN where no valid pixels survived all iterations.
%          - Var   : M x N array (same class as Cube)  [optional].
%                    Per-pixel variance of the final iteration.
%                      Unweighted: sample variance  M2 / (Nused - 1)
%                      Weighted:   biased weighted variance  M2w / sum(w)
%                    NaN where Img is NaN.
%          - Nused : M x N double array  [optional].
%                    Number of frames admitted at each pixel in the final
%                    iteration.  Integer-valued, independent of Weights.
%
% Requires: AVX2 instruction set (Intel Haswell 2013+, AMD Zen 2019+).
%           Compile with:
%             mex CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -mavx2 -mfma -fopenmp' ...
%                 LDFLAGS='$LDFLAGS -fopenmp' sigmaClipCubeN.cpp
%
% Author : Eran Ofek  (Jan 2026); rewritten with Welford algorithm and
%          optional weights by (Apr 2026)
% Example:
%   % Basic 2-pass sigma clip (equivalent to sigma_clip_cube_fast)
%   A = randn(512, 512, 20, 'single');
%   [M, V, N] = sigmaClipCubeN(A, [2.5 2.5], 2);
%
%   % Iterative clip until convergence (up to 5 passes)
%   [M, V, N] = sigmaClipCubeN(A, [3 3], 5);
%
%   % Weighted stack: down-weight noisy frames
%   w = 1 ./ [1 1 1 2 2 2 3 3 3 4 4 4 2 2 2 1 1 1 1 1];   % example weights
%   [M, V, N] = sigmaClipCubeN(A, [2.5 2.5], 3, w);
%
%   % No clipping, just weighted mean and variance
%   [M, V] = sigmaClipCubeN(A, [0 0], 1, w);