function Result = fitPoissPSF(Image, X, Y, Back, Args)
% Poisson (Cash-statistic) PSF photometry and source-detection test.
% Description: For one or more source positions in a low-count (Poisson) image,
%              fit the PSF amplitude (flux) using the correct Poisson likelihood
%              and test the hypothesis that a source is present at the position.
%              The pixel model is mu_i = Back + F*P_i, with the PSF P fixed and
%              normalised (sum P = 1) and F (total source counts) the only
%              amplitude parameter. The maximum-likelihood flux Fhat solves
%                  sum_i n_i P_i/(Back+F P_i) = sum_i P_i ,
%              pinned at Fhat=0 when no positive-flux solution exists (the F>=0
%              boundary). The detection statistic is the profiled
%              log-likelihood ratio (equivalently the Cash/Wilks statistic)
%                  S = sum_i [ n_i ln(1+Fhat P_i/Back) - Fhat P_i ],
%              i.e. how much better a best-fit PSF explains the counts than
%              background alone. This is amplitude-only PSF fitting with the
%              Poisson noise model (the low-count-correct replacement for
%              chi^2/Gaussian PSF fitting), and the detection test is the
%              significance of the fitted amplitude.
%              The probability that the measured flux is a background
%              fluctuation (the false-alarm probability / p-value) is
%                  p = P( q_null >= 2*S | background only ),
%              computed by default from a Monte-Carlo null distribution of the
%              SAME profiled statistic (exact at low counts, where the
%              asymptotic 1/2 chi^2_1 result is invalid), or via that asymptotic
%              with 'PvalMethod'='chernoff'. The null depends only on Back, the
%              PSF and the search settings, so it is built once and reused for
%              all input positions.
% Input  : - Image : 2-D image of counts, indexed (Y=row, X=column).
%          - X : source X coordinate(s) (column index). Scalar or vector.
%          - Y : source Y coordinate(s) (row index). Scalar or vector,
%                same length as X.
%          - Back : scalar background level per pixel [counts].
%          * ...,key,val,... :
%            'PSF'        - PSF stamp, an odd-by-odd matrix whose central pixel
%                           is the PSF centre; assumed to sum to 1. REQUIRED.
%            'FixedPos'   - If true (default) X,Y are taken as exact and only
%                           the flux is fitted (nearest-pixel stamp). If false,
%                           the integer-pixel position is also searched within
%                           'MaxDeltaPos' and the best-S position is returned.
%            'MaxDeltaPos'- Max position shift [pixels] when FixedPos=false.
%                           Search is over integer offsets with
%                           sqrt(dx^2+dy^2) <= MaxDeltaPos. Default 1.
%            'PvalMethod' - 'mc' (default) Monte-Carlo null of the profiled
%                           statistic (selection-aware, exact at low counts);
%                           'fft' analytic null of the kernel statistic
%                           S_kernel(Fhat)=sum n_i ln(1+Fhat P_i/Back) built at
%                           the fitted Fhat via the compound-Poisson FFT method
%                           (same engine as
%                           imUtil.poissNoise.poiss_f_threshold_analytical_fft) -
%                           fast and analytic, but because the template flux is
%                           selected from the data it is mildly anti-conservative
%                           (slightly understates p) near the detection limit;
%                           'chernoff' the asymptotic p=0.5*erfc(sqrt(S)) (fast,
%                           inaccurate at very low counts); 'none' to skip.
%            'Nsim'       - Number of Monte-Carlo simulations ('mc' only).
%                           Default 1e5 (limits the smallest p to ~1/Nsim).
%            'FFTkmax'    - Max photon number for the 'fft' null. [] = auto
%                           (captures the Poisson tail). Default [].
%            'FFTdx'      - Grid spacing for the 'fft' null. [] = auto. Def [].
%            'ChunkSize'  - MC simulation chunk size (memory control). Def 2e4.
%            'Seed'       - RNG seed for the MC null (for reproducibility). Def [].
%            'Verbose'    - Print progress. Default false.
% Output : - Result : struct of column-vector fields, one entry per position:
%                .Flux    - best-fit flux Fhat [counts] (0 at the F>=0 boundary)
%                .FluxErr - 1-sigma flux error from the Fisher information,
%                           1/sqrt(sum P_i^2/(Back+Fhat P_i)) (at the best pos)
%                .S       - profiled log-likelihood-ratio statistic S(Fhat)
%                .Pvalue  - probability the flux is a background fluctuation
%                .Xfit, .Yfit - position used/fitted (=input if FixedPos)
%                .Nsigma  - sqrt(2*S), a convenience Gaussian-equivalent score
%                .Edge    - true if the stamp was trimmed at the image edge
%                           (then Pvalue is approximate; null assumes interior)
%                .PvalMethod, .Nsim - bookkeeping
%                .qNull   - the MC null statistic vector (if PvalMethod='mc')
% License: GNU general public license version 3
% Tested : Matlab R2021b
%     By : (statistical method & MATLAB implementation)             Jun 2026
%    URL :
% Example: P = imUtil.kernel2.gauss(1.3, [7 7]); P = P./sum(P(:));  % or any PSF
%          Image = poissrnd(0.05, 256, 256);
%          xs = 128; ys = 128;
%          Image(ys-3:ys+3, xs-3:xs+3) = Image(ys-3:ys+3, xs-3:xs+3) + ...
%                                        poissrnd(8*P);
%          R = imUtil.poissNoise.fitPoissPSF(Image, xs, ys, 0.05, 'PSF',P);
%          % R.Flux, R.FluxErr, R.S, R.Pvalue
%          % allow +-1 pix position search:
%          R2 = imUtil.poissNoise.fitPoissPSF(Image, xs, ys, 0.05, 'PSF',P, 'FixedPos',false);
% Reliable: 2
%--------------------------------------------------------------------------

    arguments
        Image                {mustBeNumeric}
        X                    {mustBeNumeric}
        Y                    {mustBeNumeric}
        Back        (1,1)    {mustBeNumeric, mustBePositive}
        Args.PSF             {mustBeNumeric} = []
        Args.FixedPos        = true
        Args.MaxDeltaPos     = 1
        Args.PvalMethod char = 'mc'
        Args.Nsim            = 1e5
        Args.ChunkSize       = 2e4
        Args.FFTkmax         = []
        Args.FFTdx           = []
        Args.Seed            = []
        Args.Verbose         = false
    end

    P = Args.PSF;
    if isempty(P)
        error('PSF is required (pass ''PSF'',stamp).');
    end
    [np_y, np_x] = size(P);
    if mod(np_y,2)==0 || mod(np_x,2)==0
        error('PSF must have odd dimensions (central pixel = PSF centre).');
    end
    hy = (np_y - 1)/2;
    hx = (np_x - 1)/2;

    X = X(:);  Y = Y(:);
    if numel(X) ~= numel(Y)
        error('X and Y must have the same number of elements.');
    end
    M = numel(X);
    B = Back;

    % integer-pixel position offsets to search (FixedPos=false)
    if Args.FixedPos
        offs = [0 0];
    else
        mo = floor(Args.MaxDeltaPos + 1e-9);
        [DX, DY] = meshgrid(-mo:mo, -mo:mo);
        keep = (DX.^2 + DY.^2) <= Args.MaxDeltaPos^2 + 1e-9;
        offs = [DX(keep), DY(keep)];
    end

    % -------- per-source fit --------
    Flux   = nan(M,1);  FluxErr = nan(M,1);  Sstat = nan(M,1);
    Xfit   = nan(M,1);  Yfit    = nan(M,1);  Edge  = false(M,1);
    bestN  = cell(M,1); bestP   = cell(M,1);

    for s = 1:M
        xc0 = round(X(s));  yc0 = round(Y(s));
        bestS = -Inf; bestF = 0; bestSig = NaN; bx = xc0; by = yc0; bedge = false;
        bn = []; bp = [];
        for io = 1:size(offs,1)
            xc = xc0 + offs(io,1);
            yc = yc0 + offs(io,2);
            [nvec, Pvec, isedge] = extract_stamp(Image, P, xc, yc, hx, hy);
            if isempty(nvec)
                continue;
            end
            [F, S, sig] = fit_flux(nvec, Pvec, B);
            if S > bestS
                bestS = S; bestF = F; bestSig = sig; bx = xc; by = yc; bedge = isedge;
                bn = nvec; bp = Pvec;
            end
        end
        Flux(s)=bestF; FluxErr(s)=bestSig; Sstat(s)=bestS;
        Xfit(s)=bx; Yfit(s)=by; Edge(s)=bedge;
        bestN{s}=bn; bestP{s}=bp;
    end

    Result = struct();
    Result.Flux    = Flux;
    Result.FluxErr = FluxErr;
    Result.S       = Sstat;
    Result.Xfit    = Xfit;
    Result.Yfit    = Yfit;
    Result.Nsigma  = sqrt(max(2*Sstat,0));
    Result.Edge    = Edge;
    Result.PvalMethod = Args.PvalMethod;
    Result.Nsim    = Args.Nsim;

    % -------- p-value --------
    switch lower(Args.PvalMethod)
        case 'none'
            Result.Pvalue = nan(M,1);

        case 'fft'
            % Analytic null of the kernel statistic S_kernel(Fhat) built at the
            % fitted Fhat, via the compound-Poisson FFT method. Per source: the
            % kernel uses the actual stamp pixels (so edge-trimmed stamps are
            % handled exactly). p = P(S_kernel >= S_kernel,obs) from the FFT
            % survival function.
            p = ones(M,1);
            for s = 1:M
                Fs = Flux(s);
                nv = bestN{s};  Pv = bestP{s};
                if isempty(nv) || Fs <= 0
                    p(s) = 1;    % no positive-flux detection
                    continue;
                end
                kernel = log(1 + Fs .* Pv ./ B);
                Sobs   = sum(nv .* kernel);
                if Sobs <= 0
                    p(s) = 1; continue;
                end
                [xg, sf] = fft_kernel_sf(kernel, B, Args.FFTkmax, Args.FFTdx);
                if Sobs <= xg(1)
                    p(s) = min(1, sf(1));
                elseif Sobs >= xg(end)
                    p(s) = max(sf(end), realmin);   % beyond grid: tail floor
                else
                    p(s) = interp1(xg, sf, Sobs, 'linear');
                end
                p(s) = min(max(p(s), realmin), 1);
            end
            Result.Pvalue = p;

        case 'chernoff'
            % asymptotic 1/2 chi^2_1 (Chernoff, boundary at F>=0):
            % p = 0.5*P(chi^2_1 >= 2S) = 0.5*erfc(sqrt(S)).  FixedPos only.
            if ~Args.FixedPos
                warning('fitPoissPSF:chernoffPos', ...
                    ['chernoff p-value ignores the position search; ' ...
                     'use PvalMethod=''mc'' for FixedPos=false.']);
            end
            p = 0.5 * erfc(sqrt(max(Sstat,0)));
            p(Sstat<=0) = 1;
            Result.Pvalue = p;

        case 'mc'
            % full PSF template (interior source) null, shared across sources.
            % Look-elsewhere from the (small) position search is ignored, so the
            % null is built at fixed position (single offset).
            qnull = mc_null(P(:).', B, [0 0], hx, hy, np_y, np_x, ...
                            Args.Nsim, Args.ChunkSize, Args.Seed, Args.Verbose);
            qnull = sort(qnull);
            Nn = numel(qnull);
            qobs = 2*Sstat;
            p = ones(M,1);
            for s = 1:M
                if qobs(s) > 0
                    % number of null >= qobs via binary search on sorted qnull
                    k = Nn - upper_count_lt(qnull, qobs(s));
                    p(s) = (1 + k) / (1 + Nn);
                else
                    p(s) = 1;
                end
            end
            Result.Pvalue = p;
            Result.qNull  = qnull;

        otherwise
            error('Unknown PvalMethod "%s".', Args.PvalMethod);
    end
end

% =====================================================================
% Extract the in-image overlap of an N x N stamp centred at (xc,yc) and the
% matching PSF values. Returns column vectors and an edge-trim flag.
% =====================================================================
function [nvec, Pvec, isedge] = extract_stamp(Image, P, xc, yc, hx, hy)
    [Ny, Nx] = size(Image);
    r0 = yc - hy;  r1 = yc + hy;
    c0 = xc - hx;  c1 = xc + hx;
    rr0 = max(1,r0); rr1 = min(Ny,r1);
    cc0 = max(1,c0); cc1 = min(Nx,c1);
    if rr0 > rr1 || cc0 > cc1
        nvec = []; Pvec = []; isedge = true; return;
    end
    isedge = (rr0~=r0) || (rr1~=r1) || (cc0~=c0) || (cc1~=c1);
    sub  = Image(rr0:rr1, cc0:cc1);
    Psub = P(rr0-r0+1:rr1-r0+1, cc0-c0+1:cc1-c0+1);
    nvec = sub(:);
    Pvec = Psub(:);
end

% =====================================================================
% Maximum-likelihood Poisson PSF flux at fixed position.
%   Fhat solves sum n_i P_i/(B+F P_i) = sum P_i, pinned at 0 on the boundary.
%   S = sum n_i log(1+F P_i/B) - F sum P_i  (profiled LLR)
%   sigF = 1/sqrt(sum P_i^2/(B+F P_i))      (expected Fisher information)
% Bisection (monotone score), no toolbox required.
% =====================================================================
function [F, S, sigF] = fit_flux(n, P, B)
    n = n(:); P = P(:);
    psum = sum(P);
    Snp  = sum(n .* P);
    if Snp <= B * psum
        F = 0;
    else
        g = @(F) sum(n .* P ./ (B + F .* P)) - psum;
        lo = 0; hi = 1;
        while g(hi) > 0 && hi < 1e12
            hi = hi * 2;
        end
        for it = 1:200
            mid = 0.5*(lo+hi);
            if g(mid) > 0, lo = mid; else, hi = mid; end
            if (hi - lo) < 1e-10 * max(1,hi), break; end
        end
        F = 0.5*(lo+hi);
    end
    S    = sum(n .* log(1 + F .* P ./ B)) - F * psum;
    fish = sum(P.^2 ./ (B + F .* P));
    sigF = 1 / sqrt(fish);
end

% =====================================================================
% Monte-Carlo null distribution of q=2S under background only, using the
% full PSF template. Handles the position search (FixedPos=false) by
% simulating a margin region and maximising q over the same offsets.
% Vectorised over simulations, chunked for memory.
% =====================================================================
function qnull = mc_null(P, B, offs, hx, hy, npy, npx, Nsim, chunk, seed, verbose)
    if ~isempty(seed)
        rng_state = rng_local(seed); %#ok<NASGU>
    end
    Nsim  = round(Nsim);
    noff  = size(offs,1);
    mo_x  = max(abs(offs(:,1)));
    mo_y  = max(abs(offs(:,2)));
    % simulation region big enough to hold every offset stamp
    Rny = npy + 2*mo_y;
    Rnx = npx + 2*mo_x;
    cyR = mo_y + hy + 1;     % region-centre row (1-based)
    cxR = mo_x + hx + 1;     % region-centre col
    % precompute, per offset, the linear indices of the N x N sub-block
    subidx = cell(noff,1);
    for io = 1:noff
        r0 = cyR + offs(io,2) - hy;
        c0 = cxR + offs(io,1) - hx;
        [CC, RR] = meshgrid(c0:c0+npx-1, r0:r0+npy-1);
        subidx{io} = sub2ind([Rny, Rnx], RR(:), CC(:)).';   % 1 x Npix
    end
    Pvec = P;                       % 1 x Npix (already row)
    psum = sum(Pvec);
    Npix = numel(Pvec);

    qnull = zeros(Nsim,1);
    done = 0;
    while done < Nsim
        nb = min(chunk, Nsim - done);
        Creg = poissrnd_local(B, nb, Rny*Rnx);    % nb x (Rny*Rnx)
        qmax = zeros(nb,1);
        for io = 1:noff
            C = Creg(:, subidx{io});               % nb x Npix
            q = profiled_q(C, Pvec, psum, Npix, B);
            if io == 1
                qmax = q;
            else
                qmax = max(qmax, q);
            end
        end
        qnull(done+1:done+nb) = qmax;
        done = done + nb;
        if verbose
            fprintf('  MC null: %d / %d\n', done, Nsim);
        end
    end
end

% vectorised profiled q = 2S for a block of count-stamps C (Nb x Npix)
function q = profiled_q(C, P, psum, Npix, B)
    Snp = C * P.';                        % Nb x 1
    Nb  = size(C,1);
    F   = zeros(Nb,1);
    act = Snp > B*psum;
    if any(act)
        Ca = C(act,:);
        Fa = max(1e-8, (sum(Ca,2) - B*Npix)./psum);
        for it = 1:80
            den = B + Fa .* P;            % na x Npix (implicit expansion)
            g   = sum(Ca .* P ./ den, 2) - psum;
            gp  = -sum(Ca .* (P.^2) ./ den.^2, 2);
            step = g ./ gp;
            Fa  = Fa - step;
            Fa(Fa < 0) = 0;
        end
        F(act) = Fa;
    end
    q = 2*(sum(C .* log(1 + F .* P ./ B), 2) - F .* psum);
    q(q < 0) = 0;
end

% number of sorted-vector entries strictly less than v (binary search)
function c = upper_count_lt(sorted_vec, v)
    lo = 0; hi = numel(sorted_vec);
    while lo < hi
        mid = floor((lo+hi)/2) + 1;
        if sorted_vec(mid) < v
            lo = mid;
        else
            hi = mid - 1;
        end
    end
    c = lo;
end

% --- small toolbox-free helpers ---
function s = rng_local(seed)
    s = [];
    try, rng(seed); catch, rand('seed',seed); randn('seed',seed); end
end

function out = poissrnd_local(lam, m, n)
    % Poisson(lam) random integers, scalar lam, size m x n.
    % Knuth for small lam (this routine targets the low-count regime).
    N = m*n;
    out = zeros(N,1);
    L = exp(-lam);
    for i = 1:N
        k = 0; p = 1;
        while true
            k = k + 1;
            p = p * rand();
            if p <= L, break; end
        end
        out(i) = k - 1;
    end
    out = reshape(out, m, n);
end

% =====================================================================
% FFT null survival function of the kernel statistic S = sum n_i kernel_i,
% where each pixel is Poisson(B). Compound-Poisson construction: bin the
% one-photon kernel values, k-fold self-convolve via FFT, Poisson-mix over
% photon number. Returns the grid x and survival sf(i)=P(S>=x(i)).
% This is the same engine as imUtil.poissNoise.pdf_from_kpmf_fft (validated
% against it); in the package, call that shared function instead.
% =====================================================================
function [x, sf] = fft_kernel_sf(kernel, B, kmax, dx)
    pixels = kernel(:);
    pixels = pixels(~isnan(pixels));
    len = numel(pixels);
    mu_tot = len * B;

    % auto kmax: capture the Poisson tail well below any tail probability
    if isempty(kmax)
        kmax = 6;
        while poisssf_local(kmax, mu_tot) > 1e-12 && kmax < 80
            kmax = kmax + 1;
        end
    end
    pmin = min(pixels); pmax = max(pixels);
    if isempty(dx)
        if pmax == pmin
            dx = 1.0;
        else
            dx = (pmax - pmin) / 2000;
            if dx <= 0, dx = 1.0; end
        end
    end

    B0 = ceil(max(0.0, -pmin / dx));
    j  = round_half_even(pixels / dx) + B0;     % 0-based indices
    J  = max(j);
    p1 = accumarray(j + 1, 1, [J + 1, 1], @sum, 0);
    p1 = p1 / sum(p1);

    global_len = kmax * J + 1;
    nfft = pow2(nextpow2_len(global_len));
    F1 = fft(p1(:).', nfft);

    m = (0:global_len - 1);
    x = (dx * (m - kmax * B0)).';

    ks = (0:kmax);
    pw = poisspdf_local(ks, mu_tot);

    pmf = zeros(global_len, 1);
    for k = 0:kmax
        if k == 0
            pk = 1.0;
        else
            pk_full = real(ifft(F1 .^ k, nfft));
            pk = pk_full(1:(k * J + 1));
            pk = max(pk, 0.0);
            sden = sum(pk);
            if sden > 0, pk = pk / sden; end
        end
        pk = pk(:);
        off = (kmax - k) * B0;
        emb = zeros(global_len, 1);
        emb(off + 1 : off + numel(pk)) = pk;
        pmf = pmf + pw(k + 1) * emb;
    end
    retained = sum(pw);
    if retained > 0, pmf = pmf / retained; end

    sf = flipud(cumsum(flipud(pmf)));
end

function y = round_half_even(x)
    y = round(x);
    d = x - floor(x);
    half = (abs(d - 0.5) < 1e-12);
    yeven = 2 * round(x / 2);
    y(half) = yeven(half);
end

function n = nextpow2_len(L)
    if L <= 1
        n = 0;
    else
        n = ceil(log2(L));
        if 2^n < L, n = n + 1; end
    end
end

function p = poisspdf_local(k, mu)
    p = exp(-mu) .* mu .^ k ./ factorial(k);
end

function s = poisssf_local(k, mu)
    s = 1 - sum(poisspdf_local(0:k, mu));
    if s < 0, s = 0; end
end