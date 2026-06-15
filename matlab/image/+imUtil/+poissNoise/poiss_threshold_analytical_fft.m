function [Flim, Slim_analytical, Status, x, pmf] = poiss_threshold_analytical_fft(bck, psf, Args)
% Fast (FFT) analytical flux threshold for a Poisson matched filter.
% Description: Compute the flux threshold Fth given a PSF, background level and
%              false-alarm probability gamma, using the analytical scheme, but
%              evaluating the null (background-only) distribution of the
%              matched-filter statistic
%                  S = sum_i n_i * log(1 + F*P_i/B)
%              with an FFT-based compound-Poisson construction instead of the
%              exact combinatorial enumeration. The one-photon values are
%              quantised onto a uniform grid of width dx; the k-photon
%              distribution is the k-fold self-convolution of the one-photon
%              PMF (via FFT), and these are mixed with Poisson(mu_tot) weights
%              over photon number k=0..kmax. An adaptive driver refines dx and
%              kmax until the threshold passes a set of convergence/validation
%              checks. This is much faster than the exact-combinatorics routine
%              poiss_f_threshold_analytical, and converges to it as dx->0.
%              MATLAB port of the X-sifter Python function of the same name.
% Input  : - bck : mean background level per pixel [counts]. Scalar > 0.
%          - psf : PSF stamp, a 2-D matrix P. NaNs are ignored.
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'gamma'        - False-alarm probability (alpha). No default
%                             (required).
%            'Sigma_guess'  - Gaussian sigma of the PSF, used only to set the
%                             first guess of the flux threshold via
%                             Flim0 = poissinv(1-gamma, 4*pi*Sigma^2*bck).
%                             No default (required).
%            'Nsim'         - Number of simulations, used to convert the PMF
%                             into expected counts. No default (required).
%            'Stampsize'    - Used only in verbose printing. Default is [].
%            'step'         - Accepted for parity with the source; unused.
%                             Default is 1.
%            'ConvAccuracy' - Fixed-point tolerance on |Flim_new-Flim_old|.
%                             Default is 0.03.
%            'ConvMaxIter'  - Maximum number of fixed-point iterations.
%                             Default is 10.
%            'Verbose'      - Print progress. Default is true.
%            'unique_F'     - Accepted for parity with the source; unused.
%                             Default is false.
%            'bins_number'  - Accepted for parity with the source; unused in
%                             the FFT path. Default is 200.
%            'six'          - Compatibility flag passed to the FFT routines.
%                             Default is false.
%            'FixBug'       - Correct the latent bug by which the *returned*
%                             threshold variable was not zeroed/NaN-ed in the
%                             degenerate branch (see "Bug fix" below). Set to
%                             false to reproduce the original Python behaviour
%                             bit-for-bit. Default is true.
% Output : - Flim : Flux threshold Fth. NaN if it collapses to 0 (not converged).
%          - Slim_analytical : The threshold in units of the matched-filter
%            statistic S (from the FFT survival function).
%          - Status : true if converged within ConvMaxIter, false otherwise.
%          - x : Bin centres of the FFT output grid for S.
%          - pmf : Probability mass on x (the binned null PMF of S).
% Bug fix: As in poiss_f_threshold_analytical, the degenerate branch
%              if Slim_analytical == min(x)
%                  Slim  = 0;   % assigns a variable that is NEVER returned
%                  Flim1 = 0;
%              end
%          sets a local 'Slim' rather than the returned 'Slim_analytical'. With
%          'FixBug'=true (default) the returned Slim_analytical is set to NaN
%          consistently with Flim; 'FixBug'=false reproduces the original.
% Tested : Matlab R2021b
% Author : Maayane T. Soumagnac (Python original, Nov 2025); Claude + Eran Ofek MATLAB port  (Jun 2026)
%    URL : https://github.com/maayane/X-sifter
% Example: yy = (-2:2).'; xx = (-2:2); [X,Y] = meshgrid(xx,yy);
%          P  = exp(-(X.^2+Y.^2)./(2*1.1^2)); P = P./sum(P(:));
%          [Flim,Slim,Status] = imUtil.poissNoise.poiss_threshold_analytical_fft(0.05, P, ...
%               'gamma',0.01, 'Sigma_guess',1.1, 'Nsim',1e6);

    arguments
        bck                       {mustBeNumeric, mustBeScalarOrEmpty}
        psf                       {mustBeNumeric}
        Args.gamma          = []; % required; validated below
        Args.Sigma_guess    = []; % required; validated below
        Args.Nsim           = []; % required; validated below
        Args.Stampsize      = [];
        Args.step           = 1;       % accepted for parity with source; unused
        Args.ConvAccuracy   = 0.03;
        Args.ConvMaxIter    = 10;
        Args.Verbose        = true;
        Args.unique_F       = false;   % accepted for parity with source; unused
        Args.bins_number    = 200;     % accepted for parity with source; unused
        Args.six            = false;
        Args.FixBug         = true;
    end

    gamma        = Args.gamma;
    Sigma_guess  = Args.Sigma_guess;
    Nsim         = Args.Nsim;
    six          = Args.six;
    Verbose      = Args.Verbose;
    FixBug       = Args.FixBug;
    ConvAccuracy = Args.ConvAccuracy;
    ConvMaxIter  = Args.ConvMaxIter;
    step = Args.step; %#ok<NASGU> % parity: accepted, unused

    if isempty(gamma)
        error('gamma is required (false-alarm probability).');
    end
    if isempty(Sigma_guess)
        error('Sigma_guess is required (used for the initial flux guess).');
    end
    if isempty(Nsim)
        error('Nsim is required.');
    end

    if Verbose
        fprintf(['***** I am calculating the Threshold value, for the computed ' ...
                 'PSF, background and false-alarm *****\n']);
        fprintf('The size of the psf is %s, bck=%g\n', mat2str(Args.Stampsize), bck);
    end

    P = psf;

    % ---------------- first guess for the flux threshold ----------------
    Flimx = poissinv_local(1 - gamma, 4 * pi * Sigma_guess^2 * bck);
    Flim  = max(Flimx, 1);

    converge = false;
    iter = 0;
    Slim = NaN; %#ok<NASGU> % the (unused-in-original) variable, kept for parity

    Slim_analytical = NaN;
    x = [];
    pmf = [];
    Status = true;

    while ~converge
        iter = iter + 1;

        kernel = log(1 + Flim .* (P ./ bck));
        pixels = kernel(:);
        pixels = pixels(~isnan(pixels));
        len = numel(pixels); %#ok<NASGU>

        result = adaptive_fft_threshold(kernel, bck, Nsim, gamma, six, ...
                                        0.01, 6, 20);
        fft_result = result.fft_result;
        x        = fft_result.x;
        pmf      = fft_result.pmf;
        Slim_fft = fft_result.Slim;

        Slim_analytical = Slim_fft;

        sum_pmf = nansum_local(log(1 + Flim .* (P ./ bck)));
        Flim1 = (Slim_analytical - bck * sum_pmf) / FluxNormFun(Flim, P, bck);

        if isnan(Flim1)
            error('Flim1 is nan, solve this before continuing or it will never converge');
        end

        if Slim_analytical == min(x)
            if FixBug
                Slim_analytical = 0;   % FIX: zero the variable that is actually returned
            else
                Slim = 0;              %#ok<NASGU> % ORIGINAL latent bug (unused var)
            end
            Flim1 = 0;
        end

        converge = (abs(Flim1 - Flim) < ConvAccuracy) || (iter > ConvMaxIter);
        Flim = Flim1;

        if Verbose
            fprintf('iteration %d: Flim=%g\n', iter, Flim);
        end

        if iter > ConvMaxIter
            Status = false;
        else
            Status = true;
        end

        if Flim == 0
            Flim = NaN;
            Slim = NaN; %#ok<NASGU>
            if FixBug
                Slim_analytical = NaN;
            end
        end

        if imag(Flim) > 1e-3
            error('The imaginary part of Flim is suspiciously large');
        else
            Flim = real(Flim);
        end
    end

    if Verbose
        fprintf('converged?: %d\n', converge);
    end
    if imag(Slim_analytical) > 1e-3
        error('The imaginary part of Slim is suspiciously large');
    else
        Slim_analytical = real(Slim_analytical);
    end
end

% =====================================================================
% Helper: matched-filter flux normalisation (this is SF)
% =====================================================================
function s = FluxNormFun(Fth, PSF, Back)
    s = nansum_local(PSF .* log(1 + Fth .* (PSF ./ Back)));
end

% =====================================================================
% adaptive_fft_threshold
% Refine dx and kmax until the FFT threshold passes validation.
% Mirrors the Python adaptive_fft_threshold (slim_tol=0.05, sf_rel_tol=1).
% =====================================================================
function out = adaptive_fft_threshold(kpmf, bck, Nsim, alpha, six, ...
                                      dx_initial, kmax_initial, max_iter)
    if nargin < 6 || isempty(six),          six = false;       end
    if nargin < 7 || isempty(dx_initial),   dx_initial = 0.01; end
    if nargin < 8 || isempty(kmax_initial), kmax_initial = 6;  end
    if nargin < 9 || isempty(max_iter),     max_iter = 10;     end

    dx_refine_factor = 2;
    kmax_step        = 10;

    dx   = dx_initial;
    kmax = kmax_initial;

    for i = 1:max_iter
        check = validate_fft_threshold(kpmf, bck, Nsim, alpha, six, kmax, dx, ...
                                       0.05, 1.0);  % slim_tol=0.05, sf_rel_tol=1

        if check.all_ok
            check.fft_result = check.res_base;
            out = check;
            return;
        end

        % 1. Photon-truncation problem -> increase kmax
        if (~check.check_poisson_tail_ok) || (~check.check_slim_kmax_ok)
            kmax = kmax + kmax_step;
            continue;
        end

        % 2. Resolution problem -> reduce dx
        if (~check.check_slim_dx_ok) || (~check.check_sf_region_ok)
            dx = dx / dx_refine_factor;
            continue;
        end

        % fallback
        dx   = dx / dx_refine_factor;
        kmax = kmax + kmax_step;
    end

    error('Adaptive FFT threshold did not converge within max_iter refinements');
end

% =====================================================================
% validate_fft_threshold
% Run pdf_from_kpmf_fft three times (base; refined dx; refined kmax) and
% apply four convergence/trust checks. Mirrors the Python validator
% (plots/prints omitted). Optional args slim_tol and sf_rel_tol override
% the defaults, as used by adaptive_fft_threshold.
% =====================================================================
function out = validate_fft_threshold(kpmf, bck, Nsim, alpha, six, kmax, dx, ...
                                      slim_tol, sf_rel_tol)
    if nargin < 5 || isempty(six),  six = false; end
    if nargin < 6, kmax = []; end
    if nargin < 7, dx   = []; end
    if nargin < 8, slim_tol   = []; end
    if nargin < 9 || isempty(sf_rel_tol), sf_rel_tol = 0.5; end

    nbins_one_photon     = 2000;
    dx_refine_factor     = 2.0;
    kmax_refine_step     = 2;
    sf_abs_tol           = 1e-3;
    poisson_tail_fraction= 0.01;
    region_factor_low    = 0.1;
    region_factor_high   = 10;

    if isempty(kmax)
        if six, kmax = 6; else, kmax = 5; end
    end

    % ---- Base run ----
    res_base = pdf_from_kpmf_fft(kpmf, bck, Nsim, six, kmax, dx, nbins_one_photon, alpha);
    dx_base   = res_base.dx;
    slim_base = res_base.Slim;

    % ---- Refine dx ----
    dx_refined = dx_base / dx_refine_factor;
    res_dx = pdf_from_kpmf_fft(kpmf, bck, Nsim, six, kmax, dx_refined, nbins_one_photon, alpha);
    slim_dx = res_dx.Slim;

    % ---- Refine kmax ----
    kmax_refined = kmax + kmax_refine_step;
    res_kmax = pdf_from_kpmf_fft(kpmf, bck, Nsim, six, kmax_refined, dx_base, nbins_one_photon, alpha);
    slim_kmax = res_kmax.Slim;

    % ---- Check 1: Slim convergence with dx ----
    if isempty(slim_tol)
        slim_tol = dx_refined;
    end
    if isempty(slim_base) || isempty(slim_dx)
        slim_dx_diff = Inf;
        slim_dx_ok   = false;
    else
        slim_dx_diff = abs(slim_dx - slim_base);
        slim_dx_ok   = slim_dx_diff < slim_tol;
    end

    % ---- Check 2: SF convergence near threshold with dx ----
    sf_base_on_dx = interpolate_sf(res_base.x, res_base.sf, res_dx.x);
    sf_dx = res_dx.sf;
    region_mask = get_threshold_region_mask(sf_dx, alpha, region_factor_low, region_factor_high);
    if any(region_mask)
        sf_abs_diff_region = max(abs(sf_dx(region_mask) - sf_base_on_dx(region_mask)));
        sf_tol_used = max(sf_rel_tol * alpha, sf_abs_tol);
        sf_region_ok = sf_abs_diff_region < sf_tol_used;
    else
        sf_abs_diff_region = NaN;
        sf_region_ok = false;
    end

    % ---- Check 3: Poisson truncation tail small enough ----
    poisson_tail_base = res_base.poisson_tail;
    poisson_tail_ok = poisson_tail_base < poisson_tail_fraction * alpha;

    % ---- Check 4: Slim convergence with kmax ----
    if isempty(slim_base) || isempty(slim_kmax)
        slim_kmax_diff = Inf;
        slim_kmax_ok   = false;
    else
        slim_kmax_diff = abs(slim_kmax - slim_base);
        slim_kmax_ok   = slim_kmax_diff < slim_tol;
    end

    all_ok = slim_dx_ok && sf_region_ok && poisson_tail_ok && slim_kmax_ok;

    out = struct();
    out.all_ok               = all_ok;
    out.slim_base            = slim_base;
    out.slim_dx_refined      = slim_dx;
    out.slim_kmax_refined    = slim_kmax;
    out.slim_tol             = slim_tol;
    out.slim_dx_diff         = slim_dx_diff;
    out.slim_kmax_diff       = slim_kmax_diff;
    out.sf_abs_diff_region   = sf_abs_diff_region;
    out.poisson_tail_base    = poisson_tail_base;
    out.check_slim_dx_ok     = slim_dx_ok;
    out.check_sf_region_ok   = sf_region_ok;
    out.check_poisson_tail_ok= poisson_tail_ok;
    out.check_slim_kmax_ok   = slim_kmax_ok;
    out.dx_base              = dx_base;
    out.dx_refined           = dx_refined;
    out.kmax_base            = kmax;
    out.kmax_refined         = kmax_refined;
    out.res_base             = res_base;
    out.res_dx_refined       = res_dx;
    out.res_kmax_refined     = res_kmax;
end

% =====================================================================
% pdf_from_kpmf_fft
% Approximate the null PMF of the matched-filter statistic by binning the
% one-photon kernel values and forming a compound-Poisson mixture of FFT
% self-convolutions. Mirrors the Python pdf_from_kpmf_fft exactly.
% Returns a struct with fields x, bin_edges, pmf, pdf, counts, sf, Slim,
% dx, mu_tot, kmax, poisson_weights, poisson_tail, per_k_pmf.
% =====================================================================
function res = pdf_from_kpmf_fft(kpmf, bck, Nsim, six, kmax, dx, nbins_one_photon, tail_prob)
    if nargin < 4 || isempty(six),  six = false; end
    if nargin < 5, kmax = []; end
    if nargin < 6, dx   = []; end
    if nargin < 7 || isempty(nbins_one_photon), nbins_one_photon = 2000; end
    if nargin < 8, tail_prob = []; end

    pixels = kpmf(:);
    pixels = pixels(~isnan(pixels));
    len = numel(pixels);
    if len == 0
        error('kpmf has no non-NaN pixels');
    end
    if isempty(kmax)
        if six, kmax = 6; else, kmax = 5; end
    end

    mu_tot = len * bck;

    % 1) bin width dx for the one-photon values
    pmin = min(pixels);
    pmax = max(pixels);
    if isempty(dx)
        if pmax == pmin
            dx = 1.0;
        else
            dx = (pmax - pmin) / nbins_one_photon;
            if dx <= 0
                dx = 1.0;
            end
        end
    end

    % 2) quantise one-photon values onto an integer grid.
    %    B is a non-negative shift so all indices are >= 0 (0-based, as in Python).
    B = ceil(max(0.0, -pmin / dx));
    % np.rint == round half to even. MATLAB round() is half away from zero,
    % so use an explicit round-half-to-even to match Python bit-for-bit.
    j = round_half_even(pixels / dx) + B;     % 0-based indices (as in numpy)
    J = max(j);

    % np.bincount(j, minlength=J+1): counts of indices 0..J
    p1 = accumarray(j + 1, 1, [J + 1, 1], @sum, 0);   % +1 for MATLAB 1-based
    p1 = p1 / sum(p1);

    % 3) FFT setup
    global_len = kmax * J + 1;
    nfft = pow2(nextpow2_len(global_len));   % 1 << (global_len-1).bit_length()

    F1 = fft(p1(:).', nfft);                 % full complex FFT (row), length nfft

    % common output grid: x_m = dx*(m - kmax*B), m = 0..global_len-1
    m = (0:global_len - 1);
    x = dx * (m - kmax * B);
    x = x(:);

    % bin edges from centres
    bin_edges = zeros(global_len + 1, 1);
    bin_edges(2:end-1) = 0.5 * (x(1:end-1) + x(2:end));
    bin_edges(1)   = x(1)   - 0.5 * dx;
    bin_edges(end) = x(end) + 0.5 * dx;

    % 4) per-k PMFs embedded on the common grid
    per_k_pmf = cell(kmax + 1, 1);
    for k = 0:kmax
        if k == 0
            pk = 1.0;                 % length-1 vector, value 1
        else
            Fk = F1 .^ k;
            pk_full = real(ifft(Fk, nfft));
            pk = pk_full(1:(k * J + 1));
            pk = max(pk, 0.0);
            s = sum(pk);
            if s > 0
                pk = pk / s;
            end
        end
        pk = pk(:);
        pk_embedded = zeros(global_len, 1);
        offset = (kmax - k) * B;                      % 0-based offset
        pk_embedded(offset + 1 : offset + numel(pk)) = pk;
        per_k_pmf{k + 1} = pk_embedded;
    end

    % 5) Poisson mixture over photon number
    ks = (0:kmax);
    poisson_weights = poisspdf_local(ks, mu_tot);     % row vector
    poisson_tail = poisssf_local(kmax, mu_tot);       % P(K > kmax)

    pmf = zeros(global_len, 1);
    for k = 0:kmax
        pmf = pmf + poisson_weights(k + 1) * per_k_pmf{k + 1};
    end

    retained_mass = sum(poisson_weights);
    if retained_mass > 0
        pmf = pmf / retained_mass;
    end

    pdf = pmf / dx;
    counts = Nsim * pmf;

    % survival function P(S >= x)
    sf = flipud(cumsum(flipud(pmf)));

    Slim = [];
    if ~isempty(tail_prob)
        idx = find(sf <= tail_prob, 1, 'first');
        if ~isempty(idx)
            Slim = x(idx);
        else
            Slim = [];
        end
    end

    res = struct();
    res.x               = x;
    res.bin_edges       = bin_edges;
    res.pmf             = pmf;
    res.pdf             = pdf;
    res.counts          = counts;
    res.sf              = sf;
    res.Slim            = Slim;
    res.tail_prob       = tail_prob;
    res.dx              = dx;
    res.pixels          = pixels;
    res.mu_tot          = mu_tot;
    res.kmax            = kmax;
    res.poisson_weights = poisson_weights;
    res.poisson_tail    = poisson_tail;
    res.per_k_pmf       = {per_k_pmf};
end

% =====================================================================
% interpolate_sf / get_threshold_region_mask  (mirror the Python helpers)
% =====================================================================
function sf_new = interpolate_sf(x_old, sf_old, x_new)
    % linear interp; outside the range fill with edge values (left/right)
    x_old = x_old(:); sf_old = sf_old(:); x_new = x_new(:);
    sf_new = interp1(x_old, sf_old, x_new, 'linear');
    sf_new(x_new < x_old(1))   = sf_old(1);
    sf_new(x_new > x_old(end)) = sf_old(end);
end

function mask = get_threshold_region_mask(sf, alpha, factor_low, factor_high)
    if nargin < 3 || isempty(factor_low),  factor_low  = 0.1;  end
    if nargin < 4 || isempty(factor_high), factor_high = 10.0; end
    mask = (sf >= factor_low * alpha) & (sf <= factor_high * alpha);
end

% =====================================================================
% Numerical helpers (toolbox-free)
% =====================================================================
function s = nansum_local(x)
    x = x(:);
    s = sum(x(~isnan(x)));
end

function y = round_half_even(x)
    % Round half to even (banker's rounding), matching numpy.rint.
    y = round(x);                       % half away from zero
    d = x - floor(x);
    half = (abs(d - 0.5) < 1e-12);      % exactly .5 cases
    % for halves, force to even integer
    yeven = 2 * round(x / 2);
    y(half) = yeven(half);
end

function n = nextpow2_len(L)
    % smallest n with 2^n >= L, matching 1 << (L-1).bit_length()
    if L <= 1
        n = 0;
    else
        n = ceil(log2(L));
        if 2^n < L
            n = n + 1;
        end
    end
end

function p = poisspdf_local(k, mu)
    p = exp(-mu) .* mu .^ k ./ factorial(k);
end

function s = poisssf_local(k, mu)
    % P(K > k) = 1 - P(K <= k), Poisson(mu), k integer >= 0
    s = 1 - sum(poisspdf_local(0:k, mu));
    if s < 0, s = 0; end
end

function x = poissinv_local(q, mu)
    if q >= 1, x = Inf; return; end
    if q <= 0, x = 0;   return; end
    x = 0;
    cdf = exp(-mu);
    term = cdf;
    while cdf < q
        x = x + 1;
        term = term * mu / x;
        cdf = cdf + term;
        if x > 1e7, break; end
    end
end