function [Sth, Res] = poissThresholdSimulations(Kernel, Back, Args)
    % Threshold of a linear filter under Poisson noise, estimated from simulations.
    % Description: Estimate the null (background-only) distribution of the
    %              linear detection statistic
    %                  S = sum_q K(q) * M(q),   M(q) ~ Poisson(lambda(q))
    %              by simulating Nsim background-only stamps (independent
    %              Poisson counts in each pixel) and filtering them with the
    %              kernel. The simulated values of S are histogrammed on a
    %              lattice of spacing DeltaS (the same convention as in
    %              imUtil.poissNoise.poissThresholdFFT and
    %              imUtil.poissNoise.poissThresholdPanjer), so that the full
    %              sample need not be stored. The detection threshold Sth for
    %              a false-alarm probability gamma is the smallest lattice
    %              value s_j for which the empirical P(S >= s_j) <= gamma.
    %              This method does not rely on the compound-Poisson
    %              formalism, and it is therefore an independent check of the
    %              analytical methods. However, it requires Nsim >> 1/gamma
    %              (the relative error of the survival function near the
    %              threshold is ~1/sqrt(gamma*Nsim)).
    %              The simulations use poissrnd if available (Statistics
    %              Toolbox); otherwise, a built-in inverse-CDF sampler.
    % Input  : - Kernel : The linear filter K. A matrix of any dimension.
    %                     NaN values are ignored.
    %          - Back   : Expected background counts per pixel lambda(q)
    %                     [counts]. Either a scalar (uniform background) or an
    %                     array with the same size as Kernel.
    %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
    %            where keyword are one of the followings:
    %            'Gamma'        - False-alarm probability per resolution
    %                             element, P(S >= Sth | H0). A scalar or a
    %                             vector. Default is 1e-4.
    %            'Nsim'         - Number of simulated background stamps.
    %                             Default is 1e6.
    %            'BlockSize'    - Number of stamps simulated at once (controls
    %                             memory usage). Default is 1e5.
    %            'DeltaS'       - Lattice bin width in units of S. If empty,
    %                             use max(abs(Kernel))/500. Default is [].
    %            'MinEvents'    - Minimum expected number of simulated events
    %                             above the threshold (gamma*Nsim) for the
    %                             threshold to be declared resolved. Below 1,
    %                             Sth is set to NaN. Default is 10.
    %            'Seed'         - Random number generator seed. If empty, do
    %                             not set the seed. Default is [].
    %            'UsePoissrnd'  - Use poissrnd if available. Default is true.
    %            'Verbose'      - Print progress. Default is false.
    % Output : - Sth : Detection threshold (in units of S) for each Gamma.
    %                  NaN if Gamma*Nsim < 1.
    %          - Res : A structure with the following fields:
    %                  .S        - Lattice values s_j = j*DeltaS (column).
    %                  .Counts   - Number of simulations in each bin.
    %                  .PMF      - Counts/Nsim.
    %                  .PDF      - PMF/DeltaS.
    %                  .SF       - Empirical survival function P(S >= s_j).
    %                  .SFErr    - Poisson error of SF, sqrt(N(S>=s_j))/Nsim.
    %                  .Gamma, .Sth
    %                  .Resolved - Logical per Gamma: Gamma*Nsim >= MinEvents.
    %                  .Nsim, .DeltaS, .Lambda
    %                  .Time     - Run time [s].
    % Author : Claude + Eran Ofek (Sep 2026)
    %    URL : https://github.com/EranOfek/AstroPack
    % Example: [X,Y] = meshgrid(-2:2); P = exp(-(X.^2+Y.^2)/2); P = P./sum(P(:));
    %          B = 7.3e-3; K = log(1 + 3.*P./B);
    %          [Sth, Res] = imUtil.poissNoise.poissThresholdSimulations(K, B, 'Gamma',1e-3, 'Nsim',1e6);

    arguments
        Kernel
        Back
        Args.Gamma                = 1e-4
        Args.Nsim                 = 1e6
        Args.BlockSize            = 1e5
        Args.DeltaS               = []
        Args.MinEvents            = 10
        Args.Seed                 = []
        Args.UsePoissrnd logical  = true
        Args.Verbose logical      = false
    end

    Tstart = tic;
    Gamma  = Args.Gamma(:).';

    K = Kernel(:);
    if isscalar(Back)
        Lam = Back.*ones(size(K));
    else
        if numel(Back) ~= numel(Kernel)
            error('Back must be a scalar or an array with the same size as Kernel');
        end
        Lam = Back(:);
    end
    Flag = ~isnan(K) & ~isnan(Lam);
    K    = K(Flag);
    Lam  = Lam(Flag);
    if any(Lam < 0)
        error('Back must be non-negative');
    end

    if isempty(Args.DeltaS)
        DeltaS = max(abs(K))./500;
    else
        DeltaS = Args.DeltaS;
    end

    if ~isempty(Args.Seed)
        rng(Args.Seed);
    end

    UsePoissrnd = Args.UsePoissrnd && exist('poissrnd', 'file') > 0;
    if ~UsePoissrnd && max(Lam) > 500
        error('Background >500 counts/pixel requires poissrnd (Statistics Toolbox)');
    end

    Npix  = numel(K);
    Nsim  = round(Args.Nsim);
    Hist  = zeros(0,1);
    Off   = 0;               % lattice index of Hist(1)
    Ndone = 0;
    while Ndone < Nsim
        Nb = min(Args.BlockSize, Nsim - Ndone);
        if UsePoissrnd
            M = poissrnd(repmat(Lam, 1, Nb));
        else
            M = poissonInvCDF(Lam, Nb);
        end
        S   = K.' * M;                     % 1 x Nb
        Ind = round(S(:)./DeltaS);

        % extend the histogram range if needed
        Imin = min(Ind);
        Imax = max(Ind);
        if isempty(Hist)
            Off  = Imin;
            Hist = zeros(Imax - Imin + 1, 1);
        else
            if Imin < Off
                Hist = [zeros(Off - Imin, 1); Hist];
                Off  = Imin;
            end
            Ntop = Imax - Off + 1;
            if Ntop > numel(Hist)
                Hist = [Hist; zeros(Ntop - numel(Hist), 1)];
            end
        end
        Hist  = Hist + accumarray(Ind - Off + 1, 1, [numel(Hist) 1]);
        Ndone = Ndone + Nb;
        if Args.Verbose
            fprintf('Simulated %d / %d stamps (Npix=%d)\n', Ndone, Nsim, Npix);
        end
    end

    Ncum = flipud(cumsum(flipud(Hist)));   % number of sims with S >= s_j

    Res.S      = (Off:(Off + numel(Hist) - 1)).'.*DeltaS;
    Res.Counts = Hist;
    Res.PMF    = Hist./Nsim;
    Res.PDF    = Res.PMF./DeltaS;
    Res.SF     = Ncum./Nsim;
    Res.SFErr  = sqrt(Ncum)./Nsim;

    Sth = nan(size(Gamma));
    for Ig=1:numel(Gamma)
        if Gamma(Ig).*Nsim >= 1
            I = find(Res.SF <= Gamma(Ig), 1, 'first');
            if isempty(I)
                % no simulated value above the threshold - one bin above max
                Sth(Ig) = Res.S(end) + DeltaS;
            else
                Sth(Ig) = Res.S(I);
            end
        end
    end
    Resolved = Gamma.*Nsim >= Args.MinEvents;
    if any(~Resolved)
        warning('poissThresholdSimulations:NotResolved', ...
                'Gamma*Nsim < %g for some Gamma values - increase Nsim', Args.MinEvents);
    end

    Res.Gamma    = Gamma;
    Res.Sth      = Sth;
    Res.Resolved = Resolved;
    Res.Nsim     = Nsim;
    Res.DeltaS   = DeltaS;
    Res.Lambda   = sum(Lam);
    Res.Time     = toc(Tstart);
end

%--------------------------------------------------------------------------
% Internal functions
%--------------------------------------------------------------------------
function M = poissonInvCDF(Lam, Nb)
    % Poisson random numbers by CDF inversion (column vector Lam, Nb columns)
    % Efficient for small lambda (low-count regime).
    Npix = numel(Lam);
    U    = rand(Npix, Nb);
    LamM = repmat(Lam, 1, Nb);
    M    = zeros(Npix, Nb);
    Pk   = exp(-LamM);           % P(M=k), starting at k=0
    Cdf  = Pk;
    Flag = U > Cdf;
    Kc   = 0;
    while any(Flag(:))
        Kc       = Kc + 1;
        M(Flag)  = M(Flag) + 1;
        Pk       = Pk.*LamM./Kc;
        Cdf      = Cdf + Pk;
        % guard against round-off of the CDF near 1
        Flag     = U > Cdf & Pk > 0;
    end
end
