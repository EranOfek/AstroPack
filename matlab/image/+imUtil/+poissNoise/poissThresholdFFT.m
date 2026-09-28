function [Sth, Res] = poissThresholdFFT(Kernel, Back, Args)
    % Threshold of a linear filter under Poisson noise, using the FFT (characteristic-function) method.
    % Description: Compute the null (background-only) distribution of the
    %              linear detection statistic
    %                  S = sum_q K(q) * M(q),   M(q) ~ Poisson(lambda(q))
    %              and the detection threshold Sth for a given false-alarm
    %              probability gamma, P(S >= Sth | H0) <= gamma.
    %              S is a compound Poisson variable: the number of background
    %              photons in the kernel support is N ~ Poisson(Lambda), with
    %              Lambda = sum_q lambda(q), and each photon contributes
    %              X = K(Q), where the pixel Q is drawn with probability
    %              lambda(q)/Lambda. The one-photon PMF pX is the (background-
    %              weighted) histogram of the kernel values on a lattice of
    %              spacing DeltaS, and the characteristic function of S is
    %                  phiS = exp(Lambda * (phiX - 1)).
    %              The PMF of S is obtained with one FFT, a pointwise
    %              exponential, and one inverse FFT. The FFT length is chosen
    %              such that no circular aliasing occurs for up to nalias
    %              photons, where P(N > nalias) < EpsAlias. The support is
    %              further tightened using a Chernoff bound on the tails of S
    %              (P(S outside support) <= 2*EpsAlias), which is much
    %              tighter than nalias*max(K) for large Lambda.
    %              The kernel may contain negative values (e.g., a Mexican-hat
    %              wavelet). Note that the far tail (PMF below ~1e-16 of its
    %              peak) is limited by round-off (catastrophic cancellation in
    %              the inverse FFT); an estimate of this floor is returned in
    %              Res.NoiseFloor. For very small gamma use
    %              imUtil.poissNoise.poissThresholdPanjer (for K>=0).
    %              Optionally, the result is validated by repeating the
    %              calculation with a finer lattice (DeltaS/2) and a stricter
    %              aliasing tolerance, and refining until the threshold and
    %              the survival function near gamma are stable.
    %              Reference: Soumagnac, Ofek, Israeli & Glukhov (2026).
    % Input  : - Kernel : The linear filter K (e.g., the PSF, or the optimal
    %                     Poisson matched filter log(1+F*P/B)). A matrix of any
    %                     dimension. NaN values are ignored.
    %          - Back   : Expected background counts per pixel lambda(q)
    %                     [counts]. Either a scalar (uniform background) or an
    %                     array with the same size as Kernel.
    %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
    %            where keyword are one of the followings:
    %            'Gamma'        - False-alarm probability per resolution
    %                             element, P(S >= Sth | H0). A scalar or a
    %                             vector. Default is 2.8665e-7 (5 sigma,
    %                             one-sided Gaussian).
    %            'DeltaS'       - Lattice bin width in units of S. If empty,
    %                             use max(abs(Kernel))/500. Default is [].
    %            'EpsAlias'     - Initial tolerance on the omitted Poisson
    %                             tail, P(N > nalias). The initial value used is
    %                             min(EpsAlias, 0.1*AliasFactor*min(Gamma)).
    %                             Default is 1e-12.
    %            'AliasFactor'  - Require P(N > nalias) < AliasFactor*min(Gamma).
    %                             Default is 0.01.
    %            'Validate'     - Run the convergence checks and the adaptive
    %                             refinement. Default is true.
    %            'EpsS'         - Absolute tolerance on the threshold [units of
    %                             S]. If empty, use 2 times the initial DeltaS.
    %                             Default is [].
    %            'EpsRel'       - Relative (to gamma) tolerance on the survival
    %                             function near the threshold. Default is 0.2.
    %            'EpsAbs'       - Absolute tolerance on the survival function
    %                             near the threshold. Default is 0.
    %            'RegionFactor' - The survival function is compared in the
    %                             region gamma/RegionFactor <= SF <=
    %                             gamma*RegionFactor. Default is 2.
    %            'RefineEps'    - Factor by which EpsAlias is multiplied in
    %                             the refinement. Default is 1e-3.
    %            'MaxIter'      - Maximum number of refinement iterations.
    %                             Default is 10.
    %            'Verbose'      - Print progress. Default is false.
    % Output : - Sth : Detection threshold (in units of S) for each Gamma: the
    %                  smallest lattice value s_j for which P(S >= s_j) <= Gamma.
    %                  NaN if Gamma is below the computed tail.
    %          - Res : A structure with the following fields:
    %                  .S          - Lattice values s_j = j*DeltaS (column).
    %                  .PMF        - Probability of each lattice bin.
    %                  .PDF        - PMF/DeltaS (probability density).
    %                  .SF         - Survival function P(S >= s_j | H0).
    %                  .Gamma, .Sth
    %                  .DeltaS     - The (final) bin width.
    %                  .EpsAlias   - The (final) aliasing tolerance.
    %                  .TailProb   - Upper bound on the probability outside
    %                                the computed support.
    %                  .Nalias     - Max. number of photons in the FFT support.
    %                  .Lambda     - Expected number of photons in the kernel.
    %                  .Nfft       - FFT length.
    %                  .NoiseFloor - Total negative probability removed after
    %                                the inverse FFT; SF values smaller than
    %                                ~NoiseFloor are not reliable.
    %                  .Checks     - 1x5 logical vector of the convergence
    %                                checks: [Sth vs DeltaS, SF vs DeltaS,
    %                                P(N>nalias)<AliasFactor*gamma, Sth vs
    %                                EpsAlias, SF vs EpsAlias].
    %                  .Converged  - true if all checks passed.
    %                  .Niter      - Number of iterations.
    %                  .Time       - Run time [s].
    % Author : Claude + Eran Ofek (Sep 2026)
    %    URL : https://github.com/EranOfek/AstroPack
    % Example: [X,Y] = meshgrid(-2:2); P = exp(-(X.^2+Y.^2)/2); P = P./sum(P(:));
    %          B = 7.3e-3; K = log(1 + 3.*P./B);
    %          [Sth, Res] = imUtil.poissNoise.poissThresholdFFT(K, B, 'Gamma',[1e-3 3.17e-5]);
    %          semilogy(Res.S, Res.SF)

    arguments
        Kernel
        Back
        Args.Gamma                = 2.8665e-7
        Args.DeltaS               = []
        Args.EpsAlias             = 1e-12
        Args.AliasFactor          = 0.01
        Args.Validate logical     = true
        Args.EpsS                 = []
        Args.EpsRel               = 0.2
        Args.EpsAbs               = 0
        Args.RegionFactor         = 2
        Args.RefineEps            = 1e-3
        Args.MaxIter              = 10
        Args.Verbose logical      = false
    end

    Tstart = tic;
    Gamma  = Args.Gamma(:).';
    [K, Lam] = prepareInput(Kernel, Back);
    Lambda   = sum(Lam);

    if isempty(Args.DeltaS)
        DeltaS = max(abs(K))./500;
    else
        DeltaS = Args.DeltaS;
    end
    if isempty(Args.EpsS)
        EpsS = 2.*DeltaS;
    else
        EpsS = Args.EpsS;
    end
    % start with a tail tolerance well below the requested Gamma
    EpsAlias = min(Args.EpsAlias, 0.1.*Args.AliasFactor.*min(Gamma));

    Checks    = false(1,5);
    Converged = false;
    for Iter=1:1:Args.MaxIter
        R0   = coreFFT(K, Lam, Lambda, DeltaS, EpsAlias);
        Sth0 = thresholdFromSF(R0, Gamma);
        if ~Args.Validate
            break;
        end

        Rd   = coreFFT(K, Lam, Lambda, DeltaS./2, EpsAlias);
        Ra   = coreFFT(K, Lam, Lambda, DeltaS, EpsAlias.*Args.RefineEps);
        Sthd = thresholdFromSF(Rd, Gamma);
        Stha = thresholdFromSF(Ra, Gamma);

        Checks(1) = all(abs(Sthd - Sth0) < EpsS);
        Checks(2) = compareSF(R0, Rd, Gamma, Args.RegionFactor, Args.EpsRel, Args.EpsAbs);
        Checks(3) = R0.TailProb < Args.AliasFactor.*min(Gamma);
        Checks(4) = all(abs(Stha - Sth0) < EpsS);
        Checks(5) = compareSF(R0, Ra, Gamma, Args.RegionFactor, Args.EpsRel, Args.EpsAbs);

        if Args.Verbose
            fprintf('Iter %d: DeltaS=%g EpsAlias=%g Nfft=%d Checks=[%d %d %d %d %d]\n', ...
                    Iter, DeltaS, EpsAlias, R0.Nfft, Checks);
        end

        if all(Checks)
            Converged = true;
            break;
        end
        if ~all(Checks(3:5))
            % refine the tail tolerance first (it also affects checks 1-2)
            EpsAlias = EpsAlias.*Args.RefineEps;
        elseif ~all(Checks(1:2))
            DeltaS = DeltaS./2;
        end
    end

    if Args.Validate && ~Converged
        warning('poissThresholdFFT:NotConverged', 'Convergence checks failed after %d iterations', Args.MaxIter);
    end

    if any(Gamma < 10.*R0.NoiseFloor)
        warning('poissThresholdFFT:NoiseFloor', ...
                'Gamma is close to the FFT round-off floor (%g) - use poissThresholdPanjer', R0.NoiseFloor);
    end

    Sth           = Sth0;
    Res           = R0;
    Res.Gamma     = Gamma;
    Res.Sth       = Sth;
    Res.Lambda    = Lambda;
    Res.Checks    = Checks;
    Res.Converged = Converged;
    Res.Niter     = Iter;
    Res.Time      = toc(Tstart);
end

%--------------------------------------------------------------------------
% Internal functions
%--------------------------------------------------------------------------
function [K, Lam] = prepareInput(Kernel, Back)
    % Return column vectors of kernel values and expected background counts
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
    if sum(Lam) <= 0
        error('The total expected background in the kernel support must be positive');
    end
end

function [N, Tail] = poissTailN(Lambda, Eps)
    % Smallest N such that P(n > N) < Eps, for n ~ Poisson(Lambda)
    % P(n > N) = P(n >= N+1) = gammainc(Lambda, N+1) (lower regularized)
    Nmax = ceil(Lambda + 10.*sqrt(Lambda) + 20);
    while true
        Nv = (0:Nmax).';
        T  = gammainc(Lambda, Nv + 1);
        I  = find(T < Eps, 1, 'first');
        if ~isempty(I)
            N    = Nv(I);
            Tail = T(I);
            return;
        end
        Nmax = 2.*Nmax;
    end
end

function R = coreFFT(K, Lam, Lambda, DeltaS, EpsAlias)
    % Compound-Poisson PMF on a lattice using the characteristic function
    Bin  = round(K./DeltaS);
    [Nalias, TailProb] = poissTailN(Lambda, EpsAlias);

    Jmin  = min(Bin);
    Jmax  = max(Bin);
    % Support needed for up to Nalias photons, tightened with a Chernoff
    % bound on each tail of S (P(S>Jhigh), P(S<Jlow) <= EpsAlias).
    Jup   = chernoffTailJ(Bin, Lam./Lambda, Lambda, log(EpsAlias));
    Jdn   = chernoffTailJ(-Bin, Lam./Lambda, Lambda, log(EpsAlias));
    Jlow  = min(0, max(Nalias.*Jmin, -Jdn));
    Jhigh = max(0, min(Nalias.*Jmax, Jup));
    Ngrid = Jhigh - Jlow + 1;
    Nfft  = 2.^nextpow2(Ngrid);

    % one-photon PMF - negative indices are stored circularly
    PX    = accumarray(mod(Bin, Nfft) + 1, Lam, [Nfft 1])./Lambda;
    PhiS  = exp(Lambda.*(fft(PX) - 1));
    Pc    = ifft(PhiS);

    J     = (Jlow:Jhigh).';
    P     = real(Pc(mod(J, Nfft) + 1));
    Fneg  = P < 0;
    NoiseFloor = sum(-P(Fneg));
    P(Fneg)    = 0;

    R.S          = J.*DeltaS;
    R.PMF        = P;
    R.PDF        = P./DeltaS;
    R.SF         = flipud(cumsum(flipud(P)));
    R.DeltaS     = DeltaS;
    R.EpsAlias   = EpsAlias;
    R.TailProb   = min(TailProb, 2.*EpsAlias);  % bound on the omitted probability
    R.Nalias     = Nalias;
    R.Nfft       = Nfft;
    R.NoiseFloor = NoiseFloor;
end

function [J, LogBound] = chernoffTailJ(Bin, W, Lambda, LogEps)
    % Chernoff bound on the upper tail of the compound Poisson lattice
    % variable S = sum_i Bin(Q_i), with P(Q=q) = W(q), N ~ Poisson(Lambda):
    %     P(S >= J) <= exp(-Theta*J + Lambda*(M(Theta) - 1)),  Theta > 0
    % where M(Theta) = sum_q W(q)*exp(Theta*Bin(q)). Return the smallest J
    % (over a grid of Theta) for which the bound is <= exp(LogEps).
    % J=0 if Bin<=0 everywhere (then S<=0, and the upper tail is empty).
    if max(Bin) <= 0
        J        = 0;
        LogBound = -Inf;
        return;
    end
    [Ub, ~, Iu] = unique(Bin);
    Wu    = accumarray(Iu, W);
    Theta = logspace(-5, log10(50), 400)./max(Bin);
    E     = log(Wu) + Ub.*Theta;                  % Nbin x Ntheta
    Emax  = max(E, [], 1);
    LogM  = Emax + log(sum(exp(E - Emax), 1));    % log M(Theta)
    Jv    = (Lambda.*(exp(LogM) - 1) - LogEps)./Theta;
    Jv(~isfinite(Jv)) = Inf;
    J        = ceil(min(Jv));
    LogBound = LogEps;
end

function Sth = thresholdFromSF(R, Gamma)
    % smallest lattice value with SF <= Gamma
    Sth = nan(size(Gamma));
    for Ig=1:numel(Gamma)
        I = find(R.SF <= Gamma(Ig), 1, 'first');
        if ~isempty(I)
            Sth(Ig) = R.S(I);
        end
    end
end

function Flag = compareSF(R0, R1, Gamma, RegionFactor, EpsRel, EpsAbs)
    % Compare the survival functions of two calculations near each Gamma.
    % SF(s_j) = P(S >= lower edge of bin j), so compare at the bin lower edges.
    Edge0 = R0.S - R0.DeltaS./2;
    Edge1 = R1.S - R1.DeltaS./2;
    LogSF1 = log(max(R1.SF, realmin));
    Flag  = true;
    for Ig=1:numel(Gamma)
        G   = Gamma(Ig);
        Sel = R0.SF >= G./RegionFactor & R0.SF <= G.*RegionFactor;
        if ~any(Sel)
            % no lattice point in the region - use the two points bracketing G
            I   = find(R0.SF <= G, 1, 'first');
            if isempty(I)
                Flag = false;
                return;
            end
            Sel = false(size(R0.SF));
            Sel(max(I-1,1):I) = true;
        end
        SF1 = exp(interp1(Edge1, LogSF1, Edge0(Sel), 'linear', 'extrap'));
        Diff = max(abs(SF1 - R0.SF(Sel)));
        Flag = Flag && Diff < max(EpsRel.*G, EpsAbs);
    end
end
