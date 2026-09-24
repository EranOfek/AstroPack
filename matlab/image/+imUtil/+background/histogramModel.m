function [I, P, Info] = histogramModel(Args)
    % Model the histogram of pixel values for a background + power-law stars.
    %   Compute the probability density of pixel values for an image with a
    %   uniform background B and a Poisson population of Gaussian-PSF stars
    %   whose differential flux distribution is a power law, under
    %   background-limited Gaussian noise. Uses the P(D) / compound-Poisson
    %   (Scheuer 1957; Condon 1974) formalism, evaluated by FFT.
    %
    %   The pixel value is  I = B + D + n,  with n ~ N(0, SigmaN^2),
    %   SigmaN^2 = B + SigmaRead^2, and D the summed deposit of all stars.
    %   The Gaussian PSF (unit volume) is PSF(r)=exp(-r^2/(2 SigmaP^2))/Kappa
    %   with Kappa = 2*pi*SigmaP^2, so a star of flux f at radius r deposits
    %   g(f,r) = (f/Kappa)*exp(-r^2/(2 SigmaP^2)) [photons], peak f/Kappa.
    %   The differential counts (stars/pixel/flux) are
    %   rho(f) = (Ns/B)*(f/B)^(-Alpha), f in [Fmin,Fmax], and the deflection
    %   rate is R(g) = (Kappa/g)*Int_{max(Kappa g,Fmin)}^{Fmax} rho(f) df.
    %   The characteristic function of a pixel value is
    %   Phat(w) = exp( i w B - 0.5 SigmaN^2 w^2 + Psi(w) ),
    %   Psi(w)  = Int_0^inf R(g) (exp(i w g) - 1) dg,
    %   and the histogram is P(I) = (1/2pi) Int Phat(w) exp(-i w I) dw.
    %   R(g) diverges as 1/g at g->0 but the Psi integrand is bounded there;
    %   it is evaluated on a midpoint g-grid as Psi = BinSize*(S-C) so the two
    %   grid-dependent pieces (S=sum R e^{iwg}, C=sum R) cancel to the finite
    %   regularized value.
    % Input  : * ...,key,val,...
    %            'B'        - Background level [photons]. Default is 1000.
    %            'SigmaP'   - PSF Gaussian sigma [pix]. Default is 1.5.
    %            'Alpha'    - Differential count power-law slope, rho~f^-Alpha
    %                         (require Alpha>1 and Alpha~=1). Default is 1.5.
    %            'Ns'       - Stars per pixel per unit flux at f=B.
    %                         Default is 0.01.
    %            'Fmin'     - Minimum star flux [photons]. If NaN, set to
    %                         0.01*B. Default is NaN.
    %            'Fmax'     - Maximum star flux [photons]. If NaN, set to
    %                         1000*B. Default is NaN.
    %            'SigmaRead'- Read noise [photons], added in quadrature.
    %                         Default is 0.
    %            'SigmaN'   - Override total noise sigma. If NaN, set to
    %                         sqrt(B+SigmaRead^2). Default is NaN.
    %            'N'        - FFT length (power of 2 recommended).
    %                         Default is 2^15.
    %            'BinSize'  - Grid/bin width [photons]. If NaN, set to
    %                         SigmaN/8. Default is NaN.
    %            'Imin'     - Grid start [photons]. If NaN, set to
    %                         B-Nsigma*SigmaN. Default is NaN.
    %            'Nsigma'   - Lower grid extent in units of SigmaN, used for
    %                         the automatic Imin. Default is 6.
    % Output : - I    : Column vector of pixel values [photons].
    %          - P    : Column vector of probability density (sum(P)*BinSize~=1).
    %          - Info : Structure with the resolved parameters and diagnostics:
    %                   .SigmaN, .Kappa, .BinSize, .Imin, .N, .Fmin, .Fmax,
    %                   .ModeI (histogram peak), .MeanI, .StdI,
    %                   .K1,.K2,.K3 (1st-3rd cumulants of the deflection D),
    %                   .ModeShift (=ModeI-B), .Norm (~1), and .G,.R arrays.
    % Author : Claude + Eran Ofek (Jun 2026)
    % Example: [I,P,Info] = imUtil.background.histogramModel('B',1000, 'Ns',0.05, 'Alpha',1.5);
    %          semilogy(I,P); xline(Info.ModeI,'r'); xline(Info.B,'k--');
    %          % background-limited limit (no sources) -> pure Gaussian:
    %          [I,P] = imUtil.background.histogramModel('Ns',0);

    arguments
        Args.B          (1,1) double {mustBePositive}                = 1000
        Args.SigmaP     (1,1) double {mustBePositive}                = 1.5
        Args.Alpha      (1,1) double                                 = 1.5
        Args.Ns         (1,1) double {mustBeNonnegative}             = 0.01
        Args.Fmin       (1,1) double                                 = NaN
        Args.Fmax       (1,1) double                                 = NaN
        Args.SigmaRead  (1,1) double {mustBeNonnegative}             = 0
        Args.SigmaN     (1,1) double                                 = NaN
        Args.N          (1,1) double {mustBePositive,mustBeInteger}  = 2^15
        Args.BinSize    (1,1) double                                 = NaN
        Args.Imin       (1,1) double                                 = NaN
        Args.Nsigma     (1,1) double {mustBePositive}                = 6
    end

    % --- resolve parameters / defaults --------------------------------------
    B     = Args.B;
    Alpha = Args.Alpha;
    assert(abs(Alpha-1) > 1e-6, 'Alpha must not equal 1 (log-divergent counts).');

    SigmaN = Args.SigmaN;
    if ~isfinite(SigmaN)
        SigmaN = sqrt(B + Args.SigmaRead.^2);    % background-limited noise
    end
    assert(SigmaN > 0, 'SigmaN must be positive.');

    Fmin = Args.Fmin;  if ~isfinite(Fmin), Fmin = 1e-2 .* B; end
    Fmax = Args.Fmax;  if ~isfinite(Fmax), Fmax = 1e3  .* B; end
    assert(Fmin > 0 && Fmax > Fmin, 'Require 0 < Fmin < Fmax.');

    BinSize = Args.BinSize;  if ~isfinite(BinSize), BinSize = SigmaN ./ 8; end
    assert(BinSize > 0, 'BinSize must be positive.');

    Imin = Args.Imin;  if ~isfinite(Imin), Imin = B - Args.Nsigma .* SigmaN; end

    N     = double(Args.N);
    Kappa = 2.*pi.*Args.SigmaP.^2;

    % Warn if the bright tail will not fit within the FFT period.
    if (Fmax./Kappa) > (N.*BinSize - Args.Nsigma.*SigmaN)
        warning('histogramModel:wrap', ...
            ['FFT period N*BinSize = %.3g < Fmax/Kappa = %.3g; the bright ', ...
             'tail will wrap. Increase N or BinSize, or lower Fmax.'], ...
             N.*BinSize, Fmax./Kappa);
    end

    % --- frequency and deflection grids -------------------------------------
    % Signed angular frequencies in FFT order: [0,1,..,N/2-1, -N/2,..,-1].
    K     = [0:N/2-1, -N/2:-1].';        % (N x 1), assumes even N
    Omega = 2.*pi.*K ./ (N.*BinSize);    % variable conjugate to I

    % Midpoint deflection grid avoids g=0 where R diverges.
    J = (0:N-1).';
    G = (J + 0.5) .* BinSize;            % (N x 1), G in (0, N*BinSize)

    % --- deflection rate R(g) -----------------------------------------------
    Flo     = max(Kappa.*G, Fmin);
    Bracket = (Flo./B).^(1-Alpha) - (Fmax./B).^(1-Alpha);   % >=0 for Alpha>1
    R = (Kappa ./ G) .* (Args.Ns./(Alpha-1)) .* Bracket;
    R(Kappa.*G > Fmax) = 0;              % no star bright enough to reach this g
    R(~isfinite(R))    = 0;

    % --- Psi(Omega) = Int R(g)(e^{i w g}-1) dg ------------------------------
    % S_k = sum_j R_j exp(i w_k g_j); g_j=(j+0.5)*BinSize -> half-sample phase.
    % sum_j R_j exp(2 pi i k j/N) = N*ifft(R); multiply by exp(i w BinSize/2),
    % using the SIGNED Omega so the half-sample phase keeps Hermitian symmetry.
    S   = N .* exp(1i.*Omega.*BinSize./2) .* ifft(R);
    C   = sum(R);
    Psi = BinSize .* (S - C);
    Psi(1) = 0;                          % enforce Psi(0)=0 exactly (normalization)

    % --- assemble characteristic function and invert ------------------------
    Phi  = exp(Psi);                                       % E[e^{i w D}]
    Phat = exp(1i.*Omega.*B - 0.5.*SigmaN.^2.*Omega.^2) .* Phi;

    A = Phat .* exp(-1i.*Omega.*Imin);   % phase for grid origin Imin
    P = real(fft(A)) ./ (N.*BinSize);    % P(I_m), I_m = Imin + m*BinSize
    P(P < 0) = 0;                        % clip sub-ulp negative ringing
    I = Imin + (0:N-1).' .* BinSize;

    % --- diagnostics --------------------------------------------------------
    [~, Im] = max(P);
    K1 = BinSize .* sum(G    .* R);      % mean of D
    K2 = BinSize .* sum(G.^2 .* R);      % variance of D
    K3 = BinSize .* sum(G.^3 .* R);      % third central moment of D

    Info           = struct();
    Info.B         = B;
    Info.SigmaN    = SigmaN;
    Info.Kappa     = Kappa;
    Info.Alpha     = Alpha;
    Info.Ns        = Args.Ns;
    Info.Fmin      = Fmin;
    Info.Fmax      = Fmax;
    Info.BinSize   = BinSize;
    Info.Imin      = Imin;
    Info.N         = N;
    Info.ModeI     = I(Im);              % peak of the histogram
    Info.MeanI     = B + K1;             % exact mean
    Info.StdI      = sqrt(SigmaN.^2 + K2);   % exact std
    Info.K1        = K1;
    Info.K2        = K2;
    Info.K3        = K3;
    Info.ModeShift = Info.ModeI - B;     % offset of the peak above the true B
    Info.Norm      = sum(P) .* BinSize;  % should be ~1
    Info.G         = G;
    Info.R         = R;
end