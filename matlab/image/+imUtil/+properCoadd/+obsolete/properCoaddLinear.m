function [R, P_R, Info] = properCoaddLinear(M, P, Sigma_M, W_M, Args)
    % Proper image coaddition (Zackay & Ofek 2017) as a linear system for the coadd R.
    %   Solves directly for the proper coadd R (Eq. 20 of the paper), not for the
    %   deconvolved sky:
    %
    %       A_W R = d_W ,  A_W = sum_j B_j^T W_j B_j ,  d_W = sum_j B_j^T W_j M_j ,
    %
    %   where B_j = F_j P_j G maps the coadd to image j, G is the band-limited
    %   inverse of the coadd response D = F_R P_R (Ghat = 1/Dhat on the retained
    %   Fourier band, 0 outside), and W_j = diag(omega_j) with
    %   omega_j = W_M/Sigma_M^2 (times the robust IRLS weight, if requested).
    %   The image model is
    %
    %       M_j = F_j*(P_j conv T) + eps_j = B_j R0 + eps_j ,  R0 = F_R*(P_R conv T).
    %
    %   For spatially uniform weights A_W = I on the retained band, so R = d_W,
    %   which is exactly the Zackay & Ofek Fourier coadd Rhat = Shat/Dhat (a single
    %   FFT pass). For per-pixel weights, masks, regularization or robust (IRLS)
    %   rejection, A_W R = d_W is solved by conjugate gradients with FFT-based
    %   operator applications. D, P_R, F_R and G are computed once from the
    %   representative per-image weights omegaBar_j (spatial mean of the input
    %   weights) and kept fixed, so the coadd PSF is P_R for any weights. No
    %   estimate of the deconvolved sky is formed.
    %
    % Input  : - M       : Cube of registered, background-subtracted images,
    %                       size [Ny, Nx, Nim].
    %          - P        : Cube of PSF stamps, size [ny, nx, Nim] (usually much
    %                       smaller than the images) or full [Ny, Nx, Nim].
    %                       Each stamp is assumed centered on its central pixel.
    %          - Sigma_M  : Background noise. Scalar | per-image [1,1,Nim] or
    %                       vector of length Nim | per-pixel cube [Ny,Nx,Nim] or
    %                       [Ny,Nx]. Interpreted as noise std unless
    %                       'SigmaIsVariance' is true. Default is 1.
    %          - W_M      : Image weights (multiplicative inverse-variance factor,
    %                       0 = masked). Same shape options as Sigma_M.
    %                       Default is 1.
    %          * ...,key,val,...
    %            'Flux'          - Vector of per-image transparencies F_j
    %                              (length Nim). Default is ones -> [1,1,Nim].
    %            'SigmaIsVariance'- If true, Sigma_M holds variance rather than
    %                              std. Default is false.
    %            'NormPSF'       - Normalize each PSF stamp to unit sum.
    %                              Default is true.
    %            'Robust'        - Perform IRLS outlier rejection. Default false.
    %            'RobustFun'     - 'Tukey' (hard redescending) | 'Huber' (soft).
    %                              Default is 'Tukey'.
    %            'RobustPar'     - Tuning constant c of the robust weight
    %                              (in units of sigma). [] uses 4.685 for Tukey,
    %                              1.345 for Huber. Default is [].
    %            'MaxIter'       - Max IRLS iterations. Default is 4.
    %            'Tol'           - Relative change in R between IRLS iterations,
    %                              ||R_k+1 - R_k||/||R_k|| (Eq. 44), to stop IRLS.
    %                              Default is 1e-4.
    %            'RegParam'      - Tikhonov lambda: adds lambda*||G R||^2
    %                              (= lambda*||T||^2) to the objective (Eq. 23).
    %                              Default 0.
    %            'EtaBand'       - Retained-band threshold eta_band (Eq. 32):
    %                              frequencies with |Dhat|^2 <= EtaBand*max|Dhat|^2
    %                              are set to zero. Default is 1e-12.
    %            'CGTol'         - Relative residual tolerance for CG,
    %                              ||d_W - A_W R||/||d_W||. Default is 1e-6.
    %            'CGMaxIter'     - Max CG iterations. Default is 100.
    %            'PsfOutSize'    - [py px] to crop the returned centered P_R.
    %                              [] returns full [Ny,Nx]. Default is [].
    % Output : - R    : Proper coadd image, [Ny,Nx]. In the uniform-weight,
    %                   non-robust case its noise is white with unit variance.
    %          - P_R  : Coadd PSF, [Ny,Nx] (or cropped), centered, unit sum.
    %          - Info : Struct with fields: Method ('direct'|'cg'), F_R,
    %                   NiterRobust, CGiter (CG iterations of each linear solve),
    %                   BandFrac (fraction of retained Fourier modes), SigmaR
    %                   (robust std of R), ReducedChi2 (approx., weighted
    %                   residual chi^2 per weighted pixel).
    % Author : Claude + Eran Ofek (Jul 2026)
    % Reference: Zackay & Ofek 2017, ApJ, 836, 188.
    % Example: % three gaussian-PSF frames with different seeing/noise
    %          Ny=128; Nx=128; Nim=3;
    %          M=zeros(Ny,Nx,Nim); P=zeros(15,15,Nim);
    %          sig=[1.3 2.0 1.6]; F=[1 0.8 1.1]; B=[1 1.5 0.8];
    %          T=zeros(Ny,Nx); T(64,64)=200; T(40,90)=120;
    %          [xx,yy]=meshgrid(-7:7,-7:7);
    %          for j=1:Nim
    %              g=exp(-(xx.^2+yy.^2)/(2*sig(j)^2)); g=g/sum(g(:));
    %              P(:,:,j)=g;
    %              M(:,:,j)=F(j)*imUtilConv(T,g)+B(j)*randn(Ny,Nx); %#ok
    %          end
    %          [R,P_R]=imUtil.properCoadd.properCoaddLinear(M,P,B,1,'Flux',F);
    %          % robust version rejecting cosmic rays:
    %          M(70,70,2)=M(70,70,2)+900;
    %          [Rr,~]=imUtil.properCoadd.properCoaddLinear(M,P,B,1,'Flux',F,'Robust',true);
    %
    %
    % Usage tips (setting the robustness parameters):
    %   The default call (Sigma_M=1, W_M=1, Robust=false) returns the exact
    %   Zackay & Ofek Fourier coadd via a single FFT pass. Robustness is opt-in;
    %   the CG solver (and any rejection) is engaged only when W_M or Sigma_M is
    %   per-pixel, RegParam>0, or Robust=true. The knobs below control it.
    %
    %   1. Sigma_M is the single most important parameter. Rejection thresholds
    %      the *standardized* residual u = (M - B_j R)/Sigma_M against the tuning
    %      constant, so Sigma_M must be the true noise scale of each frame. If it
    %      is too large, real outliers look like <1 sigma fluctuations and are
    %      kept; if too small, valid pixels are rejected and the coadd loses
    %      depth. For real data pass the per-pixel or per-image background noise.
    %      For noiseless simulations pass a robust per-frame scale, e.g.
    %          for I=1:Nim, x=M(:,:,I);
    %              Sig(I)=1.4826*median(abs(x(:)-median(x(:)))); end
    %      and give Sigma_M as that [1,1,Nim] vector. A wrong Sigma_M silently
    %      defeats rejection far more often than any other setting.
    %
    %   2. Robust (false | true). Leave false for clean data (fastest, exact,
    %      unit-variance R). Set true only when frames contain outliers not
    %      already masked by W_M (cosmic rays, satellite trails, ghosts,
    %      uncleaned transients). Rejection needs the other frames to outvote the
    %      bad one, so it is meaningful only for Nim >~ 4-5; with 2-3 frames use
    %      W_M masking instead.
    %
    %   3. RobustFun ('Tukey' | 'Huber').
    %        Tukey (biweight): redescending, gives *hard* rejection (weight -> 0
    %          beyond RobustPar sigma). Best final pass for gross outliers, but
    %          non-convex, so it needs a good starting solution (see MaxIter note).
    %        Huber: soft, only down-weights the tails (weight ~ c/|u|), never
    %          rejects outright, and is convex/stable. Prefer it for mild, heavy-
    %          tailed noise.
    %
    %   4. RobustPar is the tuning constant c, in units of sigma. [] uses the
    %      standard values c=4.685 (Tukey) and c=1.345 (Huber), which reject at
    %      ~4.7 sigma / down-weight beyond ~1.3 sigma. Lower c = more aggressive
    %      rejection (cleaner, but risks clipping real bright pixels and biasing
    %      photometry); higher c = more permissive. For crowded or high-dynamic-
    %      range fields raise Tukey's c (e.g. 6-8) so stellar cores are not
    %      mistaken for outliers; only lower it if genuine artifacts survive.
    %
    %   5. MaxIter / Tol control the IRLS loop. Because the seed is the non-robust
    %      solution (already correct away from outliers), 3-6 iterations suffice;
    %      Tol=1e-4 (relative change in R) stops it early. More iterations
    %      rarely help and, with Tukey, can chase local minima. Each iteration is
    %      one warm-started CG solve, so cost scales with MaxIter.
    %
    %   6. W_M (pre-masking) is the most reliable tool for *large* single-pixel
    %      spikes: a huge value corrupts the non-robust seed that IRLS starts
    %      from, so mask such pixels up front by setting W_M=0 there (e.g. flag
    %      |M-median(M,3)| > k*MAD with a high k so real signal survives), then
    %      coadd. W_M and Robust compose: masks are honored and the robust weight
    %      multiplies into them. Prefer W_M masking for known-bad pixels/columns
    %      and CRs; reserve IRLS for statistical, unknown outliers.
    %
    %   7. EtaBand / RegParam. Frequencies where the combined response is
    %      negligible are removed by the retained band (EtaBand), so RegParam is
    %      seldom needed. A nonzero RegParam adds lambda*||G R||^2 and trades a
    %      little of R's properness for stability against poorly constrained
    %      modes introduced by large masks.
    %
    %   Note on properness under rejection: with per-pixel or robust weights, R
    %   keeps the global PSF P_R (for fixed weights E[R] = F_R*(P_R conv T)), but
    %   its noise is no longer exactly white: Cov(R) ~ inv(A_W), which is
    %   correlated and non-stationary near masked or down-weighted pixels. When
    %   such pixels are sparse and differ between images, R is close to white,
    %   unit-variance noise.


    arguments
        M
        P
        Sigma_M                        = 1
        W_M                            = 1
        Args.Flux                      = []
        Args.SigmaIsVariance logical   = false
        Args.NormPSF logical           = true
        Args.Robust logical            = false
        Args.RobustFun                 = 'Tukey'
        Args.RobustPar                 = []
        Args.MaxIter                   = 4
        Args.Tol                       = 1e-4
        Args.RegParam                  = 0
        Args.EtaBand                   = 1e-12
        Args.CGTol                     = 1e-6
        Args.CGMaxIter                 = 100
        Args.PsfOutSize                = []
    end

    % ------------------------------------------------------------------ sizes
    [Ny, Nx, Nim] = size(M);

    % ------------------------------------------------------------- transparency
    if isempty(Args.Flux)
        F = ones(1,1,Nim);
    else
        F = reshape(Args.Flux, 1, 1, Nim);
    end

    % --------------------------------------------- broadcastable noise & weights
    SigVar = broadcastArg(Sigma_M, Nim, Ny, Nx);       % noise (std or var)
    if ~Args.SigmaIsVariance
        SigVar = SigVar.^2;                            % -> variance
    end
    Wuser  = broadcastArg(W_M, Nim, Ny, Nx);           % user weights (>=0)

    % effective inverse-variance weight  omega = W / sigma^2   (per pixel/image)
    Omega  = Wuser ./ SigVar;

    % representative per-image scalar weight omegaBar_j (Eq. 12); fixed
    OmegaBar = mean(mean(Omega,1),2);                  % [1,1,Nim]
    OmegaBar = OmegaBar + zeros(1,1,Nim);              % ensure length Nim

    % --------------------------------------------------------------- PSF -> OTF
    OTF = complex(zeros(Ny, Nx, Nim));
    for Iim = 1:Nim
        Ps = P(:,:,Iim);
        if Args.NormPSF
            Ps = Ps ./ sum(Ps(:));
        end
        OTF(:,:,Iim) = psf2otfLocal(Ps, [Ny Nx]);
    end

    % ------------------ coadd response D, PSF P_R and transparency F_R (Eqs. 12-14)
    Dsq    = sum( (F.^2 .* OmegaBar) .* abs(OTF).^2, 3);   % |Dhat|^2 = F_R^2*|Phat_R|^2
    Dhat   = sqrt(Dsq);
    F_R    = sqrt( sum(F.^2 .* OmegaBar, 3) );             % scalar
    Phat_R = Dhat ./ F_R;                                  % real, >=0, DC=1
    Phat_R(~isfinite(Phat_R)) = 0;

    P_R = fftshift(real(ifft2(Phat_R)));
    if ~isempty(Args.PsfOutSize)
        P_R = cropCenter(P_R, Args.PsfOutSize);
    end

    % -------------------- retained band and band-limited inverse G (Eqs. 32-33)
    Band = Dsq > Args.EtaBand * max(Dsq(:));
    Ghat = zeros(Ny, Nx);
    Ghat(Band) = 1 ./ Dhat(Band);

    % ------------- coadd-coordinate forward operators B_j = F_j P_j G (Eq. 30)
    Bhat = (F .* OTF) .* Ghat;                             % [Ny,Nx,Nim]
    clear OTF

    % ---------------------------------------------- choose solution strategy ---
    UniformSig = (size(SigVar,1)==1 && size(SigVar,2)==1);
    UniformW   = (size(Wuser,1)==1  && size(Wuser,2)==1);
    UseDirect  = UniformSig && UniformW && ~Args.Robust && Args.RegParam==0;

    Info = struct('Method','', 'F_R',F_R, 'NiterRobust',0, 'CGiter',[], ...
                  'BandFrac',mean(Band(:)), 'SigmaR',NaN, 'ReducedChi2',NaN);

    % right-hand side d_W = sum_j B_j^T W_j M_j (Eq. 34)
    dW = applyBt(Bhat, Omega .* M);

    if UseDirect
        % uniform weights: A_W = I on the retained band, so R = d_W (Eq. 27),
        % i.e., Rhat = Shat/Dhat (Zackay & Ofek 2017).
        R = dW;
        Info.Method = 'direct';
    else
        % Preconditioner: inverse of the shift-invariant part of the operator,
        % (1 + lambda*Ghat^2)^-1 on the retained band (exact for uniform weights).
        % For lambda=0 it is the projection onto the retained band, i.e., plain CG
        % on A_W R = d_W.
        Ghat2 = Ghat.^2;
        Qinv  = zeros(Ny, Nx);
        Qinv(Band) = 1 ./ (1 + Args.RegParam .* Ghat2(Band));
        preFun = @(X) real(ifft2( fft2(X) .* Qinv ));

        [R, cgit] = solveCoaddEq(Bhat, Omega, dW, Args.RegParam, Ghat2, ...
                                 preFun, Args.CGTol, Args.CGMaxIter, zeros(Ny,Nx));
        Info.CGiter = cgit;

        if Args.Robust
            Sig   = sqrt(SigVar);
            cTune = robustTune(Args.RobustFun, Args.RobustPar);
            for Iter = 1:Args.MaxIter
                % prediction B_j R, residual and standardized residual (Eqs. 36-38)
                Uarr  = (M - applyB(Bhat, R)) ./ Sig;
                Wrob  = robustWeight(Uarr, Args.RobustFun, cTune);   % [Ny,Nx,Nim]
                Omega = (Wuser .* Wrob) ./ SigVar;                   % Eq. 41
                dW    = applyBt(Bhat, Omega .* M);

                [Rnew, cgit] = solveCoaddEq(Bhat, Omega, dW, Args.RegParam, Ghat2, ...
                                 preFun, Args.CGTol, Args.CGMaxIter, R);
                Info.CGiter(end+1) = cgit;
                RelChg = norm(Rnew(:)-R(:)) / max(norm(R(:)), eps);  % Eq. 44
                R = Rnew;
                Info.NiterRobust = Iter;
                if RelChg < Args.Tol
                    break;
                end
            end
        end
        Info.Method = 'cg';
    end

    % ---- diagnostics: white-noise level estimated robustly on the coadd ------
    Info.SigmaR = 1.4826 * median(abs(R(:) - median(R(:))));
    if nargout > 2
        Res  = M - applyB(Bhat, R);
        Chi2 = Omega .* Res.^2;
        Npix = (Omega > 0) + zeros(size(Res));
        Info.ReducedChi2 = sum(Chi2(:)) / max(sum(Npix(:)), 1);
    end
end

% ======================================================================= locals
function Y = applyB(Bhat, V)
    % Coadd -> images: B_j V for all j. [Ny,Nx] -> [Ny,Nx,Nim]
    Y = real(ifft2( Bhat .* fft2(V) ));
end

function Y = applyBt(Bhat, X)
    % Images -> coadd: sum_j B_j^T X_j. [Ny,Nx,Nim] -> [Ny,Nx]
    Y = real(ifft2( sum(conj(Bhat) .* fft2(X), 3) ));
end

function [R, cgit] = solveCoaddEq(Bhat, Omega, dW, lambda, Ghat2, preFun, tol, maxit, R0)
    % Preconditioned CG for A_W R = d_W (Eq. 20), or, for lambda>0,
    % (A_W + lambda*G'G) R = d_W (Eq. 23), with
    %   A_W v = sum_j B_j^T (Omega_j .* (B_j v))   (Eq. 35).
    if lambda > 0
        Afun = @(V) applyBt(Bhat, Omega .* applyB(Bhat, V)) + ...
                    lambda .* real(ifft2( Ghat2 .* fft2(V) ));
    else
        Afun = @(V) applyBt(Bhat, Omega .* applyB(Bhat, V));
    end

    R     = R0;
    r     = dW - Afun(R);
    dnorm = norm(dW(:)) + eps;
    cgit  = 0;
    if norm(r(:)) <= tol*dnorm
        return;
    end
    z  = preFun(r);
    p  = z;
    rz = sum(r(:).*z(:));
    for cgit = 1:maxit
        Ap    = Afun(p);
        alpha = rz / sum(p(:).*Ap(:));
        R     = R + alpha*p;
        r     = r - alpha*Ap;
        if norm(r(:)) <= tol*dnorm
            break;
        end
        z     = preFun(r);
        rzNew = sum(r(:).*z(:));
        p     = z + (rzNew/rz)*p;
        rz    = rzNew;
    end
end

function OTF = psf2otfLocal(Psf, OutSize)
    % Zero-pad a centered PSF stamp to OutSize and circularly shift its center
    % to pixel (1,1), then FFT. Mirrors MATLAB's psf2otf (no toolbox needed).
    [ny, nx] = size(Psf);
    Padded = zeros(OutSize);
    Padded(1:ny, 1:nx) = Psf;
    Padded = circshift(Padded, -floor([ny nx]/2));
    OTF    = fft2(Padded);
end

function Y = broadcastArg(X, Nim, Ny, Nx) %#ok<INUSD>
    % Reshape a scalar / per-image vector / per-pixel array into a form that
    % broadcasts against an [Ny,Nx,Nim] cube via implicit expansion.
    if isscalar(X)
        Y = X;
    elseif isvector(X) && numel(X)==Nim
        Y = reshape(X, 1, 1, Nim);
    else
        Y = X;                       % assumed [Ny,Nx] or [Ny,Nx,Nim]
    end
end

function c = robustTune(Fun, Par)
    if ~isempty(Par)
        c = Par; return;
    end
    switch lower(Fun)
        case 'tukey', c = 4.685;
        case 'huber', c = 1.345;
        otherwise,    c = Inf;
    end
end

function W = robustWeight(U, Fun, c)
    % IRLS weight w(u) = psi(u)/u for the chosen loss, evaluated per pixel/image.
    Au = abs(U);
    switch lower(Fun)
        case 'huber'
            W = ones(size(Au));
            Big = Au > c;
            W(Big) = c ./ Au(Big);
        case 'tukey'
            W = (1 - (Au./c).^2).^2;
            W(Au >= c) = 0;
        otherwise
            W = ones(size(Au));
    end
end

function Out = cropCenter(Img, Sz)
    [Ny, Nx] = size(Img);
    py = Sz(1); px = Sz(2);
    cy = floor(Ny/2) + 1;  cx = floor(Nx/2) + 1;
    ry = cy + (-floor(py/2):ceil(py/2)-1);
    rx = cx + (-floor(px/2):ceil(px/2)-1);
    Out = Img(ry, rx);
end
