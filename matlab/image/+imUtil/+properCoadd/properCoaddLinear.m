function [R, P_R, Info] = properCoaddLinear(M, P, Sigma_M, W_M, Args)
    % Proper image coaddition (Zackay & Ofek 2017) as a real-space linear system.
    %   The Zackay & Ofek proper coadd R is the closed-form solution, in Fourier
    %   space, of the generalized-least-squares problem of estimating the sky T
    %   from a stack of PSF-blurred, background-noise-dominated images:
    %
    %       M_j = F_j*(P_j conv T) + eps_j ,   Var(eps_j)=Sigma_M_j^2 .
    %
    %   Writing the GLS normal equations gives, for the maximum-likelihood sky
    %   Tml, the system  N*Tml = S  with
    %       N = sum_j (F_j^2/v_j) P_j^T P_j ,   S = sum_j (F_j/v_j) P_j^T M_j ,
    %   where v_j is the (possibly per-pixel) effective noise variance and P_j^T
    %   is cross-correlation with the j-th PSF. The proper image is the ML sky
    %   *re-blurred* by the coadd PSF, R = F_R*(P_R conv Tml), which is exactly
    %   the whitening step that turns the (colored-noise) matched filter into an
    %   image with white, unit-variance noise. Equivalently R solves the well
    %   conditioned linear equation
    %
    %       F_R*(P_R conv R) = S .
    %
    %   Because the operator is only shift-invariant when the weights are uniform
    %   per image, casting the coadd as a linear system is what enables per-pixel
    %   weighting -> masking and robust (IRLS) outlier rejection, which the pure
    %   FFT form cannot do. When the weights are uniform and no rejection is
    %   requested, the exact Fourier closed form is used (one FFT solve); when
    %   they are per-pixel or robust, a preconditioned conjugate-gradient solver
    %   is used, with FFT-based matrix-vector products. P_R is always evaluated
    %   in Fourier space from the input PSFs and the representative weights.
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
    %            'Tol'           - Relative change in T to stop IRLS.
    %                              Default is 1e-4.
    %            'RegParam'      - Tikhonov lambda added to the normal operator
    %                              (stabilizes true null frequencies). Default 0.
    %            'CGTol'         - Relative residual tolerance for CG.
    %                              Default is 1e-6.
    %            'CGMaxIter'     - Max CG iterations. Default is 100.
    %            'PsfOutSize'    - [py px] to crop the returned centered P_R.
    %                              [] returns full [Ny,Nx]. Default is [].
    % Output : - R    : Proper coadd image, [Ny,Nx]. In the uniform-weight,
    %                   non-robust case its noise is white with unit variance.
    %          - P_R  : Coadd PSF, [Ny,Nx] (or cropped), centered, unit sum.
    %          - Info : Struct with fields: Method ('direct'|'cg'), F_R,
    %                   NiterRobust, CGiter, ReducedChi2 (approx), SigmaR.
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
    %   Zackay & Ofek Fourier coadd via a single FFT solve. Robustness is opt-in;
    %   the pixel-space CG solver (and any rejection) is engaged only when W_M is
    %   per-pixel, RegParam>0, or Robust=true. The knobs below control it.
    %
    %   1. Sigma_M is the single most important parameter. Rejection thresholds
    %      the *standardized* residual u = (M - model)/Sigma_M against the tuning
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
    %          non-convex, so it needs a good starting sky (see MaxIter note).
    %        Huber: soft, only down-weights the tails (weight ~ c/|u|), never
    %          rejects outright, and is convex/stable. Prefer it for mild, heavy-
    %          tailed noise, or as a first pass before Tukey. A robust default
    %          schedule is one Huber solve to clean the seed, then Tukey.
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
    %      Tol=1e-4 (relative change in the sky) stops it early. More iterations
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
    %   7. RegParam (Tikhonov lambda) only stabilizes true null frequencies
    %      (where every PSF has zero response). Keep it 0 or tiny; any nonzero
    %      value trades a little of R's properness (unit-variance whiteness) for
    %      numerical stability. CG already returns the minimum-norm solution at
    %      nulls, so RegParam is seldom needed.
    %
    %   Note on properness under rejection: where pixels are down-weighted the
    %   local effective PSF/noise depart slightly from the global P_R that R is
    %   re-blurred to, so R is only *locally* proper near rejected regions; in
    %   outlier-free areas (weights = 1) the exact proper coadd is recovered.


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

    % representative per-image scalar weight (for P_R, F_R, preconditioner)
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
    OTFc = conj(OTF);

    % ------------------------- coadd PSF and effective transparency (Fourier) --
    Dsq    = sum( (F.^2 .* OmegaBar) .* abs(OTF).^2, 3);   % = F_R^2 * P_R^2
    Dhat   = sqrt(Dsq);
    F_R    = sqrt( sum(F.^2 .* OmegaBar, 3) );             % scalar
    Phat_R = Dhat ./ F_R;                                  % real, >=0, DC=1
    Phat_R(~isfinite(Phat_R)) = 0;

    P_R = fftshift(real(ifft2(Phat_R)));
    if ~isempty(Args.PsfOutSize)
        P_R = cropCenter(P_R, Args.PsfOutSize);
    end

    % ---------------------------------------------- choose solution strategy ---
    UniformSig = (size(SigVar,1)==1 && size(SigVar,2)==1);
    UniformW   = (size(Wuser,1)==1  && size(Wuser,2)==1);
    UseDirect  = UniformSig && UniformW && ~Args.Robust && Args.RegParam==0;

    Info = struct('Method','', 'F_R',F_R, 'NiterRobust',0, ...
                  'CGiter',0, 'SigmaR',NaN, 'ReducedChi2',NaN);

    if UseDirect
        % ----------------------- exact Fourier closed form (one FFT solve) -----
        Mhat = fft2(M);
        Shat = sum( (F.*Omega) .* OTFc .* Mhat, 3);
        Rhat = zeros(Ny,Nx);
        Good = Dhat > 0;
        Rhat(Good) = Shat(Good) ./ Dhat(Good);
        R = real(ifft2(Rhat));
        Info.Method = 'direct';
    else
        % ----------------------- pixel-space linear system, solved by PCG ------
        % Precondition with the shift-invariant operator (symbol Qsym); it equals
        % the full operator when weights are uniform, so CG then converges in 1
        % iteration. Regularization lambda stabilizes true null frequencies.
        Qsym = Dsq + Args.RegParam;
        Qinv = zeros(Ny,Nx);
        Thr  = max(Qsym(:)) * 1e-12;
        Msk  = Qsym > Thr;
        Qinv(Msk) = 1 ./ Qsym(Msk);
        preFun = @(Res) real(ifft2( fft2(Res) .* Qinv ));

        % initial weights and solve
        Omega = Wuser ./ SigVar;
        [T, cgit] = solveNormalEq(M, OTF, OTFc, F, Omega, Args.RegParam, ...
                                  preFun, Args.CGTol, Args.CGMaxIter, zeros(Ny,Nx));
        Info.CGiter = cgit;

        if Args.Robust
            Sig = sqrt(SigVar);
            cTune = robustTune(Args.RobustFun, Args.RobustPar);
            for Iter = 1:Args.MaxIter
                % per-image residuals and standardized deviates
                Model = F .* real(ifft2( OTF .* fft2(T) ));   % F_j*(P_j conv T)
                Uarr  = (M - Model) ./ Sig;
                Wrob  = robustWeight(Uarr, Args.RobustFun, cTune);   % [Ny,Nx,Nim]
                Omega = (Wuser .* Wrob) ./ SigVar;

                [Tnew, cgit] = solveNormalEq(M, OTF, OTFc, F, Omega, ...
                                  Args.RegParam, preFun, Args.CGTol, ...
                                  Args.CGMaxIter, T);
                Info.CGiter = cgit;
                RelChg = norm(Tnew(:)-T(:)) / max(norm(T(:)), eps);
                T = Tnew;
                Info.NiterRobust = Iter;
                if RelChg < Args.Tol
                    break;
                end
            end
        end

        % re-blur the (cleaned) sky by the globally-defined coadd PSF:
        R = real(ifft2( Dhat .* fft2(T) ));     % = F_R*(P_R conv T)
        Info.Method = 'cg';
    end

    % ---- diagnostics: white-noise level estimated robustly on the coadd ------
    Info.SigmaR = 1.4826 * median(abs(R(:) - median(R(:))));
end

% ======================================================================= locals
function [T, cgit] = solveNormalEq(M, OTF, OTFc, F, Omega, lambda, preFun, tol, maxit, T0)
    % Preconditioned CG for  N*T = b  with
    %   N v = sum_j F_j^2 * corr(P_j, Omega_j .* conv(P_j,v)) + lambda*v
    %   b   = sum_j F_j    * corr(P_j, Omega_j .* M_j)
    Ny = size(M,1); Nx = size(M,2);
    F2 = F.^2;

    bRHS = sum( F .* real(ifft2( OTFc .* fft2(Omega .* M) )), 3);

    Afun = @(V) applyN(V);

    T = T0;
    r = bRHS - Afun(T);
    z = preFun(r);
    p = z;
    rz = sum(r(:).*z(:));
    bnorm = norm(bRHS(:)) + eps;
    cgit = 0;
    for cgit = 1:maxit
        Ap    = Afun(p);
        alpha = rz / sum(p(:).*Ap(:));
        T     = T + alpha*p;
        r     = r - alpha*Ap;
        if norm(r(:)) <= tol*bnorm
            break;
        end
        z      = preFun(r);
        rzNew  = sum(r(:).*z(:));
        beta   = rzNew / rz;
        p      = z + beta*p;
        rz     = rzNew;
    end

    function Y = applyN(V)
        Vh   = fft2(V);
        Conv = real(ifft2( OTF .* Vh ));           % conv(V,P_j)  [Ny,Nx,Nim]
        Tmp  = Omega .* Conv;                       % apply per-pixel weight
        Corr = real(ifft2( OTFc .* fft2(Tmp) ));    % corr(.,P_j)
        Y    = sum( F2 .* Corr, 3) + lambda .* V;
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