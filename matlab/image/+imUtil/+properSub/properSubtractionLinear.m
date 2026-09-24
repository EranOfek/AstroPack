function [Scorr, D, P_D, Info] = properSubtractionLinear(N, R, Pn, Pr, Args)
% Proper image subtraction (ZOGY) as a real-space weighted linear system.
% Package: imUtil.properSub
% Description:
%   Transient detection between a new image N and a reference R with known
%   PSFs, generalizing ZOGY (Zackay, Ofek & Gal-Yam 2016) to PER-PIXEL
%   weights. Under the null hypothesis the two images share a static sky T:
%       N = F_n (P_n conv T) + eps_n,   R = F_r (P_r conv T) + eps_r,
%   with per-pixel inverse-variance weights Omega_n, Omega_r that absorb
%   background noise, SOURCE noise, masks, and robust down-weighting. The
%   sky is fitted by generalized least squares (conjugate gradients with an
%   FFT preconditioner), and transient detection at each position is the
%   PROFILED SCORE test:
%       U     = F_n P_n^T Omega_n (N - F_n P_n conv That)
%       V(q)  = I_aa(q) - [B Nw^{-1} B]_qq ,  B = F_n^2 P_n^T Omega_n P_n
%       Scorr = U ./ sqrt(V)
%   which reduces EXACTLY to the ZOGY statistic S (and its S_corr) when the
%   weights are uniform (verified to the CG tolerance, ~3e-6 relative). The
%   difference from ZOGY's own S_corr is fundamental: there, per-pixel
%   variance enters only the NORMALIZATION of the uniform-theory statistic;
%   here the weights enter the ESTIMATOR, so noisy pixels are down-weighted
%   rather than the whole aperture being penalized, masked pixels
%   (Omega = 0) are ignored exactly, and IRLS outlier rejection is
%   available. In simulation this converts ~40-60 sigma false positives at
%   bright stars and cosmic rays into ~1-2 sigma, while INCREASING the
%   recovered significance of a real transient relative to variance-map
%   S_corr.
%
% --------- Robust rejection: DISCOURAGED here; use masks instead -----------
%   'Robust' is available but OFF by default and should generally stay off.
%   Subtraction is the N=2 case, below the quorum (N >~ 4-5 frames) that
%   makes IRLS rejection safe in coaddition: at the pixel level a real
%   transient and an artifact are both residuals against the static sky,
%   so IRLS clips real events. Measured: a faint transient drops
%   ~9.2 -> 7.5 sigma with Tukey on both images; brighter events fare
%   worse. Note also that 'RobustImages'='ref' is safe only for
%   BRIGHTENING events: a FADING transient (bright in R, absent in N)
%   places its outlier residual in the reference and is suppressed by
%   ref-side rejection. The recommended architecture is: perform robust
%   rejection UPSTREAM, when building the reference with
%   properCoaddLinear (where the frame quorum exists), and keep the
%   subtraction strictly LINEAR: per-pixel weights for source and
%   astrometric noise, plus STATIC masks (Wn/Wr = 0 from a CR finder,
%   saturation map, or bad-pixel list -- masks derive from external
%   information, not from the residuals, and therefore cannot reject a
%   transient). If 'Robust' is used regardless, protect candidate
%   positions with 'ProtectPos' and treat negative detections with
%   suspicion.
% -------------------------------------------------------------------------
%
%   Conventions follow properCoaddLinear: PSF stamps centered on pixel
%   floor(n/2)+1 (any parity), embedded corner-style; periodic boundaries
%   (pad/apodize real data); double precision assumed.
%
% Input  : - N  - New image (background subtracted), [Ny,Nx].
%          - R  - Reference image (background subtracted), [Ny,Nx].
%          - Pn - PSF stamp of N (centered; any odd/even size <= image).
%          - Pr - PSF stamp of R.
%          * ...,key,val,...
%            'SigmaN','SigmaR' - Background noise of N and R: scalar or
%                     per-pixel [Ny,Nx]. Std unless 'SigmaIsVariance'.
%                     Default is 1.
%            'SigmaIsVariance' - Default is false.
%            'Fn','Fr' - Flux zero-points (transparencies). Default is 1.
%            'Wn','Wr' - Multiplicative weights / masks (0 = ignore pixel).
%                     Scalar or [Ny,Nx]. Default is 1.
%            'SourceNoise' - Add model-based source-noise variance
%                     max(model,0)/Gain to each image's variance, with the
%                     model flux derived from the REFERENCE (regularized
%                     projection), not from the noisy data (data-derived
%                     weights bias fluxes low). Default is true.
%            'Gain' - Electrons per data unit for source noise.
%                     Default is 1.
%            'RegProj' - Regularization of the reference projection used
%                     for the source-noise model (relative to max|Pr_f|^2).
%                     Default is 3e-3.
%            'Robust' - IRLS artifact rejection. Default is false.
%            'RobustImages' - 'ref' (default) | 'new' | 'both'. See above.
%            'RobustFun' - 'Tukey' | 'Huber'. Default is 'Tukey'.
%            'RobustPar' - Tuning constant c (sigma units). [] = 4.685 /
%                     1.345. Default is [].
%            'MaxIter' - Max IRLS iterations. Default is 4.
%            'Tol' - Relative change in That to stop IRLS. Default 1e-4.
%            'ProtectPos' - [K x 2] (y,x) positions whose footprints are
%                     excluded from robust down-weighting in N.
%                     Default is [].
%            'ProtectRadius' - Radius (pix) of protected footprints.
%                     Default is 5.
%            'CGTol' - CG relative residual tolerance. Default is 1e-8.
%            'CGMaxIter' - Default is 300.
%            'NormPSF' - Normalize PSF stamps to unit sum. Default true.
%            'Renormalize' - Divide Scorr by its robustly measured
%                     background std (the locally-uniform Fisher is
%                     slightly conservative, ~25% in tests), so the output
%                     background is unit-variance. The raw factor is
%                     reported in Info.BkgStdRaw. Default is true.
% Output : - Scorr - Transient significance map (profiled score / sqrt of
%                     profiled Fisher), background ~ N(0,1) after
%                     renormalization. Positive peaks = brightening in N;
%                     negative = fading (or new-image artifact if masked
%                     imperfectly).
%          - D     - ZOGY proper difference image computed with the final
%                     REPRESENTATIVE (spatial-mean) weights: exact where
%                     weights are uniform, indicative elsewhere. For
%                     photometry of detected transients.
%          - P_D   - PSF of D (centered, unit sum), with zero-point
%                     Info.F_D.
%          - Info  - Struct: That (fitted sky), U, V (score numerator and
%                     variance), Wn, Wr (final per-pixel weights), F_D,
%                     BkgStdRaw, NiterRobust, CGiter, Flags.
%
% Reference: Zackay, Ofek & Gal-Yam 2016, ApJ, 830, 27 (ZOGY);
%            Zackay & Ofek 2017, ApJ, 836, 188; Ofek & Zackay (in prep.).
% Author : Eran O. Ofek + Claude (Jul 2026)
% Example:
%   [Scorr,D,P_D,Info] = imUtil.properSub.properSubtractionLinear(...
%        N, R, Pn, Pr, 'SigmaN',SigN, 'SigmaR',SigR, ...
%        'SourceNoise',true, 'Robust',true, 'RobustImages','ref');
%--------------------------------------------------------------------------

    arguments
        N double
        R double
        Pn double
        Pr double
        Args.SigmaN                     = 1;
        Args.SigmaR                     = 1;
        Args.SigmaIsVariance logical    = false;
        Args.Fn (1,1) double            = 1;
        Args.Fr (1,1) double            = 1;
        Args.Wn                         = 1;
        Args.Wr                         = 1;
        Args.SourceNoise (1,1) logical  = true;
        Args.Gain (1,1) double          = 1;
        Args.RegProj (1,1) double       = 3e-3;
        Args.Robust (1,1) logical       = false;
        Args.RobustImages               = 'ref';
        Args.RobustFun                  = 'Tukey';
        Args.RobustPar                  = [];
        Args.MaxIter (1,1) double       = 4;
        Args.Tol (1,1) double           = 1e-4;
        Args.ProtectPos double          = zeros(0,2);
        Args.ProtectRadius (1,1) double = 5;
        Args.CGTol (1,1) double         = 1e-8;
        Args.CGMaxIter (1,1) double     = 300;
        Args.NormPSF (1,1) logical      = true;
        Args.Renormalize (1,1) logical  = true;
    end

    [Ny, Nx] = size(N);
    if ~isequal(size(R), [Ny Nx])
        error('properSubtractLinear:size', 'N and R must have equal size.');
    end
    Fn = Args.Fn; Fr = Args.Fr;

    % ------------------------------------------------------------ PSFs -> OTF
    if Args.NormPSF
        Pn = Pn ./ sum(Pn(:));  Pr = Pr ./ sum(Pr(:));
    end
    Pn_f = i_psf2otf(Pn, [Ny Nx]);
    Pr_f = i_psf2otf(Pr, [Ny Nx]);

    % ------------------------------------------------------ variance & weights
    Vn = i_expand(Args.SigmaN, Ny, Nx);  Vr = i_expand(Args.SigmaR, Ny, Nx);
    if ~Args.SigmaIsVariance
        Vn = Vn.^2;  Vr = Vr.^2;
    end
    if Args.SourceNoise
        % model flux from the REFERENCE (regularized projection), never from
        % the noisy data themselves (avoids inverse-variance flux bias)
        Reg  = Args.RegProj * max(abs(Pr_f(:)).^2);
        Tmod = max(real(ifft2(fft2(R) .* conj(Pr_f) ./ (abs(Pr_f).^2 + Reg))), 0) ./ max(Fr, eps);
        Vn = Vn + max(Fn .* i_conv(Pn_f, Tmod), 0) ./ Args.Gain;
        Vr = Vr + max(Fr .* i_conv(Pr_f, Tmod), 0) ./ Args.Gain;
    end
    Wn0 = i_expand(Args.Wn, Ny, Nx) ./ Vn;      % initial inverse-variance weights
    Wr0 = i_expand(Args.Wr, Ny, Nx) ./ Vr;
    Omn = Wn0;  Omr = Wr0;

    % protected footprints (kept at full weight during IRLS in N)
    Protect = false(Ny, Nx);
    for K = 1:size(Args.ProtectPos, 1)
        [YY, XX] = ndgrid(1:Ny, 1:Nx);
        Protect = Protect | ((YY - Args.ProtectPos(K,1)).^2 + ...
                             (XX - Args.ProtectPos(K,2)).^2) <= Args.ProtectRadius^2;
    end

    % ------------------------------------------------------------ GLS sky fit
    [That, CGit] = i_solveT(N, R, Pn_f, Pr_f, Fn, Fr, Omn, Omr, ...
                            Args.CGTol, Args.CGMaxIter, zeros(Ny, Nx));
    NiterRobust = 0;

    if Args.Robust
        cT = i_robustTune(Args.RobustFun, Args.RobustPar);
        DoN = any(strcmpi(Args.RobustImages, {'new','both'}));
        DoR = any(strcmpi(Args.RobustImages, {'ref','both'}));
        for Iter = 1:Args.MaxIter
            Un = (N - Fn .* i_conv(Pn_f, That)) .* sqrt(Wn0);
            Ur = (R - Fr .* i_conv(Pr_f, That)) .* sqrt(Wr0);
            if DoN
                Wrob = i_robustWeight(Un, Args.RobustFun, cT);
                Wrob(Protect) = 1;
                Omn = Wn0 .* Wrob;
            end
            if DoR
                Omr = Wr0 .* i_robustWeight(Ur, Args.RobustFun, cT);
            end
            [Tnew, CGit] = i_solveT(N, R, Pn_f, Pr_f, Fn, Fr, Omn, Omr, ...
                                    Args.CGTol, Args.CGMaxIter, That);
            RelChg = norm(Tnew(:) - That(:)) / max(norm(That(:)), eps);
            That = Tnew;  NiterRobust = Iter;
            if RelChg < Args.Tol
                break;
            end
        end
    end

    % ------------------------------------------------- profiled score and Fisher
    U = Fn .* i_corr(Pn_f, Omn .* (N - Fn .* i_conv(Pn_f, That)));

    % locally-uniform profiled Fisher (exact in the uniform-weight limit):
    % PSF^2-smoothed effective weights inserted in the closed-form symbol
    pn = real(ifft2(Pn_f));  pr = real(ifft2(Pr_f));
    WnEff = real(ifft2(conj(fft2(pn.^2)) .* fft2(Omn))) ./ sum(pn(:).^2);
    WrEff = real(ifft2(conj(fft2(pr.^2)) .* fft2(Omr))) ./ sum(pr(:).^2);
    V = i_localFisher(Fn^2.*WnEff, Fr^2.*WrEff, abs(Pn_f).^2, abs(Pr_f).^2);
    V = max(V, 1e-12*max(V(:)));

    Scorr = U ./ sqrt(V);

    % robust background renormalization (locally-uniform Fisher is slightly
    % conservative); reported so significances remain traceable
    BkgStdRaw = 1.4826 * median(abs(Scorr(:) - median(Scorr(:))));
    if Args.Renormalize && BkgStdRaw > 0
        Scorr = Scorr ./ BkgStdRaw;
    end

    % --------------------- ZOGY D and P_D with representative final weights ---
    VnRep = 1 ./ max(mean(Omn(Omn>0), 'all'), eps);
    VrRep = 1 ./ max(mean(Omr(Omr>0), 'all'), eps);
    Den   = VnRep .* Fr^2 .* abs(Pr_f).^2 + VrRep .* Fn^2 .* abs(Pn_f).^2;
    Den   = max(Den, 1e-12*max(Den(:)));
    D_f   = (Fr .* Pr_f .* fft2(N) - Fn .* Pn_f .* fft2(R)) ./ sqrt(Den);
    D     = real(ifft2(D_f));
    F_D   = Fn * Fr / sqrt(VnRep*Fr^2 + VrRep*Fn^2);
    PD_f  = (Fn*Fr .* Pn_f .* Pr_f) ./ (F_D .* sqrt(Den));
    P_D   = fftshift(real(ifft2(PD_f)));

    Info = struct('That',That, 'U',U, 'V',V, 'Wn',Omn, 'Wr',Omr, ...
                  'F_D',F_D, 'BkgStdRaw',BkgStdRaw, ...
                  'NiterRobust',NiterRobust, 'CGiter',CGit, ...
                  'Flags',struct('Robust',Args.Robust, ...
                                 'RobustImages',Args.RobustImages, ...
                                 'SourceNoise',Args.SourceNoise));
end

% ========================================================================
function [T, it] = i_solveT(N, R, Pn_f, Pr_f, Fn, Fr, Omn, Omr, tol, maxit, T0)
    b = Fn .* i_corr(Pn_f, Omn .* N) + Fr .* i_corr(Pr_f, Omr .* R);
    Sym = Fn^2*mean(Omn(:)).*abs(Pn_f).^2 + Fr^2*mean(Omr(:)).*abs(Pr_f).^2;
    Qi  = zeros(size(Sym));  Msk = Sym > 1e-12*max(Sym(:));
    Qi(Msk) = 1 ./ Sym(Msk);
    pre = @(v) real(ifft2(Qi .* fft2(v)));
    Nop = @(v) Fn^2 .* i_corr(Pn_f, Omn .* i_conv(Pn_f, v)) + ...
               Fr^2 .* i_corr(Pr_f, Omr .* i_conv(Pr_f, v));
    T = T0;  r = b - Nop(T);
    bn = norm(b(:));  it = 0;
    if bn == 0, return; end
    z = pre(r);  p = z;  rz = sum(r(:).*z(:));
    for it = 1:maxit
        Ap = Nop(p);  den = sum(p(:).*Ap(:));
        if den <= 0 || rz <= 0, break; end
        al = rz/den;  T = T + al*p;  r = r - al*Ap;
        if norm(r(:)) < tol*bn, break; end
        z = pre(r);  rzn = sum(r(:).*z(:));
        p = z + (rzn/rz)*p;  rz = rzn;
    end
end

function V = i_localFisher(A2wn, B2wr, U, W)
    % V(q) = (1/Np) sum_f [ a*u - (a*u)^2 / (a*u + b*w) ] with a=A2wn(q),
    % b=B2wr(q); chunked over pixels to bound memory.
    [Ny, Nx] = size(A2wn);  Np = Ny*Nx;
    a = A2wn(:);  b = B2wr(:);  u = U(:).';  w = W(:).';
    V = zeros(Np, 1);  Ch = 1024;
    for i0 = 1:Ch:Np
        i1 = min(i0+Ch-1, Np);
        Au = a(i0:i1) * u;             % chunk x Np
        Bw = b(i0:i1) * w;
        V(i0:i1) = sum(Au - Au.^2 ./ max(Au + Bw, realmin), 2) ./ Np;
    end
    V = reshape(V, Ny, Nx);
end

function OTF = i_psf2otf(Psf, OutSize)
    [ny, nx] = size(Psf);
    Pad = zeros(OutSize);  Pad(1:ny, 1:nx) = Psf;
    OTF = fft2(circshift(Pad, -floor([ny nx]/2)));
end

function Y = i_conv(OTF, X),  Y = real(ifft2(OTF .* fft2(X)));        end
function Y = i_corr(OTF, X),  Y = real(ifft2(conj(OTF) .* fft2(X)));  end

function Y = i_expand(X, Ny, Nx)
    if isscalar(X), Y = repmat(X, Ny, Nx); else, Y = X; end
end

function c = i_robustTune(Fun, Par)
    if ~isempty(Par), c = Par; return; end
    switch lower(Fun)
        case 'tukey', c = 4.685;
        case 'huber', c = 1.345;
        otherwise,    c = Inf;
    end
end

function W = i_robustWeight(U, Fun, c)
    Au = abs(U);
    switch lower(Fun)
        case 'huber'
            W = ones(size(Au));  Big = Au > c;  W(Big) = c ./ Au(Big);
        case 'tukey'
            W = (1 - (Au./c).^2).^2;  W(Au >= c) = 0;
        otherwise
            W = ones(size(Au));
    end
end