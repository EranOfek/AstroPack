function [R, PR, R_f, PR_f, F_R] = properCoaddFFT(Data, PSF, Args)
    % Proper coaddition of images in a cube (self-contained, convention-pinned)
    %   Package: imUtil.properCoadd
    %   Description: Proper coaddition (Zackay & Ofek 2017, ApJ 836, 188) of a cube
    %              of registered, background-subtracted images. In Fourier space:
    %
    %                  R_f  = sum_j (F_j/V_j) conj(P_j_f) M_j_f  /  D_f
    %                  D_f  = sqrt( sum_j (F_j^2/V_j) |P_j_f|^2 )
    %                  PR_f = D_f / F_R ,   F_R = sqrt( sum_j F_j^2/V_j )
    %
    %              R has white, unit-variance noise when V_j is the true
    %              background variance of image j and F_j its flux zero-point.
    %
    % -------------------------------- CONVENTIONS --------------------------------
    %   These conventions are chosen to be identical to properCoaddLinear (and to
    %   MATLAB's psf2otf / ifftshift), for BOTH odd- and even-sized stamps/images:
    %
    %   1. STAMP CENTER ('PsfCenter'): the pixel of the PSF stamp that represents
    %      the source position is moved to the FFT origin, so that a source at
    %      pixel p in Data lands at pixel p in R (R is aligned to the input grid;
    %      it is NEVER shifted to the corner). 'PsfCenter' declares where that
    %      center is:
    %          'pixel'    - central pixel ( floor(ny/2)+1 , floor(nx/2)+1 ).
    %                       This is the default and is byte-identical to
    %                       properCoaddLinear / MATLAB's psf2otf.
    %          'corner'   - the stamp is already stored with its center at
    %                       pixel (1,1) (e.g. an already psf2otf'ed / fftshifted
    %                       PSF). Use this if R comes out shifted by ~floor(n/2)
    %                       ("shifted by PR") with 'pixel'.
    %          'centroid' - use each plane's flux-weighted centroid; handles
    %                       arbitrary mis-centering including SUB-PIXEL offsets
    %                       (applied as a Fourier phase ramp). Robust default for
    %                       real, imperfectly centered stamps.
    %          [yc xc]    - explicit center (1-based, may be fractional), applied
    %                       to all planes; or an [Nim x 2] matrix for per-plane
    %                       centers.
    %      NOTE (even sizes): a symmetric PSF sampled on an even grid cannot be
    %      both symmetric and peak-on-pixel. If your even stamps are geometrically
    %      centered (center on the half-pixel boundary), 'pixel' leaves a +0.5 pix
    %      residual shift; use 'centroid' (or an explicit fractional [yc xc]) to
    %      remove it exactly. Odd stamps avoid the ambiguity and are recommended.
    %
    %   2. INTERNAL LAYOUT: PSFs are zero-padded to the image size and circularly
    %      shifted so the center pixel defined above lands at (1,1) ("corner"
    %      layout). All FFTs, and the returned R_f / PR_f, use this layout, so R
    %      is aligned with the input image pixel grid (no phase ramp).
    %
    %   3. OUTPUT LAYOUT ('ShiftToCenter'): with 'ShiftToCenter'=true (default)
    %      the returned PR is CENTERED (peak at floor(N/2)+1 on each axis of the
    %      returned array), matching properCoaddLinear's P_R exactly. With
    %      'ShiftToCenter'=false, PR is returned in corner/FFT layout (peak at
    %      (1,1), wings wrapped), ready for direct use in further FFT-based
    %      filtering without an ifftshift. With 'Full2stamp'=true the stamp-size
    %      crop is applied in either layout (the two are related by
    %      fftshift/ifftshift of the stamp).
    %      The coadd image R is aligned with the input image pixel grid by
    %      construction, in BOTH codes, and is never shifted: 'ShiftToCenter'
    %      affects only PR (and does not affect R_f/PR_f, which are always the
    %      transforms of the corner-layout arrays, as standard for FFT work).
    %
    %   4. NORMALIZATION: PR is normalized to unit sum (DC of PR_f equals 1),
    %      per the paper; the effective coadd zero-point F_R is returned
    %      separately. PSF photometry on R must be divided by F_R to recover
    %      fluxes in the input zero-point. (Cropping by 'Full2stamp', or tapering
    %      by 'AnnulusPost', removes wing flux, so the returned stamp may sum to
    %      slightly less than 1.)
    %
    %   5. 'Var' IS A VARIANCE (sigma^2), not a standard deviation. This differs
    %      from properCoaddLinear's positional Sigma_M argument, which is a std
    %      by default.
    %
    %   6. BOUNDARIES: all convolutions are circular (periodic). Sources within
    %      ~a PSF width of an edge wrap around; pad or apodize real data.
    % ------------------------------------------------------------------------------
    %
    % Input  : - Data - Cube of registered, background-subtracted images,
    %                   size [Ny,Nx,Nim] (or a single [Ny,Nx] image).
    %          - PSF  - Cube of PSF stamps [ny,nx,Nim], or a single [ny,nx] stamp
    %                   shared by all images. Stamps may be smaller than, or equal
    %                   to, the image size. Centered per convention 1.
    %          * ...,key,val,...
    %            'F'   - Vector (length Nim) or scalar of per-image flux
    %                    zero-points / transparencies F_j. Default is 1.
    %            'Var' - Vector (length Nim) or scalar of per-image background
    %                    VARIANCES sigma_j^2. Default is 1.
    %            'Norm'- Normalize each input PSF stamp to unit sum before
    %                    coaddition. Default is true.
    %            'PsfCenter' - Where the PSF center sits in the input stamp:
    %                    'pixel' (default) | 'corner' | 'centroid' | [yc xc] |
    %                    [Nim x 2]. See convention 1. Controls alignment of R to
    %                    the input grid; 'pixel' matches properCoaddLinear.
    %            'AnnulusPre'  - [Rin Rout] radii (pix) of a cosine-bell taper
    %                    applied to each input PSF (radius measured from the PSF
    %                    center): weight 1 for r<=Rin, cosine falloff to 0 at
    %                    r=Rout, 0 beyond. Empty = skip. Default is [].
    %            'AnnulusPost' - [Rin Rout] cosine-bell taper radii applied to
    %                    the coadd PSF PR. Empty = skip (exact statistic).
    %                    Default is [] (NOTE: changed from the legacy [5 8] so
    %                    that the DEFAULT output is the exact ZO17 statistic).
    %            'ReCalcAfterAnnPost' - If AnnulusPost is given and this is true,
    %                    recompute R using the tapered PR as denominator, keeping
    %                    R and PR mutually consistent. Default is true.
    %            'Full2stamp'   - If true, crop the returned PR to the input
    %                    stamp size about its center. If false, return the full
    %                    [Ny,Nx] PR. Default is true.
    %            'ShiftToCenter'- If true, return PR in CENTERED layout (peak at
    %                    floor(N/2)+1), equivalent to properCoaddLinear's P_R.
    %                    If false, return PR in corner/FFT layout (peak at
    %                    (1,1)), the legacy combine_proper convention.
    %                    Applies to both the full-size and the cropped
    %                    ('Full2stamp') PSF; never affects R. Default is true.
    %            'Convert2real' - Take the real part of R and PR (imaginary
    %                    residuals are FFT round-off; a warning is issued if the
    %                    imaginary part is anomalously large, which indicates a
    %                    PSF centering/convention error). Default is true.
    % Output : - R    - Proper coadd image [Ny,Nx], aligned with the input grid.
    %          - PR   - Coadd PSF, unit-sum; centered if 'ShiftToCenter'=true
    %                   (default, matching properCoaddLinear), corner layout
    %                   otherwise (see conventions 3-4).
    %          - R_f  - fft2 of R (corner layout).
    %          - PR_f - fft2 of the corner-layout, full-size PR (unit DC).
    %          - F_R  - Effective coadd zero-point sqrt(sum(F.^2./Var)).
    % Reference: Zackay & Ofek 2017, ApJ, 836, 188 (How to COAAD Images. II)
    % Author : Claude + Eran Ofek (Jul 2026)
    % Example:
    %          Psf  = imUtil.kernel2.gauss([1 2 3 4 5]');
    %          Data = Psf + randn(size(Psf)).*0.001;
    %          [R,PR,R_f,PR_f,F_R] = imUtil.properCoadd.combine_proper(Data,Psf);

    arguments
        Data
        PSF
        Args.F                          = 1;
        Args.Var                        = 1;
        Args.Norm(1,1) logical          = true;
        Args.PsfCenter                  = 'pixel';
        Args.AnnulusPre                 = [];
        Args.AnnulusPost                = [];
        Args.ReCalcAfterAnnPost logical = true;
        Args.Full2stamp(1,1) logical    = true;
        Args.ShiftToCenter(1,1) logical = true;
        Args.Convert2real(1,1) logical  = true;
    end

    % ---------------------------------------------------------------- sizes
    [Ny, Nx, Nim] = size(Data);
    StampSize     = [size(PSF,1), size(PSF,2)];
    NimPsf        = size(PSF,3);

    if ~(NimPsf==Nim || NimPsf==1)
        error('combine_proper:psfCube', ...
            'PSF cube has %d planes; expected 1 (shared) or Nim=%d.', NimPsf, Nim);
    end
    if any(StampSize > [Ny Nx])
        error('combine_proper:psfSize', ...
            'PSF stamp [%d %d] is larger than the image [%d %d].', ...
            StampSize(1), StampSize(2), Ny, Nx);
    end

    % --------------------------------------------------- per-image F and Var
    F = Args.F(:);
    V = Args.Var(:);
    if isscalar(F), F = repmat(F, Nim, 1); end
    if isscalar(V), V = repmat(V, Nim, 1); end
    if numel(F)~=Nim || numel(V)~=Nim
        error('combine_proper:weights', ...
            'F and Var must be scalars or vectors of length Nim=%d.', Nim);
    end
    if any(V<=0)
        error('combine_proper:var', 'All variances must be positive.');
    end

    WW_n = reshape(F.^2./V, 1, 1, Nim);   % weights of |P_f|^2 (denominator)
    WW_d = reshape(F   ./V, 1, 1, Nim);   % weights of the matched filter
    F_R  = sqrt(sum(F.^2./V));            % effective coadd zero-point

    % ------------------------------------------------------- PSF preparation
    if Args.Norm
        PSF = PSF ./ sum(PSF, [1 2]);
    end

    % Where is each stamp's PSF center? Move that point to the FFT origin (1,1)
    % so that a source at pixel p in Data lands at pixel p in R (convention 1).
    % Integer part of the offset is applied by circshift (image domain); any
    % sub-pixel remainder is applied as a Fourier phase ramp after fft2.
    Ctr    = i_psfCenters(PSF, Args.PsfCenter, NimPsf);   % [NimPsf x 2], 1-based
    Off    = Ctr - 1;                                     % target offset -> origin
    Ioff   = round(Off);
    Frac   = Off - Ioff;                                  % sub-pixel remainder

    % integer-roll each plane so round(center) -> (1,1)  ["corner" layout]
    PSF = i_stamp2corner(PSF, [Ny Nx], Ioff);

    if ~isempty(Args.AnnulusPre)
        PSF = PSF .* i_cornerTaper([Ny Nx], Args.AnnulusPre);
    end

    % ------------------------------------------- proper coaddition (Fourier)
    PSF_f  = fft2(PSF);                   % [Ny,Nx,NimPsf] (NimPsf=1 or Nim)
    if any(Frac(:) ~= 0)                  % apply sub-pixel alignment, if any
        PSF_f = i_phaseShift(PSF_f, Frac);
    end
    Data_f = fft2(Data);

    % D_f = F_R * |PR_f|; real^2+imag^2 avoids abs()'s sqrt-then-square
    D_f = sqrt(sum(WW_n .* (real(PSF_f).^2 + imag(PSF_f).^2), 3));
    S_f = sum(WW_d .* conj(PSF_f) .* Data_f, 3);

    % guarded division: zero out dead frequencies (relative, class-aware
    % threshold; same protection as properCoaddLinear's solver)
    Thr = 1e3 * eps(max(D_f(:)));
    R_f = i_guardedDiv(S_f, D_f, Thr);

    PR_f = D_f ./ F_R;                    % unit DC -> unit-sum PR (convention 4)

    R  = ifft2(R_f);
    PR = ifft2(PR_f);                     % corner layout; real up to round-off

    % ----------------------------------------- optional taper of the coadd PSF
    if ~isempty(Args.AnnulusPost)
        PR = PR .* i_cornerTaper([Ny Nx], Args.AnnulusPost);

        if Args.ReCalcAfterAnnPost
            % recompute R with the tapered denominator so that R and PR stay
            % mutually consistent (fft2 here, NOT ifft2 - legacy bug)
            PR_f = fft2(PR);
            R_f  = i_guardedDiv(S_f, F_R .* PR_f, Thr);
            R    = ifft2(R_f);
        end
    end

    % ------------------------------------------------------------ real parts
    if Args.Convert2real
        MaxIm = max(abs(imag(R(:))));
        MaxRe = max(abs(real(R(:))));
        if MaxIm > 1e-6 * max(MaxRe, eps)
            warning('combine_proper:largeImag', ...
                ['Imaginary residual of R is %.3g of its real amplitude. ', ...
                 'This usually indicates the PSF is not centered on pixel ', ...
                 'floor(n/2)+1 (see convention 1), e.g. a half-pixel offset ', ...
                 'in an even-sized stamp.'], MaxIm/MaxRe);
        end
        R  = real(R);
        PR = real(PR);
    end

    % ----------------------------- output layout of PR (conventions 3 and 4)
    % PR is in corner/FFT layout here. The centered layout is produced by
    % fftshift; the stamp-size crop is defined about the center pixel and,
    % for corner output, shifted back by ifftshift of the STAMP (exact
    % inverse for odd and even sizes alike). R is aligned with the input
    % pixel grid by construction and is never shifted.
    PRc = fftshift(PR);                              % centered, full size
    if Args.Full2stamp
        PRc = i_cropCenter(PRc, StampSize);          % centered stamp
    end
    if Args.ShiftToCenter
        PR = PRc;                                    % properCoaddLinear layout
    else
        PR = ifftshift(PRc);                         % corner/FFT layout
    end
end

% =========================================================================
% internal subfunctions (no external dependencies)
% =========================================================================

function Full = i_stamp2corner(Stamp, OutSize, Ioff)
    % Zero-pad PSF stamp(s) to OutSize and circularly shift so that the pixel
    % given by Ioff+1 (per plane, 1-based center) lands at (1,1). Ioff is
    % [np x 2] integer offsets (row, col). Valid for odd and even stamps.
    [ny, nx, np] = size(Stamp);
    Full = zeros(OutSize(1), OutSize(2), np, 'like', Stamp);
    Full(1:ny, 1:nx, :) = Stamp;
    if size(Ioff,1)==1
        Full = circshift(Full, [-Ioff(1), -Ioff(2), 0]);
    else
        for Ip = 1:np
            Full(:,:,Ip) = circshift(Full(:,:,Ip), [-Ioff(Ip,1), -Ioff(Ip,2)]);
        end
    end
end

function Ctr = i_psfCenters(Stamp, Spec, NimPsf)
    % Return the [NimPsf x 2] center location (1-based, possibly fractional) of
    % each PSF plane according to Spec:
    %   'pixel'    -> central pixel floor(n/2)+1 (default; == properCoaddLinear)
    %   'corner'   -> (1,1) (stamp already stored with its center at pixel 1)
    %   'centroid' -> per-plane flux-weighted first moment (handles arbitrary
    %                 mis-centering, including sub-pixel)
    %   [yc xc]    -> explicit center applied to all planes (may be fractional)
    %   [NimPsf x 2] numeric -> explicit per-plane centers
    [ny, nx, ~] = size(Stamp);
    if isnumeric(Spec)
        if isequal(size(Spec),[1 2])
            Ctr = repmat(Spec(:).', NimPsf, 1);
        elseif isequal(size(Spec),[NimPsf 2])
            Ctr = Spec;
        else
            error('combine_proper:psfCenterNum', ...
                'Numeric PsfCenter must be [1 2] or [NimPsf 2].');
        end
        return;
    end
    switch lower(Spec)
        case 'pixel'
            Ctr = repmat([floor(ny/2)+1, floor(nx/2)+1], NimPsf, 1);
        case 'corner'
            Ctr = ones(NimPsf, 2);
        case 'centroid'
            Ctr = zeros(NimPsf, 2);
            [YY, XX] = ndgrid(1:ny, 1:nx);
            for Ip = 1:NimPsf
                P  = Stamp(:,:,Ip);
                Sp = sum(P(:));
                if Sp==0
                    error('combine_proper:psfCentroid', ...
                        'PSF plane %d has zero sum; cannot take centroid.', Ip);
                end
                Ctr(Ip,:) = [sum(YY(:).*P(:)), sum(XX(:).*P(:))] ./ Sp;
            end
        otherwise
            error('combine_proper:psfCenter', ...
                'PsfCenter must be ''pixel'', ''corner'', ''centroid'', or numeric.');
    end
end

function OTF = i_phaseShift(OTF, Frac)
    % Apply a sub-pixel shift of -Frac(Ip,:) pixels to each plane of OTF via a
    % Fourier phase ramp, moving the PSF's fractional center onto the origin.
    % (Integer part is already handled by circshift.) The DFT shift operator
    % exp(-2i*pi*k*delta/N) is exact and periodic, so k=0:N-1 is used directly.
    [Ny, Nx, np] = size(OTF);
    ky = (0:Ny-1).';
    kx = (0:Nx-1);
    for Ip = 1:np
        if any(Frac(min(Ip,size(Frac,1)),:) ~= 0)
            fy = Frac(min(Ip,size(Frac,1)),1);
            fx = Frac(min(Ip,size(Frac,1)),2);
            Ramp = exp(-2i*pi*(ky.*(fy/Ny) + kx.*(fx/Nx)));
            OTF(:,:,Ip) = OTF(:,:,Ip) .* Ramp;
        end
    end
end

function W = i_cornerTaper(SizeIm, Radii)
    % Radial cosine-bell (Tukey) taper for a CORNER-layout kernel:
    %   weight = 1                                   for r <= Rin
    %   weight = 0.5*(1+cos(pi*(r-Rin)/(Rout-Rin)))  for Rin < r < Rout
    %   weight = 0                                   for r >= Rout
    % where r is the toroidal (wrap-around) distance from pixel (1,1), so the
    % taper is correctly centered on the kernel in corner layout.
    Rin  = Radii(1);
    Rout = Radii(2);
    if ~(isfinite(Rin) && isfinite(Rout) && Rout > Rin && Rin >= 0)
        error('combine_proper:taper', ...
            'Taper radii must satisfy 0 <= Rin < Rout; got [%g %g].', Rin, Rout);
    end
    if Rout > min(SizeIm)/2
        warning('combine_proper:taperLarge', ...
            'Outer taper radius %g exceeds half the smallest image axis (%g).', ...
            Rout, min(SizeIm)/2);
    end
    vy = (0:SizeIm(1)-1).';
    vx = (0:SizeIm(2)-1);
    dy = min(vy, SizeIm(1)-vy);          % toroidal axis distance from row 1
    dx = min(vx, SizeIm(2)-vx);          % toroidal axis distance from col 1
    Rmat = sqrt(dy.^2 + dx.^2);

    W = zeros(SizeIm);
    W(Rmat <= Rin) = 1;
    Band = Rmat > Rin & Rmat < Rout;
    W(Band) = 0.5*(1 + cos(pi*(Rmat(Band)-Rin)./(Rout-Rin)));
end

function Q = i_guardedDiv(Num, Den, Thr)
    % Elementwise Num./Den with dead frequencies (|Den|<=Thr) set to zero,
    % instead of amplifying rounding noise / producing Inf-NaN.
    Good = abs(Den) > Thr;
    Q = complex(zeros(size(Num), 'like', real(Num)));
    Q(Good) = Num(Good) ./ Den(Good);
end

function Out = i_cropCenter(Img, Sz)
    % Crop a CENTERED image about its center pixel ( floor(N/2)+1 ), returning
    % a [Sz(1),Sz(2)] stamp whose own center pixel ( floor(p/2)+1 ) coincides
    % with the image center pixel. Exact for odd and even sizes; inverse of
    % the i_stamp2corner placement.
    [Ny, Nx] = size(Img);
    py = Sz(1);  px = Sz(2);
    if py > Ny || px > Nx
        error('combine_proper:crop', ...
            'Requested stamp [%d %d] exceeds image [%d %d].', py, px, Ny, Nx);
    end
    cy = floor(Ny/2) + 1;
    cx = floor(Nx/2) + 1;
    Out = Img(cy + (-floor(py/2) : ceil(py/2)-1), ...
              cx + (-floor(px/2) : ceil(px/2)-1));
end