function NewPSF = addEmpiricalWings2PSF(PSF, ProfileRadius, ProfileValue, Args)
    % Splice a measured (empirical) radial wing profile into a PSF.
    % Description: Given a measured (possibly asymmetric) PSF, preserve the
    %              core for radius r<=R1 and replace the outer wings with a
    %              circularly-symmetric profile built from real data
    %              (ProfileRadius, ProfileValue - already on the same
    %              absolute flux scale as PSF, e.g. via seam-matching in
    %              imUtil.psf.buildEmpiricalWing). The core and the
    %              empirical wing are blended over the transition annulus
    %              [R1,R2] using a C2 smootherstep window, as in
    %              imUtil.psf.addWings2PSF. Unlike addWings2PSF, the result
    %              is also tapered smoothly to zero by the stamp edge, so
    %              there is no discontinuity at the array boundary.
    % Input  : - PSF matrix (2D, real). The core may be asymmetric and is
    %            kept unchanged for r<=R1.
    %          - ProfileRadius. Vector of radii [pix] at which the
    %            empirical wing was measured.
    %          - ProfileValue. Measured values at ProfileRadius, on the
    %            same absolute (unit-flux) scale as PSF. Must be positive
    %            and, ideally, non-increasing with radius (the caller,
    %            imUtil.psf.buildEmpiricalWing, already enforces this).
    %          * ...,key,val,...
    %            'R1' - Inner radius of the transition annulus [pix]. For
    %                   r<=R1 the measured PSF is used unchanged.
    %                   Default is 0.25.*Rgrid, where Rgrid=(min(size(PSF))-1)./2.
    %            'R2' - Outer radius of the transition annulus [pix]. For
    %                   r>=R2 the empirical wing is used. Must be > R1.
    %                   Default is 0.40.*Rgrid.
    %            'Norm' - A logical flag indicating if to normalize the
    %                   output PSF such that sum(NewPSF(:))==1.
    %                   Default is true.
    %            'Rmax' - Radius [pix] beyond which the PSF is forced to
    %                   zero. Empty -> the stamp half-size (the array
    %                   edge). Default is [].
    %            'TaperWidth' - Width [pix] of the smooth (C2 smootherstep)
    %                   taper-to-zero window ending at Rmax.
    %                   Default is 3.
    %            'Cx' - The X (column) coordinate of the PSF center [pix,
    %                   1-based]. If NaN, use the flux centroid over the
    %                   positive pixels. Default is NaN.
    %            'Cy' - The Y (row) coordinate of the PSF center [pix,
    %                   1-based]. Default is NaN.
    % Output : - NewPSF. The PSF with the empirical wing extension (double).
    % Author : AI-assisted (2026 Jul)
    % Example: NewPSF = imUtil.psf.addEmpiricalWings2PSF(PSF, R, V, 'R1',4, 'R2',7);

    arguments
        PSF
        ProfileRadius
        ProfileValue
        Args.R1          = 0.25*(min(size(PSF)) - 1)
        Args.R2          = 0.40*(min(size(PSF)) - 1)
        Args.Norm        = true
        Args.Rmax        = []
        Args.TaperWidth  = 3
        Args.Cx          = NaN
        Args.Cy          = NaN
        Args.PA          = 0     % major-axis position angle [rad] for elliptical wings
        Args.AxisRatio   = 1     % minor/major axis ratio; 1 = circular (legacy)
    end

    assert(Args.R2 > Args.R1, 'addEmpiricalWings2PSF:radii', 'Require R2 > R1.');

    [nr, nc]  = size(PSF);
    HalfSize  = (min(nr, nc) - 1)/2;
    if isempty(Args.Rmax)
        Rmax = HalfSize;
    else
        Rmax = Args.Rmax;
    end

    [X, Y] = meshgrid(1:nc, 1:nr);

    % --- center: flux centroid over positive pixels (if not supplied) ---
    Cx = Args.Cx;
    Cy = Args.Cy;
    if isnan(Cx) || isnan(Cy)
        wpos = max(PSF, 0);
        s    = sum(wpos(:));
        if s > 0
            if isnan(Cx), Cx = sum(sum(wpos.*X)) / s; end
            if isnan(Cy), Cy = sum(sum(wpos.*Y)) / s; end
        else
            if isnan(Cx), Cx = 0.5*(nc + 1); end
            if isnan(Cy), Cy = 0.5*(nr + 1); end
        end
    end

    % Elliptical radius (see addWings2PSF); taper/Rmax stay circular.
    Xr = (X - Cx).*cos(Args.PA) + (Y - Cy).*sin(Args.PA);
    Yr = -(X - Cx).*sin(Args.PA) + (Y - Cy).*cos(Args.PA);
    R  = hypot(Xr, Yr ./ max(Args.AxisRatio, 0.1));
    Rcirc = hypot(X - Cx, Y - Cy);

    % --- clean/sort the measured profile ---
    Valid = isfinite(ProfileRadius(:)) & isfinite(ProfileValue(:)) & ProfileValue(:) > 0;
    PR = ProfileRadius(Valid);
    PV = ProfileValue(Valid);
    [PR, SortIdx] = sort(PR(:));
    PV = PV(SortIdx);
    assert(numel(PR) >= 2, 'addEmpiricalWings2PSF:profile', 'Need at least 2 valid profile points.');

    % --- evaluate the empirical wing at every pixel: log-linear
    %     interpolation across the measured range, held flat (nearest)
    %     beyond it -- no assumed functional form is extrapolated ---
    Rclip = min(max(R, PR(1)), PR(end));
    LogW  = interp1(PR, log(PV), Rclip(:), 'linear');
    W     = reshape(exp(LogW), nr, nc);

    % --- core/wing blend over [R1,R2], C2 smootherstep (as in addWings2PSF) ---
    t = min(max((R - Args.R1)./(Args.R2 - Args.R1), 0), 1);
    S = t.^3 .* (t.*(t.*6 - 15) + 10);      % 6t^5 - 15t^4 + 10t^3
    w = 1 - S;                              % data weight: 1 at R1 -> 0 at R2

    NewPSF          = w.*PSF + (1 - w).*W;
    NewPSF(R <= Args.R1) = PSF(R <= Args.R1);
    NewPSF(R >= Args.R2) = W(R >= Args.R2);

    % --- smooth taper to zero by the stamp edge ---
    Rtaper0 = max(Rmax - Args.TaperWidth, Args.R2);
    tt      = min(max((Rcirc - Rtaper0)./(Rmax - Rtaper0), 0), 1);
    Edge    = 1 - (tt.^3 .* (tt.*(tt.*6 - 15) + 10));
    NewPSF  = NewPSF .* Edge;
    NewPSF(Rcirc > Rmax) = 0;

    if Args.Norm
        s = sum(NewPSF(:));
        if s ~= 0, NewPSF = NewPSF ./ s; end
    end
end
