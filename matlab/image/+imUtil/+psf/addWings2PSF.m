function [NewPSF, Amp] = addWings2PSF(PSF, Alpha, R1, R2, Norm, Rmax, Cx, Cy, Amp)
    % Replace the noisy outer wings of a measured PSF with an analytic power-law.
    % Description: Given a measured (possibly asymmetric) PSF, preserve the core
    %              for radius r<=R1 and replace the noisy outer wings with a
    %              circularly-symmetric power-law W(r)=Amp.*r.^(-Alpha) for r>=R2.
    %              The core and the analytic wing are blended over the transition
    %              annulus [R1,R2] using a C2 smootherstep window, so that the
    %              value, first and second radial derivatives are continuous at
    %              both seams (no Fourier-domain ringing in forward modeling).
    % Input  : - PSF matrix (2D, real). The core may be asymmetric and is kept
    %            unchanged for r<=R1.
    %          - Alpha. The wing power-law index, W=Amp.*r.^(-Alpha), Alpha>0.
    %            Default is 2.5.
    %          - R1. Inner radius of the transition annulus [pix]. For r<=R1 the
    %            measured PSF is used unchanged.
    %            Default is 0.50.*Rgrid, where Rgrid=(min(size(PSF))-1)./2.
    %          - R2. Outer radius of the transition annulus [pix]. For r>=R2 the
    %            analytic wing is used. Must be larger than R1.
    %            Default is 0.80.*Rgrid.
    %          - Norm. A logical flag indicating if to normalize the output PSF
    %            such that sum(NewPSF(:))==1.
    %            Default is true.
    %          - Rmax. Truncation radius [pix]. Pixels with r>Rmax are set to 0.
    %            Default is Inf.
    %          - Cx. The X (column) coordinate of the PSF center [pix, 1-based].
    %            If NaN, then use the flux centroid over the positive pixels.
    %            Default is NaN.
    %          - Cy. The Y (row) coordinate of the PSF center [pix, 1-based].
    %            If NaN, then use the flux centroid over the positive pixels.
    %            Default is NaN.
    %          - Amp. The wing amplitude. If NaN, then robustly estimate it as
    %            the median over the annulus of (PSF(r).*r.^Alpha).
    %            Default is NaN.
    % Output : - NewPSF. The PSF with the analytic wing extension (double).
    %          - Amp. The wing amplitude actually used.
    % Author : <your name> (2026 Jun)
    % Example: NewPSF = addWings2PSF(PSF, 2.5, 25, 40, true, 95);
    %          [NewPSF,Amp] = addWings2PSF(PSF);

    arguments
        PSF   
        Alpha    = 2.5
        R1       = 0.25*(min(size(PSF)) - 1)
        R2       = 0.40*(min(size(PSF)) - 1)
        Norm     = true
        Rmax     = Inf
        Cx       = NaN   % NaN -> flux centroid
        Cy       = NaN
        Amp      = NaN   % NaN -> robust auto-fit
    end

    assert(R2 > R1, 'addWings2PSF:radii', 'Require R2 > R1.');

    [nr, nc] = size(PSF);
    [X, Y]   = meshgrid(1:nc, 1:nr);

    % --- center: flux centroid over positive pixels (if not supplied) ---
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

    R = hypot(X - Cx, Y - Cy);

    % --- robust amplitude fit on the annulus (if not supplied) ---
    if isnan(Amp)
        ann = R >= R1 & R <= R2 & R > 0 & isfinite(PSF);
        if ~any(ann(:))
            warning('addWings2PSF:emptyAnnulus', ...
                    'No pixels in [R1,R2]; wing amplitude set to 0.');
            Amp = 0;
        else
            Amp = median(PSF(ann) .* R(ann).^Alpha);
        end
    end

    % --- analytic wing + C2 smootherstep blend ---
    W = Amp .* R.^(-Alpha);                 % only used where r>R1
    t = min(max((R - R1)./(R2 - R1), 0), 1);
    S = t.^3 .* (t.*(t.*6 - 15) + 10);      % 6t^5 - 15t^4 + 10t^3
    w = 1 - S;                              % data weight: 1 at R1 -> 0 at R2

    NewPSF              = w.*PSF + (1 - w).*W;   % annulus blend
    NewPSF(R <= R1)     = PSF(R <= R1);          % exact data core (avoids 0*Inf)
    NewPSF(R >= R2)     = W(R >= R2);            % pure analytic wing
    NewPSF(R >  Rmax)   = 0;                     % truncate

    if Norm
        s = sum(NewPSF(:));
        if s ~= 0, NewPSF = NewPSF ./ s; end
    end
end