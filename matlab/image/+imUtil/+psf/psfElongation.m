function [PA, AxisRatio] = psfElongation(PSF)
    % Position angle and axis ratio of a 2D PSF/image from its 2nd moments.
    % Description: Flux-weighted (non-negative pixels only) 2nd-moment
    %              measurement of the input stamp's shape. Used by
    %              imUtil.psf.wingsFix to detect a PSF with substantial
    %              ellipticity (e.g. from wind, or a tracking/guiding
    %              error) and fall back to a plain 'cosbell' taper instead
    %              of an 'analytic' or 'empirical' wing model, neither of
    %              which is a good match for a non-circular PSF.
    % Input  : - PSF. A 2D real matrix.
    % Output : - PA. Position angle [rad] of the major axis, in image (X,Y)
    %            convention (0 along +X, counterclockwise toward +Y).
    %          - AxisRatio. Minor/major axis ratio, in (0,1]. 1 for a
    %            circularly-symmetric (or degenerate/invalid) input.
    % Author : AI-assisted (2026 Aug)
    % Example: [PA, AxisRatio] = imUtil.psf.psfElongation(MeanPSF);

    arguments
        PSF
    end

    [Ny, Nx] = size(PSF);
    [X, Y]   = meshgrid(1:Nx, 1:Ny);
    W        = max(PSF, 0);
    Tot      = sum(W(:), 'omitnan');

    PA        = 0;
    AxisRatio = 1;
    if ~(isfinite(Tot) && Tot > 0)
        return;
    end

    Xc  = sum(W(:).*X(:), 'omitnan') / Tot;
    Yc  = sum(W(:).*Y(:), 'omitnan') / Tot;
    Mxx = sum(W(:).*(X(:)-Xc).^2, 'omitnan') / Tot;
    Myy = sum(W(:).*(Y(:)-Yc).^2, 'omitnan') / Tot;
    Mxy = sum(W(:).*(X(:)-Xc).*(Y(:)-Yc), 'omitnan') / Tot;

    HalfDiff = (Mxx - Myy)/2;
    HalfSum  = (Mxx + Myy)/2;
    Rad      = sqrt(HalfDiff.^2 + Mxy.^2);
    Lambda1  = HalfSum + Rad; % major-axis variance
    Lambda2  = HalfSum - Rad; % minor-axis variance

    if ~(isfinite(Lambda1) && Lambda1 > 0)
        return;
    end

    PA        = 0.5.*atan2(2.*Mxy, Mxx-Myy);
    AxisRatio = sqrt(max(Lambda2, 0)./Lambda1);
end
