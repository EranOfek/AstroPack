function [ProfileRadius, ProfileValue, Nstars, Success] = buildEmpiricalWing(CubeW, MaskCubeW, CorePSF, R1, Args)
    % Build a measured PSF wing profile from bright/near-saturated stars.
    % Description: Each bright star's saturated pixels are masked, then its
    %              flux scale is matched to CorePSF using the (unsaturated)
    %              pixel values in an annulus around R1, so no external
    %              photometric calibration is needed. The seam-matched,
    %              masked cutouts are median-stacked and azimuthally
    %              averaged into a monotonic, non-negative radial profile,
    %              suitable for splicing via imUtil.psf.addEmpiricalWings2PSF.
    % Input  : - CubeW. [ny x nx x Nstars] cube of background-subtracted,
    %            recentered bright-star cutouts, same stamp size as CorePSF.
    %          - MaskCubeW. Same-size logical/numeric cube, true/nonzero
    %            where a pixel is saturated (or otherwise unusable). Pass
    %            [] to skip masking.
    %          - CorePSF. The core (unit-flux) master PSF stamp to seam-
    %            match onto; same stamp size as CubeW's individual images.
    %          - R1. Seam radius [pix] (integer) at which the per-star
    %            scale factor is estimated and the returned profile starts.
    %          * ...,key,val,...
    %            'MinWingStars' - Minimum number of stars (after masking
    %                   and scale-factor sanity checks) required to trust
    %                   the result. Default is 8.
    % Output : - ProfileRadius. Integer-pixel radii from R1 to the stamp
    %            edge with a valid measurement (empty if Success is false).
    %          - ProfileValue. Median, monotonic-non-increasing profile
    %            value at each ProfileRadius (empty if Success is false).
    %          - Nstars. Number of stars that contributed a valid scale
    %            factor (independent of Success).
    %          - Success. True if Nstars >= MinWingStars and at least two
    %            valid radial bins were measured.
    % Author : AI-assisted (2026 Jul)
    % Example: [R,V,N,Ok] = imUtil.psf.buildEmpiricalWing(CubeW, MaskCubeW, CorePSF, 4);

    arguments
        CubeW
        MaskCubeW
        CorePSF
        R1
        Args.MinWingStars   = 8
    end

    ProfileRadius = [];
    ProfileValue  = [];
    Nstars        = 0;
    Success       = false;

    if isempty(CubeW)
        return;
    end

    [ny, nx, NstarsIn] = size(CubeW);
    Cx = (nx+1)/2;
    Cy = (ny+1)/2;
    [Xg, Yg] = meshgrid(1:nx, 1:ny);
    Rg = hypot(Xg - Cx, Yg - Cy);
    RBin = round(Rg);
    SeamSel = RBin == round(R1);

    if ~any(SeamSel(:))
        return;
    end

    CoreValAtR1 = median(CorePSF(SeamSel), 'omitnan');
    if ~(isfinite(CoreValAtR1) && CoreValAtR1 > 0)
        return;
    end

    % --- mask saturated pixels ---
    if ~isempty(MaskCubeW)
        CubeW(logical(MaskCubeW)) = NaN;
    end

    % --- per-star seam-match scale factor onto CorePSF's absolute scale ---
    ScaledCube = nan(ny, nx, NstarsIn);
    for Istar = 1:NstarsIn
        Stamp = CubeW(:,:,Istar);
        StarValAtR1 = median(Stamp(SeamSel), 'omitnan');
        if isfinite(StarValAtR1) && StarValAtR1 > 0
            ScaleFactor = CoreValAtR1 / StarValAtR1;
            if isfinite(ScaleFactor) && ScaleFactor > 0
                ScaledCube(:,:,Istar) = Stamp .* ScaleFactor;
                Nstars = Nstars + 1;
            end
        end
    end

    if Nstars < Args.MinWingStars
        return;
    end

    WingStack2D = median(ScaledCube, 3, 'omitnan');

    % --- azimuthal average, explicit NaN-aware binning ---
    MaxRBin = floor(min(nx, ny)/2);
    RadiusVec = (round(R1):MaxRBin)';
    ValueVec  = nan(size(RadiusVec));
    for k = 1:numel(RadiusVec)
        Sel = RBin == RadiusVec(k) & isfinite(WingStack2D);
        if any(Sel(:))
            ValueVec(k) = median(WingStack2D(Sel), 'omitnan');
        end
    end

    % --- keep only measured, positive bins; enforce a monotonic,
    %     non-increasing profile so the spliced result is guaranteed
    %     smooth/non-negative for cross-correlation use (the property the
    %     original analytic wing model was introduced to provide) ---
    ValidBin  = isfinite(ValueVec) & ValueVec > 0;
    RadiusVec = RadiusVec(ValidBin);
    ValueVec  = ValueVec(ValidBin);

    if numel(ValueVec) < 2
        return;
    end

    ProfileRadius = RadiusVec;
    ProfileValue  = cummin(ValueVec);
    Success       = true;
end
