function [ThreshRadius, PeakRadius] = radiusAtFraction(PSF, Threshold)
    % Radius of PSF at which its relative height equal to some threshold
    %   The radial profile is measured from the stamp centre; the threshold
    %   crossing is searched OUTWARD FROM THE PROFILE MAXIMUM, so the
    %   function is safe for non-model PSFs whose radial profile peaks off
    %   centre (e.g. an out-of-focus ring; issue #1268 - the previous
    %   version assumed a centrally-peaked monotonic profile, and on a ring
    %   returned 0, which made the wing splice overwrite the entire PSF
    %   with the analytic model).
    %   If the profile never falls below the threshold within the stamp,
    %   the stamp half-size is returned ("no outskirts inside the stamp"),
    %   so the caller's wing splice degenerates to a no-op instead of
    %   failing.
    % Input  : - PSF stamp
    %          - Threshold of height of PSF compare to max.
    % Output : - Radius at which the relative height of the PSF equal to
    %            the threshold (searched outward from the profile maximum).
    %          - Radius at which the radially-averaged profile peaks:
    %            0 for a centrally-peaked PSF (maximum in the innermost
    %            radial bin), the peak-bin radius otherwise. This is the
    %            value written to the PSF_RPK header keyword (issue #1268).
    % Author : Eran Ofek (2026 Jun) 
    % Example: ThreshRadius = imUtil.psf.radiusAtFraction(PSF, 1e-3)

    Size = size(PSF);
    HalfSize = (min(Size) - 1).*0.5; % assume odd-size PSF!
    [Radius, Mean] = imUtil.psf.mex.radialProfile_mex(PSF, HalfSize+1, HalfSize+1, HalfSize);

    [~, Imax] = max(Mean);
    if Imax==1
        PeakRadius = 0;
    else
        PeakRadius = Radius(Imax);
    end

    Prof = Mean(Imax:end)./Mean(Imax);
    if Prof(end) > Threshold
        % the profile never drops below the threshold inside the stamp
        ThreshRadius = HalfSize;
    else
        ThreshRadius = floor(tools.interp.interp1crossVal(Radius(Imax:end), Prof, Threshold, false));
    end

end
