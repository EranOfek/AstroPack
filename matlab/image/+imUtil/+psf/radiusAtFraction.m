function ThreshRadius = radiusAtFraction(PSF, Threshold)
    % Radius of PSF at which its relative height equal to some threshold
    % Input  : - PSF stamp
    %          - Threshold of height of PSF compare to max.
    % Output : -  Radius at which the relative height of the PSF equal to
    %             the threshold.
    % Author : Eran Ofek (2026 Jun) 
    % Example: ThreshRadius = imUtil.psf.radiusAtFraction(PSF, 1e-3)

    Size = size(PSF);
    HalfSize = (min(Size) - 1).*0.5; % assume odd-size PSF!
    [Radius, Mean] = imUtil.psf.mex.radialProfile_mex(PSF, HalfSize+1, HalfSize+1, HalfSize);
    ThreshRadius = floor(tools.interp.interp1crossVal(Radius, Mean./max(Mean), Threshold, false));

end
