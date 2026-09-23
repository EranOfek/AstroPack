function [Xsize, Ysize, IsTruncated] = pseudoFWHM (PSFin, Args)
    % Measure pseudo FWHM width in a PSF stamp at a given flux level
    % Package: imUtil.psf
    % Description: Measure pseudo FWHM width in a PSF stamp at a given flux level.
    %              The width along each axis is the full extent of the region at or above
    %              Level * max(PSF) -- the region's bounding box, not a cut through the peak,
    %              which is what makes it "pseudo" and robust for asymmetric PSFs. It is
    %              measured on the max-projection of the stamp onto that axis (a column holds
    %              a pixel above the level exactly when its maximum does, so the bounding box
    %              is the same), with the two outermost level crossings interpolated linearly
    %              between the last sample above the level and the first one below it.
    %              NB: until Sep 2026 the width was the distance between the first and last
    %              pixel CENTRES above the level -- about one pixel short, and quantised to
    %              whole pixels, so it jumped by a pixel with the source's sub-pixel position
    %              (issue #1310).
    % Input:  - PSFin: a 2D array containing the PSF stamp
    %         * ...,key,val,...
    %         'Level' - signal level relative to the maximum, in (0, 1]; 0.5 is default
    %
    % Output : - Xsize: pseudo FWHM width along X, i.e. along the columns (dimension 2) [pix]
    %          - Ysize: pseudo FWHM width along Y, i.e. along the rows (dimension 1) [pix]
    %            NB: until Sep 2026 the two outputs were the other way round (issue #1310).
    %          - IsTruncated: 1x2 logical [X Y], true where the region at or above the level
    %            reaches the edge of the stamp; that width is then only a lower limit
    %            (measured to the stamp edge)
    %
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (Feb 2023)
    % Example: [FWHM_X, FWHM_Y] = imUtil.psf.pseudoFWHM (PSF, 'Level', 0.8)
    %          [FWHM_X, FWHM_Y, IsTruncated] = imUtil.psf.pseudoFWHM (PSF);

    arguments

        PSFin                    % the input PSF stamp

        Args.Level (1,1) {mustBePositive, mustBeLessThanOrEqual(Args.Level,1)} = 0.5; % at half maximum

    end

    Thresh = max(PSFin, [], 'all') .* Args.Level;

    [Xsize, TruncX] = crossingWidth(max(PSFin, [], 1), Thresh);   % along the columns (dim 2)
    [Ysize, TruncY] = crossingWidth(max(PSFin, [], 2), Thresh);   % along the rows    (dim 1)

    IsTruncated = [TruncX, TruncY];
end

function [Width, IsTruncated] = crossingWidth (Prof, Thresh)
    % Distance between the outermost Thresh crossings of a 1D profile, each crossing
    % interpolated linearly between the last sample at or above Thresh and the first one
    % below it. Where the region reaches an end of the profile, the crossing is put at the
    % outer edge of the end pixel, and the width is a lower limit.
    Prof  = Prof(:);
    Nprof = numel(Prof);
    Above = find(Prof >= Thresh);
    I1    = Above(1);
    I2    = Above(end);

    if I1 > 1
        Left = I1 - (Prof(I1) - Thresh) ./ (Prof(I1) - Prof(I1-1));
    else
        Left = 0.5;
    end
    if I2 < Nprof
        Right = I2 + (Prof(I2) - Thresh) ./ (Prof(I2) - Prof(I2+1));
    else
        Right = Nprof + 0.5;
    end

    Width       = Right - Left;
    IsTruncated = (I1 == 1) || (I2 == Nprof);
end
