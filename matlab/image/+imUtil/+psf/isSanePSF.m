function Flag = isSanePSF(P)
    % Check that a PSF stamp is sane: finite, with a positive maximum near the center.
    %   An inverted PSF (e.g. built from stamps with a negative sum, #1355)
    %   has its maximum in the noise floor, away from the center.
    %   Moved from the local function of pipeline.last.pipes.pipelineII.
    % Input  : - A PSF stamp (matrix).
    % Output : - A logical: true if the stamp is not empty, all its values
    %            are finite, and its maximum is positive and within 1 pix of
    %            the stamp center.
    % Author : Ruslan Konno (Oct 2026)
    % Example: imUtil.psf.isSanePSF(imUtil.kernel2.gauss(2))

    Flag = false;
    if isempty(P) || any(~isfinite(P(:)))
        return
    end
    [MaxVal, Imax] = max(P(:));
    [Iy, Ix] = ind2sub(size(P), Imax);
    Ctr = (size(P) + 1)./2;
    Flag = MaxVal > 0 && abs(Iy - Ctr(1)) <= 1 && abs(Ix - Ctr(2)) <= 1;
end
