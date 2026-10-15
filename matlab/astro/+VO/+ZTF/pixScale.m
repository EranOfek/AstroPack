function [Scale, Naxis] = pixScale()
    % ZTF plate scale and readout-quadrant size.
    % Description: The ZTF camera is read out in 64 quadrants (16 CCDs x 4),
    %              and every per-epoch product (sciimg, psfcat, sexcat) covers
    %              one quadrant. These two numbers convert the catalogs'
    %              pixel columns to angles and fill the CCDSEC keyword that
    %              PhotCalibTrans uses to normalize its Tran2D surface.
    % Input  : null
    % Output : - Scale - plate scale [arcsec/pixel].
    %          - Naxis - [NAXIS1, NAXIS2] of one readout quadrant [pixel].
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Scale, Naxis] = VO.ZTF.pixScale();

    Scale = 1.0126;
    Naxis = [3072, 3080];
end
