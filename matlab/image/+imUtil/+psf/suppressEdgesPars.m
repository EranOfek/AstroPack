function FunPars = suppressEdgesPars(FunPars, SizeXY)
    % Convert a scalar taper width into [inner, outer] radii for a stamp size.
    %   Shared by imUtil.psf.suppressEdges and AstroPSF/suppressEdges.
    % Input  : - A scalar taper width W (pixels from the stamp outer radius),
    %            or a two-element [inner, outer] vector returned as is.
    %          - Stamp size [X, Y].
    % Output : - [inner, outer] = [R-W, R], where R is the largest on-axis
    %            radius present in the stamp about the imUtil.kernel2.*
    %            center ceil(SizeXY/2): R = min(SizeXY - ceil(SizeXY/2)).
    % Author : Eran Ofek + Claude (Sep 2026)
    % Example: imUtil.psf.suppressEdgesPars(2, [25 25])   % [10 12]
    %          imUtil.psf.suppressEdgesPars(2, [24 24])   % [10 12]

    arguments
        FunPars
        SizeXY (1,2) {mustBeInteger, mustBePositive}
    end

    if isscalar(FunPars)
        OuterRadius = min(SizeXY - ceil(SizeXY./2));
        FunPars     = [OuterRadius - FunPars, OuterRadius];
    end
end
