function [Color, Valid] = usableColor(Cat, Args)
    % Colour of a source, accepted only when its Gaia match is close enough.
    %
    %   imProc.cat.addColor reports the colour of the NEAREST Gaia source within
    %   its (5") match radius together with the distance to it, so a source whose
    %   real counterpart is absent from Gaia can still carry a neighbour's colour.
    %   Every consumer that treats the colour as belonging to the source must
    %   therefore apply a distance cut; this is that cut, in one place.
    %
    % Input  : - An AstroCatalog (or anything with ColNames/getCol).
    %          * ...,key,val,...
    %            'ColorCol'    - Colour column. Default 'BP_RP_NEAR'.
    %            'DistCol'     - Distance column [arcsec]. Default 'GAIA_DIST'.
    %            'MaxDist'     - Largest accepted separation [arcsec]. Default 1.
    %            'LegacyColorCol' - Column used by catalogs produced before the
    %                            rename, which already hold a strictly matched
    %                            colour and so need no cut. Default 'BP_RP'.
    % Output : - Color : column vector, NaN where the colour may not be used.
    %          - Valid : logical, true where it may.
    % Author : Dana Kovaleva (Sep 2026)
    % Example: [C, V] = imProc.cat.usableColor(AI.CatData, 'MaxDist', 1);

    arguments
        Cat
        Args.ColorCol char        = 'BP_RP_NEAR'
        Args.DistCol char         = 'GAIA_DIST'
        Args.MaxDist (1,1) double = 1
        Args.LegacyColorCol char  = 'BP_RP'
    end

    CN = Cat.ColNames;
    if any(strcmp(CN, Args.ColorCol))
        Color = double(Cat.getCol(Args.ColorCol));
        Color = Color(:);
        if any(strcmp(CN, Args.DistCol))
            Dist  = double(Cat.getCol(Args.DistCol));
            Valid = isfinite(Color) & isfinite(Dist(:)) & Dist(:) <= Args.MaxDist;
        else
            % Without the distance the colour cannot be shown to belong to the
            % source, and a neighbour's colour would quietly bias the
            % calibration. Fail safe rather than guess.
            warning('imProc:cat:usableColor:NoDist', ...
                ['Column ''%s'' is present but ''%s'' is not, so the match distance ', ...
                 'is unknown - the colour is treated as unusable.'], Args.ColorCol, Args.DistCol);
            Valid = false(size(Color));
        end
    elseif any(strcmp(CN, Args.LegacyColorCol))
        % Pre-rename catalogs: that column was already a strict match.
        Color = double(Cat.getCol(Args.LegacyColorCol));
        Color = Color(:);
        Valid = isfinite(Color);
    else
        Nrow  = size(Cat.Catalog, 1);
        Color = nan(Nrow, 1);
        Valid = false(Nrow, 1);
    end
    Color(~Valid) = NaN;
end
