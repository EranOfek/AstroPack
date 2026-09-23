function [Color, Valid] = usableColor(Cat, Args)
    % Colour of a source, accepted only when the Gaia match really is its own.
    %
    %   imProc.cat.addColor produces, by default, a strict 1" match in 'BP_RP':
    %   that colour belongs to the source and is used as it stands. Run with a
    %   wider radius (issue #1306) it instead reports the colour of the NEAREST
    %   Gaia source, which need not be the counterpart, under a name such as
    %   BP_RP_NEAR and beside a GAIA_DIST column. This function accepts either
    %   shape and applies the distance cut where one is needed, so consumers do
    %   not each have to know which shape they were handed.
    %
    % Input  : - An AstroCatalog (anything with ColNames/getCol).
    %          * ...,key,val,...
    %            'ColorCol'     - Strictly matched colour column, used as-is when
    %                             present. Default 'BP_RP'.
    %            'NearColorCol' - Nearest-source colour column, used only within
    %                             'MaxDist'. Default 'BP_RP_NEAR'.
    %            'DistCol'      - Match distance [arcsec] belonging to
    %                             'NearColorCol'. Default 'GAIA_DIST'.
    %            'MaxDist'      - Largest accepted separation [arcsec]. Default 1.
    % Output : - Color : column vector, NaN where the colour may not be used.
    %          - Valid : logical, true where it may.
    % Author : Dana Kovaleva (Sep 2026)
    % Example: [C, V] = imProc.cat.usableColor(AI.CatData);

    arguments
        Cat
        Args.ColorCol char        = 'BP_RP'
        Args.NearColorCol char    = 'BP_RP_NEAR'
        Args.DistCol char         = 'GAIA_DIST'
        Args.MaxDist (1,1) double = 1
    end

    CN = Cat.ColNames;
    if any(strcmp(CN, Args.ColorCol))
        % Strict match: the column already means "this source's colour".
        Color = double(Cat.getCol(Args.ColorCol));
        Color = Color(:);
        Valid = isfinite(Color);
    elseif any(strcmp(CN, Args.NearColorCol))
        Color = double(Cat.getCol(Args.NearColorCol));
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
                 'is unknown - the colour is treated as unusable.'], Args.NearColorCol, Args.DistCol);
            Valid = false(size(Color));
        end
    else
        Nrow  = size(Cat.Catalog, 1);
        Color = nan(Nrow, 1);
        Valid = false(Nrow, 1);
    end
    Color(~Valid) = NaN;
end
