function Result = catalogNameStr(CatName, Args)
    % Return the name of a reference catalog as a string, for header keywords.
    %   A reference catalog is specified either by name (e.g., 'GAIADR3') or
    %   as an already fetched AstroCatalog object. In the second case the name
    %   is taken from the object's Name property, which
    %   imProc.cat.getAstrometricCatalog stamps when it fetches the catalog,
    %   so that the provenance travels with the object when it is reused.
    % Input  : - A catalog name (char/string) or an AstroCatalog object.
    %          * ...,key,val,...
    %            'Unknown' - String to return when the catalog is an object
    %                   with no Name (e.g., a user-supplied catalog).
    %                   Default is 'USER'.
    % Output : - The catalog name [char].
    % Author : A.M. Krassilchtchikov (Sep 2026)
    % Example: Str = imProc.cat.catalogNameStr('GAIADR3');
    %          Str = imProc.cat.catalogNameStr(AstrometricCat);

    arguments
        CatName
        Args.Unknown = 'USER';
    end

    if ischar(CatName) || isstring(CatName)
        Result = char(CatName);
    elseif isa(CatName, 'AstroTable') && ~isempty(CatName) && ~isempty(CatName(1).Name)
        Result = char(CatName(1).Name);
    else
        Result = Args.Unknown;
    end
end
