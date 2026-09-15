function Result = addColor(Obj, Args)
    % Add Gaia colour (BP-RP) and/or magnitude columns to a source catalog by
    % cross-matching its RA/Dec against a Gaia catsHTM catalog.
    %
    %   Opt-in helper (issue #1289). Nothing in the pipeline calls it unless
    %   asked; catalogs are unchanged otherwise. It is intended to be called by
    %   the astrometric solvers (imProc.astrometry.astrometryCore /
    %   astrometryRefine) once the sources carry a WCS RA/Dec, so the colour a
    %   colour-dependent photometric calibration needs (e.g. the per-source
    %   RefSpecSlope of PhotCalibTrans, issue #1287) is present as a plain
    %   catalog column. To avoid a second catsHTM query it can reuse the Gaia
    %   reference catalog the astrometry already loaded ('RefCat').
    %
    % Input  : - An AstroImage / AstroDiff / AstroZOGY (its CatData is used) or
    %            an AstroCatalog array. Each element must carry sky coordinates.
    %          * ...,key,val,...
    %            'CatName'  - catsHTM catalog to match against when 'RefCat' is
    %                         empty. Default 'GAIADR3'.
    %            'RefCat'   - A pre-loaded Gaia AstroCatalog (e.g. the astrometric
    %                         reference from getAstrometricCatalog) to match
    %                         against instead of a fresh cone search. Its
    %                         coordinates must be spherical, and it must carry the
    %                         'GaiaCols' below. A single AstroCatalog is reused
    %                         for every element of Obj. Default [] (cone_search).
    %            'Radius'   - Match radius. Default 1.
    %            'RadiusUnits' - Default 'arcsec'.
    %            'GaiaCols' - Cell array of source-column names to pull from the
    %                         Gaia catalog. Default {'bp_rp'}.
    %            'OutCols'  - Cell array of output column names, same length as
    %                         'GaiaCols'. Default {'BP_RP'}.
    %            'ColPos'   - Column position for insertion. Default Inf (append).
    %            'CreateNewObj' - Operate on a copy. Default false.
    % Output : - The input object with the requested colour/magnitude columns
    %            inserted into each element's catalog. Sources with no Gaia match
    %            within 'Radius' get NaN.
    % Author : Dana Kovaleva (Sep 2026)
    % Example:
    %   AI = imProc.cat.addColor(AI);                              % BP_RP, own cone search
    %   AI = imProc.cat.addColor(AI, 'GaiaCols',{'bp_rp','phot_bp_mean_mag','phot_rp_mean_mag'}, ...
    %                                'OutCols',{'BP_RP','MAG_BP','MAG_RP'});
    %   % Reuse the astrometric reference already in memory:
    %   AI = imProc.cat.addColor(AI, 'RefCat', AstrometricCat);

    arguments
        Obj
        Args.CatName char           = 'GAIADR3'
        Args.RefCat                 = []
        Args.Radius                 = 1
        Args.RadiusUnits char       = 'arcsec'
        Args.GaiaCols               = {'bp_rp'}
        Args.OutCols                = {'BP_RP'}
        Args.ColPos                 = Inf
        Args.CreateNewObj logical   = false
        Args.boundingCircleArgs cell = {}
    end

    if ischar(Args.GaiaCols); Args.GaiaCols = {Args.GaiaCols}; end
    if ischar(Args.OutCols);  Args.OutCols  = {Args.OutCols};  end
    if numel(Args.GaiaCols) ~= numel(Args.OutCols)
        error('imProc:cat:addColor:ColMismatch', ...
            'GaiaCols and OutCols must have the same number of entries.');
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Nobj = numel(Result);
    for Iobj = 1:1:Nobj
        % Resolve the catalog to operate on (AstroImage -> its CatData).
        if isa(Result(Iobj), 'AstroImage') || isa(Result(Iobj), 'AstroDiff') || isa(Result(Iobj), 'AstroZOGY')
            Cat = Result(Iobj).CatData;
        else
            Cat = Result(Iobj);
        end

        if isemptyCatalog(Cat)
            continue;
        end

        % Gaia reference: reuse the provided one if it carries the requested
        % columns, otherwise cone-search this footprint.
        GaiaCat = [];
        if ~isempty(Args.RefCat)
            if numel(Args.RefCat) == 1
                GaiaCat = Args.RefCat;
            else
                GaiaCat = Args.RefCat(Iobj);
            end
            % Fall back to a fresh search if the reference lacks any column.
            if ~all(ismember(Args.GaiaCols, GaiaCat.ColNames))
                GaiaCat = [];
            end
        end
        if isempty(GaiaCat)
            [CircX, CircY, CircR] = Cat.boundingCircle('OutUnits','rad', 'CooType','sphere', Args.boundingCircleArgs{:});
            GaiaCat = catsHTM.cone_search(Args.CatName, CircX, CircY, CircR, ...
                                          'RadiusUnits','rad', 'OutType','astrocatalog');
        end

        Nsrc = sizeCatalog(Cat);
        Ncol = numel(Args.GaiaCols);
        ColData = nan(Nsrc, Ncol);

        if ~isemptyCatalog(GaiaCat)
            % Match Gaia (Obj1) against the source catalog (Obj2): Obj2_IndInObj1
            % is, for each source, the row of its nearest Gaia match (NaN if none).
            ResInd = imProc.match.matchReturnIndices(GaiaCat, Cat, 'CooType','sphere', ...
                                                     'Radius',Args.Radius, 'RadiusUnits',Args.RadiusUnits);
            IndInGaia = ResInd(1).Obj2_IndInObj1;
            Matched   = ~isnan(IndInGaia);
            for Icol = 1:Ncol
                GaiaVal = getGaiaCol(GaiaCat, Args.GaiaCols{Icol});
                ColData(Matched, Icol) = GaiaVal(IndInGaia(Matched));
            end
        end

        for Icol = 1:Ncol
            Cat = replaceOrInsert(Cat, ColData(:, Icol), Args.ColPos, Args.OutCols{Icol});
        end

        % Write the catalog back.
        if isa(Result(Iobj), 'AstroImage') || isa(Result(Iobj), 'AstroDiff') || isa(Result(Iobj), 'AstroZOGY')
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj) = Cat;
        end
    end
end

function tf = isemptyCatalog(Cat)
    % True when the catalog has no rows.
    tf = isempty(Cat) || isempty(Cat.Catalog) || size(Cat.Catalog,1) == 0;
end

function n = sizeCatalog(Cat)
    n = size(Cat.Catalog, 1);
end

function v = getGaiaCol(GaiaCat, ColName)
    % Fetch a Gaia column as a numeric column vector.
    v = GaiaCat.getCol(ColName);
    v = v(:);
end

function Cat = replaceOrInsert(Cat, Data, ColPos, ColName)
    % Insert a new column, or overwrite it in place if it already exists,
    % so a re-run does not create a duplicate column.
    if any(strcmp(Cat.ColNames, ColName))
        Cat = Cat.replaceCol(Data(:), ColName);
    else
        Cat = Cat.insertCol(Data(:), ColPos, {ColName});
    end
end
