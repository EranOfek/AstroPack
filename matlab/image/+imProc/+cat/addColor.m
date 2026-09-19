function Result = addColor(Obj, Args)
    % Add Gaia colour (BP-RP) and/or magnitude columns to a source catalog by
    % cross-matching its RA/Dec against a Gaia catsHTM catalog.
    %
    %   Issue #1289. Called by pipeline.last.pipes.pipelineI for both the epoch
    %   catalogs (directly, once per sub image) and the coadd catalog (through
    %   procCoadd -> astrometryRefine); everywhere else it is opt-in and
    %   catalogs are unchanged unless asked. It is meant to run right after the
    %   sources acquire a WCS RA/Dec, so the colour a
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
    %                         NOTE that an astrometric reference is magnitude
    %                         limited (getAstrometricCatalog applies RefRangeMag
    %                         and an isolation cut), so reusing it leaves the
    %                         colour empty for everything outside that range;
    %                         pass [] for a colour column as complete as Gaia is.
    %            'SharedRefCat' - Relevant only when 'RefCat' is empty and Obj has
    %                         more than one element that image the SAME field
    %                         (e.g. the epochs of one sub image). If true, one
    %                         cone search is made over the union of the elements'
    %                         footprints and reused for all of them, instead of
    %                         one search per element. Do not set it for elements
    %                         pointing at unrelated fields: the union circle then
    %                         covers everything between them. Default false.
    %            'Radius'   - Match radius. Default 1.
    %            'RadiusUnits' - Default 'arcsec'.
    %            'GaiaCols' - Cell array of source-column names to pull from the
    %                         Gaia catalog. Default {'bp_rp'}.
    %            'OutCols'  - Cell array of output column names, same length as
    %                         'GaiaCols'. Default {'BP_RP'}.
    %            'ColSphere' - Names of the spherical coordinate columns of the
    %                         source catalog. An element that lacks them (e.g. its
    %                         astrometric solution failed) gets all-NaN columns
    %                         rather than an error, so a column list built over an
    %                         array of catalogs stays uniform. Default {'RA','Dec'}.
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
        Args.SharedRefCat logical   = false
        Args.Radius                 = 1
        Args.RadiusUnits char       = 'arcsec'
        Args.GaiaCols               = {'bp_rp'}
        Args.OutCols                = {'BP_RP'}
        Args.ColSphere              = {'RA','Dec'}
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

    % The matcher requires the Gaia catalog to be sorted by Dec, and sorting a
    % ~50-column Gaia catalog is the dominant cost of this function. When one
    % reference serves every element — the pipeline case: one astrometric
    % catalog per sub-image, all epochs of that sub-image — sort it once here
    % instead of once per element.
    SharedSorted = [];
    if ~isempty(Args.RefCat) && numel(Args.RefCat) == 1 && ...
            ~isemptyCatalog(Args.RefCat) && all(ismember(Args.GaiaCols, Args.RefCat.ColNames))
        SharedSorted = sortrows(Args.RefCat.copy, 'Dec');
    end

    % No reference given, but all elements image the same field: cone-search
    % once over the union of their footprints and reuse it, instead of
    % repeating the query per element.
    if isempty(SharedSorted) && isempty(Args.RefCat) && Args.SharedRefCat && numel(Result) > 1
        [UX, UY, UR] = unionFootprint(Result, Args.ColSphere, Args.boundingCircleArgs);
        if isfinite(UR) && UR > 0
            GaiaUnion = catsHTM.cone_search(Args.CatName, UX, UY, UR, ...
                                            'RadiusUnits','rad', 'OutType','astrocatalog');
            if ~isemptyCatalog(GaiaUnion)
                SharedSorted = sortrows(GaiaUnion, 'Dec');
            end
        end
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

        Nsrc = sizeCatalog(Cat);
        Ncol = numel(Args.GaiaCols);
        ColData = nan(Nsrc, Ncol);

        % Sources with no sky coordinates cannot be matched. This happens for
        % real when an astrometric solution fails, so it must not throw: the
        % element keeps all-NaN columns and the array stays column-uniform.
        if ~all(ismember(Args.ColSphere, Cat.ColNames))
            warning('imProc:cat:addColor:NoSkyCoo', ...
                'Catalog %d has no %s columns - inserting NaN colour columns.', ...
                Iobj, strjoin(Args.ColSphere, '/'));
        else
            % Gaia reference: reuse the provided one if it carries the requested
            % columns, otherwise cone-search this footprint.
            GaiaSorted = SharedSorted;
            if isempty(GaiaSorted)
                GaiaCat = [];
                if ~isempty(Args.RefCat) && numel(Args.RefCat) > 1
                    GaiaCat = Args.RefCat(Iobj);
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
                if ~isemptyCatalog(GaiaCat)
                    GaiaSorted = sortrows(GaiaCat.copy, 'Dec');
                end
            end

            if ~isempty(GaiaSorted) && ~isemptyCatalog(GaiaSorted)
                % Match with the MEX binary-search matcher (imProc.match.matchInd):
                % ResInd(1).Ind holds, for each source, the row of its nearest Gaia
                % match (NaN if none). The matcher requires catalog 2 to be sorted
                % by Dec, hence the Dec-sorted working copy above; the Gaia columns
                % are read from that same copy, which makes index remapping
                % unnecessary.
                ResInd = imProc.match.matchInd(Cat, GaiaSorted, 'IsSpherical',true, ...
                                               'ColSphere1',Args.ColSphere, ...
                                               'SearchRadius',Args.Radius, 'SearchRadiusUnits',Args.RadiusUnits);
                IndInGaia = ResInd(1).Ind;
                Matched   = ~isnan(IndInGaia);
                for Icol = 1:Ncol
                    GaiaVal = getGaiaCol(GaiaSorted, Args.GaiaCols{Icol});
                    ColData(Matched, Icol) = GaiaVal(IndInGaia(Matched));
                end
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

function [X, Y, R] = unionFootprint(Obj, ColSphere, bcArgs)
    % Smallest circle [rad] covering the bounding circles of all elements.
    %   Centre = direction of the mean unit vector of the element centres;
    %   radius = max over elements of (distance to that centre + own radius).
    %   Elements without sky coordinates or with an unusable catalog are
    %   skipped; R is NaN when none is usable.
    Nobj = numel(Obj);
    Cx = nan(Nobj,1);  Cy = nan(Nobj,1);  Cr = nan(Nobj,1);
    for Iobj = 1:1:Nobj
        if isa(Obj(Iobj), 'AstroImage') || isa(Obj(Iobj), 'AstroDiff') || isa(Obj(Iobj), 'AstroZOGY')
            Cat = Obj(Iobj).CatData;
        else
            Cat = Obj(Iobj);
        end
        if isemptyCatalog(Cat) || ~all(ismember(ColSphere, Cat.ColNames))
            continue;
        end
        try
            [Cx(Iobj), Cy(Iobj), Cr(Iobj)] = Cat.boundingCircle('OutUnits','rad', 'CooType','sphere', bcArgs{:});
        catch
            % leave NaN - this element just does not constrain the union
        end
    end
    Ok = isfinite(Cx) & isfinite(Cy) & isfinite(Cr);
    if ~any(Ok)
        X = NaN;  Y = NaN;  R = NaN;
        return;
    end
    Cx = Cx(Ok);  Cy = Cy(Ok);  Cr = Cr(Ok);
    % Mean unit vector -> centre direction (immune to the RA=0/2pi seam).
    V = [mean(cos(Cy).*cos(Cx)), mean(cos(Cy).*sin(Cx)), mean(sin(Cy))];
    Norm = sqrt(sum(V.^2));
    if Norm == 0
        X = Cx(1);  Y = Cy(1);  R = max(Cr);
        return;
    end
    V = V ./ Norm;
    X = atan2(V(2), V(1));
    if X < 0; X = X + 2.*pi; end
    Y = asin(V(3));
    D = celestial.coo.sphere_dist(X, Y, Cx, Cy, 'rad');
    R = max(D(:) + Cr(:));
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
