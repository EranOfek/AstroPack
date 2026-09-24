function Result = addColor(Obj, Args)
    % Cross-match a source catalog against Gaia and attach, for the nearest
    % match, the requested Gaia columns, its separation and a neighbour count.
    %
    %   NAME: the function outgrew 'addColor' - the colour is now one of three
    %   products - and is to be renamed imProc.cat.addGaiaMatch in a separate
    %   change, together with the AddColor* pipeline arguments.
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
    %            'PropagatePM' - Move the Gaia reference from its catalog epoch
    %                         to the observation epoch with celestial.coo.proper_motion
    %                         before matching. Gaia PMRA is mu_alpha*cos(dec), the
    %                         convention that function expects. Rows with no PM keep
    %                         their catalog position, and the step is skipped entirely
    %                         when the PM columns or the observation epoch are absent,
    %                         so a catalog without them matches exactly as before.
    %                         Measured on a LAST field at dt=9.0 yr: median shift
    %                         0.09", 0.7%% of Gaia rows move more than the 1" match
    %                         radius. Default true.
    %            'ObsJD'    - Observation JD driving the propagation. Empty (default)
    %                         takes it from the AstroImage header ('JD', then 'MIDJD');
    %                         a bare AstroCatalog without it simply skips propagation.
    %                         When one reference serves several elements, the median
    %                         of their JDs is used - they image the same field within
    %                         a visit, so the spread is minutes.
    %            'PMCols'   - Gaia PM column names [mas/yr]. Default {'PMRA','PMDec'}.
    %            'PlxCol'   - Gaia parallax column [mas]. Default 'Plx'.
    %            'EpochCol' - Gaia epoch column [Julian yr]. Default 'Epoch'; when the
    %                         column is absent 'CatEpoch' is used.
    %            'CatEpoch' - Fallback catalog epoch [Julian yr]. Default 2016 (Gaia DR3).
    %            'AddNeighborCols' - Opt in to the blend-screening columns
    %                         (issue #1306): the distance to the nearest Gaia
    %                         source and how many lie within 'Radius'. Default
    %                         FALSE. Intended together with a wider 'Radius'
    %                         (e.g. 5") and an 'OutCols' name that says so
    %                         (e.g. 'BP_RP_NEAR'), because the colour is then the
    %                         nearest source's and need not be the counterpart.
    %            'NeighborOutCols' - Names of those two columns. Default
    %                         {'GAIA_DIST','GAIA_NSRC'}. GAIA_DIST [arcsec] is NaN
    %                         when nothing is found; GAIA_NSRC is 0 when Gaia was
    %                         searched and empty, NaN when the source could not be
    %                         matched. Set an entry to '' to skip it.
    %
    %   By default this is a strict 1" match producing one column, BP_RP: the
    %   colour of the Gaia source the object is identified with, which is what
    %   the colour term consumes. Widening 'Radius' turns the colour into "the
    %   nearest source's colour" and it must then be read through
    %   imProc.cat.usableColor, which applies a distance cut.
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
        Args.PropagatePM logical    = true
        Args.ObsJD                  = []
        Args.PMCols                 = {'PMRA','PMDec'}
        Args.PlxCol char            = 'Plx'
        Args.EpochCol char          = 'Epoch'
        Args.CatEpoch (1,1) double  = 2016.0
        Args.AddNeighborCols logical = false
        Args.NeighborOutCols        = {'GAIA_DIST','GAIA_NSRC'}
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

    % Observation epoch per element, for the proper-motion step. A missing JD
    % is not an error: that element is matched on the catalog positions.
    ObsJDs = collectObsJD(Result, Args.ObsJD);

    % One match, one radius. The colour reported is that of the NEAREST Gaia
    % source inside it, together with how far away that source is and how many
    % lie inside - the caller decides, from the distance, whether the colour is
    % close enough to belong to the source (see imProc.calib.applyColorTerm's
    % ColorMaxDist).
    MatchAS = toArcsec(Args.Radius, Args.RadiusUnits);

    % The matcher requires the Gaia catalog to be sorted by Dec, and sorting a
    % ~50-column Gaia catalog is the dominant cost of this function. When one
    % reference serves every element — the pipeline case: one astrometric
    % catalog per sub-image, all epochs of that sub-image — sort it once here
    % instead of once per element.
    SharedSorted = [];
    if ~isempty(Args.RefCat) && numel(Args.RefCat) == 1 && ...
            ~isemptyCatalog(Args.RefCat) && all(ismember(Args.GaiaCols, Args.RefCat.ColNames))
        SharedSorted = propagatePM(Args.RefCat.copy, median(ObsJDs,'omitnan'), Args);
        SharedSorted = sortrows(SharedSorted, 'Dec');
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
                GaiaUnion    = propagatePM(GaiaUnion, median(ObsJDs,'omitnan'), Args);
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
        % NaN, not 0, until a match is actually made: an element with no sky
        % coordinates or no Gaia reference must report "unknown", not "no
        % neighbours". The matched branch below overwrites both in full.
        NbrDist = nan(Nsrc, 1);
        NbrN    = nan(Nsrc, 1);

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
                    GaiaSorted = propagatePM(GaiaCat.copy, ObsJDs(Iobj), Args);
                    GaiaSorted = sortrows(GaiaSorted, 'Dec');
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
                                               'SearchRadius',MatchAS, 'SearchRadiusUnits','arcsec');
                IndInGaia = ResInd(1).Ind;
                % matchInd returns Dist in radians (spherical mode converts
                % internally); everything below is in arcsec.
                DistAS    = ResInd(1).Dist(:) .* (180./pi) .* 3600;
                Found     = ~isnan(IndInGaia);

                % The nearest Gaia source inside the match radius: its value(s),
                % its distance, and how many sources lie inside (blends,
                % issue #1306). One match supplies all three.
                for Icol = 1:Ncol
                    GaiaVal = getGaiaCol(GaiaSorted, Args.GaiaCols{Icol});
                    ColData(Found, Icol) = GaiaVal(IndInGaia(Found));
                end
                NbrDist(Found) = DistAS(Found);
                if isfield(ResInd, 'Nmatch') && ~isempty(ResInd(1).Nmatch)
                    NbrN = double(ResInd(1).Nmatch(:));
                else
                    NbrN = countWithin(Cat, GaiaSorted, Args.ColSphere, MatchAS);
                end
            end
        end

        for Icol = 1:Ncol
            Cat = replaceOrInsert(Cat, ColData(:, Icol), Args.ColPos, Args.OutCols{Icol});
        end
        if Args.AddNeighborCols
            if ~isempty(Args.NeighborOutCols{1})
                Cat = replaceOrInsert(Cat, NbrDist, Args.ColPos, Args.NeighborOutCols{1});
            end
            if numel(Args.NeighborOutCols) > 1 && ~isempty(Args.NeighborOutCols{2})
                Cat = replaceOrInsert(Cat, NbrN, Args.ColPos, Args.NeighborOutCols{2});
            end
        end

        % Write the catalog back.
        if isa(Result(Iobj), 'AstroImage') || isa(Result(Iobj), 'AstroDiff') || isa(Result(Iobj), 'AstroZOGY')
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj) = Cat;
        end
    end
end

function AS = toArcsec(Val, Units)
    % Angular value -> arcsec.
    switch lower(Units)
        case {'arcsec','as'}
            AS = Val;
        case {'deg','degree','degrees'}
            AS = Val .* 3600;
        case {'rad','radian','radians'}
            AS = Val .* (180./pi) .* 3600;
        case {'arcmin','am'}
            AS = Val .* 60;
        otherwise
            error('imProc:cat:addColor:BadUnits', 'Unsupported angular units ''%s''.', Units);
    end
end

function JD = collectObsJD(Obj, Given)
    % Observation JD per element: the caller's value if given, else the header
    % 'JD' (then 'MIDJD'). NaN where unavailable - propagation is then skipped.
    Nobj = numel(Obj);
    if ~isempty(Given)
        if isscalar(Given)
            JD = repmat(double(Given), Nobj, 1);
        else
            JD = double(Given(:));
        end
        return;
    end
    JD = nan(Nobj, 1);
    for Iobj = 1:1:Nobj
        if isa(Obj(Iobj), 'AstroImage') || isa(Obj(Iobj), 'AstroDiff') || isa(Obj(Iobj), 'AstroZOGY')
            H = Obj(Iobj).HeaderData;
            if ~isempty(H)
                for Key = {'JD','MIDJD'}
                    if isnan(JD(Iobj)) && H.isKeyExist(Key{1})
                        V = H.getVal(Key{1});
                        if ~isempty(V) && isnumeric(V) && isfinite(V(1))
                            JD(Iobj) = double(V(1));
                        end
                    end
                end
            end
        end
    end
end

function GaiaCat = propagatePM(GaiaCat, ObsJD, Args)
    % Move the Gaia reference to the observation epoch with the shared
    % celestial.coo.proper_motion (Gaia PMRA = mu_alpha*cos(dec), which is the
    % convention that function expects). Silently a no-op when switched off, or
    % when the PM columns or the epoch are unavailable, so callers that lack
    % them behave exactly as before. Rows with no PM keep their catalog
    % position rather than being dropped.
    if ~Args.PropagatePM || isempty(ObsJD) || ~isfinite(ObsJD) || isemptyCatalog(GaiaCat)
        return;
    end
    CN = GaiaCat.ColNames;
    if ~all(ismember(Args.PMCols, CN)) || ~all(ismember({'RA','Dec'}, CN))
        return;
    end
    PMRA  = double(GaiaCat.getCol(Args.PMCols{1}));   % mas/yr, mu_alpha*cos(dec)
    PMDec = double(GaiaCat.getCol(Args.PMCols{2}));   % mas/yr
    Ok    = isfinite(PMRA) & isfinite(PMDec);
    if ~any(Ok)
        return;
    end
    [RA, Dec] = GaiaCat.getLonLat('rad');
    RA = double(RA(:));  Dec = double(Dec(:));

    if any(strcmp(CN, Args.EpochCol))
        EpochYr = double(GaiaCat.getCol(Args.EpochCol));
        EpochYr(~isfinite(EpochYr)) = Args.CatEpoch;
    else
        EpochYr = repmat(Args.CatEpoch, numel(RA), 1);
    end
    EpochJD = 2451545.0 + (EpochYr - 2000).*365.25;

    if any(strcmp(CN, Args.PlxCol))
        Plx = double(GaiaCat.getCol(Args.PlxCol));
        Plx(~isfinite(Plx)) = 1e-4;     % proper_motion clamps non-positive values anyway
    else
        Plx = repmat(1e-4, numel(RA), 1);
    end

    [NewRA, NewDec] = celestial.coo.proper_motion(ObsJD, EpochJD(Ok), [], ...
                                                  RA(Ok), Dec(Ok), PMRA(Ok), PMDec(Ok), Plx(Ok));
    Good = isfinite(NewRA) & isfinite(NewDec);
    Idx  = find(Ok);
    RA(Idx(Good))  = NewRA(Good);
    Dec(Idx(Good)) = NewDec(Good);

    % Write back in whatever units the catalog stores its coordinates in.
    % Derived from the data rather than from metadata: getLonLat always returns
    % radians, so the ratio against the stored column is exactly the scale
    % (1 for radians, 180/pi for degrees).
    Stored = double(GaiaCat.getCol('Dec'));
    [~, DecRad] = GaiaCat.getLonLat('rad');
    Use = isfinite(Stored) & isfinite(DecRad(:)) & abs(DecRad(:)) > 1e-6;
    if any(Use)
        Scale = median(Stored(Use) ./ double(DecRad(Use)), 'omitnan');
    else
        Scale = 1;
    end
    if ~isfinite(Scale) || Scale <= 0
        Scale = 1;
    end
    GaiaCat = GaiaCat.replaceCol(RA .*Scale, 'RA');
    GaiaCat = GaiaCat.replaceCol(Dec.*Scale, 'Dec');
end

function N = countWithin(Cat, GaiaSorted, ColSphere, RadiusAS)
    % Number of Gaia sources within RadiusAS of each source.
    R = imProc.match.matchInd(Cat, GaiaSorted, 'IsSpherical',true, ...
                              'ColSphere1',ColSphere, ...
                              'SearchRadius',RadiusAS, 'SearchRadiusUnits','arcsec');
    N = double(R(1).Nmatch(:));
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
