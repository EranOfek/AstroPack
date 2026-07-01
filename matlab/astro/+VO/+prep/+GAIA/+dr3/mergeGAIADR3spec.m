function Result = mergeGAIADR3spec(OutDir, Args)
    % Append the 8 Gaia DR3 columns to GAIADR3spec in one pass (same name).
    % Description: Enrich the GAIADR3spec catsHTM catalog with the Gaia DR3
    %              proper-motion, photometry and classifier columns needed
    %              by imProc.transmission.fitPhotCalibTrans with
    %              'SelectionMethod','pythonLike', so that a single
    %              match_catsHTM('GAIADR3spec') supplies everything (no
    %              second GAIADR3 match, no online query at calibration time).
    %
    %              All 8 columns are sourced LOCALLY (no TAP). For every
    %              GAIADR3spec HTM cell, the cell's J2016 RA/Dec are matched:
    %                - to the GAIADR3 catsHTM  -> 7 columns: PMRA, PMDec,
    %                  phot_g_mean_mag, phot_bp_mean_mag, phot_rp_mean_mag,
    %                  bp_rp, phot_bp_rp_excess_factor;
    %                - to the GAIADR3classprob catsHTM (built by
    %                  VO.prep.GAIA.dr3.buildGaiaClassprobHTM from a
    %                  pre-downloaded VOTable) -> classprob_dsc_combmod_star.
    %              The matched values are appended as columns 693..700.
    %
    %              Mechanics are delegated to catsHTM.insertColumns, which
    %              does the whole thing in ONE pass over the (fat, ~700-col)
    %              catalog: BaseDir is read-only, the rewritten catalog is
    %              written under OutDir with the same name, and the HTM index
    %              copies over unchanged (Nsrc per cell is unchanged). Move
    %              OutDir to the production mount (e.g. /euclid/catsHTM)
    %              manually afterwards.
    %
    %              Both GAIADR3 and GAIADR3classprob must be resolvable by
    %              match_catsHTM (on the catsHTM path) when this runs.
    %              Sources with no match get NaN in the corresponding columns.
    % Input  : - OutDir : Writable directory mirroring BaseDir, where the
    %                     rewritten GAIADR3spec is placed.
    %          * ...,key,val,...
    %            'CatName'         - Catalog to enrich. Default 'GAIADR3spec'.
    %            'GaiaCatName'     - catsHTM with the 7 photometry/PM columns.
    %                                Default 'GAIADR3'.
    %            'ClassprobCatName'- catsHTM with classprob_dsc_combmod_star.
    %                                Default 'GAIADR3classprob'.
    %            'ClassprobDir'    - Directory of GAIADR3classprob; addpath-ed
    %                                so it resolves (catsHTM opens by bare name
    %                                via the MATLAB path). Default '' (assume
    %                                already on path).
    %            'GaiaDir'         - Directory of GAIADR3 if not already on the
    %                                path (startup usually adds it). Default ''.
    %            'BaseDir'         - Source catsHTM root (read-only). Default
    %                                '' (resolved by catsHTM.insertColumns).
    %            'CatRelDir'       - Catalog subdir under BaseDir. Default ''.
    %            'SearchRadius'    - Positional match radius [arcsec]. Default
    %                                1 (GAIADR3spec/GAIADR3/classprob share
    %                                J2016 positions).
    %            'NfilesInHDF'     - Cells per data file. Default 100.
    %            'SortCol'         - Existing SortCol (Dec). Default 2.
    %            'StepRows'        - save_cat index step. Default 30.
    %            'SkipExisting'    - Resume: skip source cells already fully
    %                                written in OutDir (every htm dataset at
    %                                the post-insert column count). Partially
    %                                written / old-width files are redone.
    %                                Default false.
    %            'DryRun'          - List affected files, no writes. Default false.
    %            'Check'           - Run checkGAIADR3specMerge on the written
    %                                catalog and warn if any new column is
    %                                missing/all-NaN. Skipped on DryRun. Default true.
    %            'Verbose'         - Print progress. Default true.
    % Output : - Result struct from catsHTM.insertColumns (.OutDir,
    %            .ModifiedFiles, .CellsTouched, .RowsTouched, .NewColCell,
    %            .NewSortCol), plus .Check (output of checkGAIADR3specMerge,
    %            or [] when DryRun/Check=false).
    % Author : Dana Kovaleva (Jun 2026)
    % Example:
    %   VO.prep.GAIA.dr3.buildGaiaClassprobHTM('/home/dana/tmp/xpcols/classprob.vot');
    %   addpath('/home/dana/tmp/xpcols');   % so GAIADR3classprob resolves
    %   R = VO.prep.GAIA.dr3.mergeGAIADR3spec('/data/tmp/GAIADR3spec_merged');

    arguments
        OutDir                  char
        Args.CatName            char    = 'GAIADR3spec'
        Args.GaiaCatName        char    = 'GAIADR3'
        Args.ClassprobCatName   char    = 'GAIADR3classprob'
        Args.ClassprobDir       char    = ''
        Args.GaiaDir            char    = ''
        Args.BaseDir            char    = ''
        Args.CatRelDir          char    = ''
        Args.SearchRadius       double  = 1
        Args.NfilesInHDF        double  = 100
        Args.SortCol            double  = 2
        Args.StepRows           double  = 30
        Args.SkipExisting       logical = false
        Args.DryRun             logical = false
        Args.Check              logical = true
        Args.Verbose            logical = true
    end

    % Names/units appended to GAIADR3spec. Names MUST match both the source
    % catsHTM columns and the runtime lookups in
    % PhotCalibTrans.selectCalibratorsPythonLike. The first 7 come from
    % GaiaCatName, the 8th from ClassprobCatName.
    GaiaCols   = {'PMRA','PMDec','phot_g_mean_mag','phot_bp_mean_mag', ...
                  'phot_rp_mean_mag','bp_rp','phot_bp_rp_excess_factor'};
    ClassName  = 'classprob_dsc_combmod_star';
    NewNames   = [GaiaCols, {ClassName}];
    NewUnits   = {'mas/yr','mas/yr','mag','mag','mag','mag','',''};

    % catsHTM index/data files are opened by bare name (HDF5.load -> H5F.open
    % resolves via the MATLAB path), so the catalogs must be on the path.
    % GAIADR3 is added by startup; a custom GAIADR3classprob usually is not -
    % addpath its directory here if given (idempotent).
    if ~isempty(Args.ClassprobDir)
        addpath(Args.ClassprobDir);
    end
    if ~isempty(Args.GaiaDir)
        addpath(Args.GaiaDir);
    end

    % Load the two source catalogs' HTM indices + colcell column maps ONCE.
    % The slow path re-loaded each index/colcell per cell (via match_catsHTM
    % -> cone_search) and ran fminsearch boundingCircle per cell. Here we
    % cache the indices and match each cell in memory (the xmatch_2cats
    % pattern: load_cat by id + VO.search.match_cats).
    Cache = localLoadCache(Args.GaiaCatName, GaiaCols, ...
                           Args.ClassprobCatName, ClassName, Args.NfilesInHDF);

    % Per-cell FillValue: match this cell's J2016 RA/Dec to GAIADR3 (7 cols)
    % and GAIADR3classprob (1 col) in memory; return the [Nrows x 8] block.
    FillFun = @(M) localMatchBlockFast(M, Cache, Args.SearchRadius);

    Result = catsHTM.insertColumns(Args.CatName, NewNames, NewUnits, OutDir, ...
        'BaseDir',     Args.BaseDir, ...
        'CatRelDir',   Args.CatRelDir, ...
        'FillValue',   FillFun, ...
        'Position',    'end', ...
        'SortCol',     Args.SortCol, ...
        'StepRows',    Args.StepRows, ...
        'NfilesInHDF', Args.NfilesInHDF, ...
        'SkipExisting',Args.SkipExisting, ...
        'DryRun',      Args.DryRun, ...
        'Verbose',     Args.Verbose);

    % --- Post-merge sanity check on the written catalog -----------------
    Result.Check = [];
    if Args.Check && ~Args.DryRun
        Result.Check = VO.prep.GAIA.dr3.checkGAIADR3specMerge( ...
            'CatName',     Args.CatName, ...
            'BaseDir',     OutDir, ...
            'CatRelDir',   Args.CatRelDir, ...
            'ColNames',    NewNames, ...
            'NfilesInHDF', Args.NfilesInHDF, ...
            'Verbose',     Args.Verbose);
        if ~Result.Check.Ok
            Bad = [Result.Check.MissingCols, Result.Check.AllNaNCols];
            warning('VO:prep:GAIA:dr3:mergeGAIADR3spec:CheckFailed', ...
                ['Post-merge check found missing/all-NaN column(s): %s. ', ...
                 'Were GAIADR3 and GAIADR3classprob on the catsHTM path during the merge?'], ...
                strjoin(Bad, ', '));
        end
    end
end


function Cache = localLoadCache(GaiaCat, GaiaCols, ClassCat, ClassName, NfilesInHDF)
    % Load HTM index struct + colcell column indices for both source
    % catalogs ONCE so per-cell matching needs no disk index/colcell reload.
    Cache = struct();
    Cache.NfilesInHDF = NfilesInHDF;

    % GAIADR3 (7 photometry/PM columns)
    [IFg, IVg] = catsHTM.get_index_filename(GaiaCat);
    [Cache.HTMg, Cache.DataHTMg] = catsHTM.load_htm_ind(IFg, IVg);
    ColCellG      = catsHTM.load_colcell(GaiaCat);
    Cache.GaiaCat = GaiaCat;
    Cache.NcolG   = numel(ColCellG);
    Cache.GcolIdx = zeros(1, numel(GaiaCols));
    for K = 1:numel(GaiaCols)
        J = find(strcmpi(ColCellG, GaiaCols{K}), 1);
        if isempty(J)
            error('VO:prep:GAIA:dr3:mergeGAIADR3spec:MissingGaiaCol', ...
                'Column "%s" not found in %s.', GaiaCols{K}, GaiaCat);
        end
        Cache.GcolIdx(K) = J;
    end

    % GAIADR3classprob (classprob_dsc_combmod_star)
    [IFc, IVc] = catsHTM.get_index_filename(ClassCat);
    [Cache.HTMc, Cache.DataHTMc] = catsHTM.load_htm_ind(IFc, IVc);
    ColCellC       = catsHTM.load_colcell(ClassCat);
    Cache.ClassCat = ClassCat;
    Cache.NcolC    = numel(ColCellC);
    Cache.ClassIdx = find(strcmpi(ColCellC, ClassName), 1);
    if isempty(Cache.ClassIdx)
        error('VO:prep:GAIA:dr3:mergeGAIADR3spec:MissingClassCol', ...
            'Column "%s" not found in %s.', ClassName, ClassCat);
    end
end


function Block = localMatchBlockFast(M, Cache, SearchRadius)
    % In-memory match of one GAIADR3spec cell to GAIADR3 (7 cols) and
    % GAIADR3classprob (1 col) using the cached HTM indices. No per-cell
    % index/colcell reload, no cone_search, no fminsearch.
    RAD  = 180./pi;
    Nrow = size(M, 1);
    Block = nan(Nrow, numel(Cache.GcolIdx) + 1);
    if Nrow == 0
        return
    end

    RA_rad        = double(M(:,1));
    Dec_rad       = double(M(:,2));
    SearchRad_rad = SearchRadius ./ (RAD .* 3600);
    Query         = [RA_rad, Dec_rad];   % [Nrow x 2] radians, cols 1-2 = RA/Dec

    % Cone covering this cell's sources (unit-vector centroid + max sep,
    % robust to RA wrap).
    Cx   = mean(cos(Dec_rad).*cos(RA_rad));
    Cy   = mean(cos(Dec_rad).*sin(RA_rad));
    Cz   = mean(sin(Dec_rad));
    cRA  = atan2(Cy, Cx);
    cDec = atan2(Cz, hypot(Cx, Cy));
    ConeR = max(celestial.coo.sphere_dist_fast(cRA, cDec, RA_rad, Dec_rad)) + SearchRad_rad;

    % --- GAIADR3: 7 photometry/PM columns ---
    CatG = localLoadCells(Cache.GaiaCat, Cache.HTMg, Cache.DataHTMg, ...
                          cRA, cDec, ConeR, Cache.NcolG, Cache.NfilesInHDF);
    if ~isempty(CatG)
        CatG = sortrows(CatG, 2);
        [~, ~, IndMin] = VO.search.match_cats(CatG, Query, ...
            'Radius', SearchRad_rad, 'RadiusUnits', 'rad');
        Ok = ~isnan(IndMin);
        if any(Ok)
            Block(Ok, 1:numel(Cache.GcolIdx)) = CatG(IndMin(Ok), Cache.GcolIdx);
        end
    end

    % --- GAIADR3classprob: classprob column ---
    CatC = localLoadCells(Cache.ClassCat, Cache.HTMc, Cache.DataHTMc, ...
                          cRA, cDec, ConeR, Cache.NcolC, Cache.NfilesInHDF);
    if ~isempty(CatC)
        CatC = sortrows(CatC, 2);
        [~, ~, IndMinC] = VO.search.match_cats(CatC, Query, ...
            'Radius', SearchRad_rad, 'RadiusUnits', 'rad');
        OkC = ~isnan(IndMinC);
        if any(OkC)
            Block(OkC, end) = CatC(IndMinC(OkC), Cache.ClassIdx);
        end
    end
end


function Cat = localLoadCells(CatName, HTMstruct, DataHTM, cRA, cDec, ConeR, Ncol, NfilesInHDF)
    % Load (by HTM id; no index reload) and concatenate all populated cells
    % of CatName overlapping the cone. Returns [Nsrc x Ncol] double.
    Cat = zeros(0, Ncol);
    ID  = celestial.htm.htm_search_cone(HTMstruct, cRA, cDec, ConeR);
    if isempty(ID)
        return
    end
    ID = ID(DataHTM(ID, 13) > 0);          % populated cells only
    for K = 1:numel(ID)
        C = catsHTM.load_cat(CatName, ID(K), [], Ncol, NfilesInHDF);
        if isempty(C)
            continue
        end
        % Orientation-robust: ensure [Nsrc x Ncol] (load_cat output layout
        % differs by call form; transpose only if clearly column-major).
        if size(C,2) ~= Ncol && size(C,1) == Ncol
            C = C.';
        end
        Cat = [Cat; double(C)];            %#ok<AGROW>
    end
end
