function [XidTable, Cats, Summary] = crossIDCatsHTM(RA, Dec, Radius, Args)
    % Build a cross-identification index table between one anchor catsHTM
    % catalog and all (or selected) other catsHTM catalogs in a field.
    %   For a given field (RA, Dec, search radius) the function cone-searches
    %   an anchor catalog (default GAIADR3) and every other catsHTM catalog,
    %   cross-matches each catalog against a running "master" source list and
    %   records, per master source: the row index of its match in each
    %   catalog and the number of matches within the matching radius.
    %   Sources present in a catalog but absent from the anchor are appended
    %   to the master list with a fresh global index (see 'OrphanHandling'),
    %   so the master list is the union of all sources in the field.
    %   The field is specified like catsHTM.cone_search: positional RA, Dec
    %   [radians] and Radius [arcsec by default].
    % Input  : - Field centre J2000 R.A. [radians] (or in 'CooUnits').
    %            Default is 254 deg (in radians).
    %          - Field centre J2000 Dec. [radians] (or in 'CooUnits').
    %            Default is +64 deg (in radians).
    %          - Field (cone) search radius [arcsec] (or in 'RadiusUnits'). A
    %            deliberately small, safe default of 60 (=1 arcmin); enlarge
    %            for a wider field, e.g. crossIDCatsHTM(RA,Dec,10,...
    %            'RadiusUnits','deg').
    %          * ...,key,val,...
    %            'CooUnits' - Units of the RA/Dec inputs. Default is 'rad'.
    %            'RadiusUnits' - Units of the Radius input. Default is 'arcsec'.
    %            'RefCat' - Anchor catalog name (seeds the master list and its
    %                   global indices). Default is 'GAIADR3'.
    %            'CatList' - Cellstr of catalog names to cross-match against
    %                   the anchor. If empty, all available (Status==true)
    %                   catsHTM catalogs are used. Default is {}.
    %            'SkipCats' - Cellstr of catalog names to exclude. Default {}.
    %            'MatchRadius' - Default matching radius applied to every
    %                   catalog pair. Default is 2.
    %            'MatchRadiusUnits' - Matching radius units. Default 'arcsec'.
    %            'RadiusPerCat' - Per-catalog matching-radius overrides, as an
    %                   Nx2 cell {CatName, Radius; ...} in 'MatchRadiusUnits'.
    %                   Catalogs not listed use 'MatchRadius'. Default is {}.
    %            'OrphanHandling' - How to treat catalog sources with no anchor
    %                   match:
    %                   'growing' - append them to the master list; later
    %                        catalogs can match a previous catalog's orphan
    %                        (union of all sources; order-dependent). Default.
    %                   'append'  - each orphan becomes its own global source;
    %                        catalogs are matched only against the anchor seed
    %                        (no cross-catalog orphan merging).
    %                   'none'    - orphans are dropped (anchor sources only).
    %            'Con' - Cell array of per-catalog cone_search constraints
    %                   (see catsHTM.cone_search). Applied to every catalog.
    %                   Default is {}.
    %            'AddDistCol' - Add a Dist_<Cat> [arcsec] column per catalog.
    %                   Default is true.
    %            'KeepExtraMatches' - When a catalog has SEVERAL sources within
    %                   the match radius of a master source, keep the full list.
    %                   The result stays one row per master source: Ind_<Cat>,
    %                   Nmatch_<Cat>, Dist_<Cat> are for the NEAREST match and
    %                   Nmatch_<Cat> is the true count; the additional (non
    %                   nearest) catalog indices, ordered by distance, are kept
    %                   per row as a VECTOR. These vectors are always returned
    %                   in Summary.ExtraMatches.<Cat> (an Nglobal-by-1 cell;
    %                   each entry is a native-index row vector, or NaN when
    %                   there are 0 or 1 matches). With OutType='table' they are
    %                   also added as an IndExtra_<Cat> cell column. Uses the
    %                   heavier multi-match; NOT supported with TableToDisk, and
    %                   the IndExtra_ cell columns are not written to CSV.
    %                   Default is false.
    %            'AddPointer' - Add a STABLE, query-independent pointer to each
    %                   source: two numeric columns CellID_<Cat> (HTM leaf-cell
    %                   id) and RowInCell_<Cat> (row within htm_<CellID>), the
    %                   intrinsic storage address in the catalog (see
    %                   catsHTM.sourcePointer). Unlike Ind_<Cat> (a cone_search
    %                   row index, valid only for this query) this pair is
    %                   reproducible across sessions and independent of the
    %                   field, and being numeric it is present in BOTH the table
    %                   and the astrocatalog output. Costs a second pass of
    %                   cone-cell reads per catalog, and is SKIPPED (with a note)
    %                   when TableToDisk=true. Default is true.
    %            'OutType' - 'table' (MATLAB table; includes the OriginCat
    %                   column and, when KeepExtraMatches=true, the IndExtra_
    %                   cell columns) or 'astrocatalog' (numeric-column
    %                   AstroCatalog - the text OriginCat and any cell columns
    %                   are NOT included; OriginCat is kept in
    %                   Summary.OriginCat, row-aligned, and extra matches in
    %                   Summary.ExtraMatches). Default is 'table'. Access
    %                   AstroCatalog columns with getCol, e.g.
    %                   Ind = getCol(XidCat,'Ind_PS1'). Ignored when
    %                   TableToDisk=true.
    %            'OutFile' - Output file base/full name. If non-empty, results
    %                   are written to disk (see 'OutFileFormat'). The '.mat'
    %                   stores variable XidTable (the SAME type as the returned
    %                   output - a table by default, or an AstroCatalog) plus
    %                   Cats and Summary (Summary.OriginCat carries provenance);
    %                   the '.csv' is the flat index table and always includes
    %                   the OriginCat column. Default is '' (no file written).
    %            'OutFileFormat' - Cellstr subset of {'mat','csv'} controlling
    %                   which files are written when 'OutFile' is set.
    %                   Default is {'mat','csv'}.
    %            'CatsToDisk' - If true, each catalog's cone_search result is
    %                   written to its own .mat file as soon as it has been
    %                   used, then cleared from memory, so peak memory is set
    %                   by the single largest catalog rather than the sum of
    %                   all of them. Use for large fields where the in-memory
    %                   Cats struct would be huge. The returned Cats then holds
    %                   file PATHS (char) instead of AstroCatalog objects; load
    %                   one with L=load(Cats.<Cat>); L.Cat is the AstroCatalog.
    %                   Ind_<Cat> still indexes that (native-order) catalog.
    %                   Default is false.
    %            'CatsDir' - Target directory for the per-catalog .mat files
    %                   when CatsToDisk=true. Default is the 'OutFile'
    %                   directory if given, otherwise the current directory.
    %            'TableToDisk' - If true, the cross-id table is written to a
    %                   v7.3 .mat column-by-column (via matfile) instead of
    %                   being built in memory, so the wide Nglobal-by-(3*Ncat)
    %                   table never lives in RAM at once (peak memory is set by
    %                   a single column, not the whole table). In this mode the
    %                   FIRST output (XidTable) is the FILE PATH (char), not a
    %                   table; load columns lazily, e.g. matfile(path).Ind_PS1
    %                   or load(path,'Ind_PS1'). The .mat also stores GlobalID,
    %                   RA, Dec, OriginCat, Summary and Cats. This mode skips
    %                   the CSV output and ignores OutType='astrocatalog'.
    %                   Combine with CatsToDisk=true for end-to-end low memory.
    %                   Default is false.
    %            'TableFile' - Target .mat path when TableToDisk=true. Default
    %                   is <OutFile>_xidtable.mat if OutFile is given, else
    %                   ./xidTable.mat.
    %            'Verbose' - Print progress. Default is true.
    % Output : - XidTable: the cross-id table. Columns: GlobalID, RA, Dec
    %            [deg], OriginCat, then Ind_<Cat> (row index into Cats.<Cat>,
    %            NaN if no match), Nmatch_<Cat> (number of catalog sources
    %            within the matching radius), optionally Dist_<Cat> [arcsec],
    %            and (when AddPointer=true) CellID_<Cat> / RowInCell_<Cat> - the
    %            stable per-source storage pointer. By default (OutType='table')
    %            this is a MATLAB table
    %            including OriginCat (and the IndExtra_<Cat> cell columns when
    %            KeepExtraMatches=true); with OutType='astrocatalog' it is an
    %            AstroCatalog with the numeric columns only (OriginCat is in
    %            Summary.OriginCat, extra matches in Summary.ExtraMatches). If
    %            'TableToDisk'=true this output is instead the .mat file PATH
    %            (char) the table was streamed to (see 'TableToDisk').
    %            Ind_<Cat> is the source identifier: since native SourceID
    %            columns are unreliable in many catsHTM catalogs, a source is
    %            identified unambiguously by (Cats.<Cat>, Ind_<Cat>). The
    %            index is in NATIVE cone_search order, so it also reproduces a
    %            fresh catsHTM.cone_search(<Cat>, RA, Dec, Radius, 'Con',Con)
    %            over the field recorded in Summary.Field.
    %          - Cats: a struct with one AstroCatalog per catalog (keyed by
    %            catalog name), holding the cone-search results the Ind_<Cat>
    %            columns index into, in native cone_search order. This is the
    %            lookup table for the indices and is saved with the '.mat'
    %            output so (index -> source) is self-contained and portable.
    %            If 'CatsToDisk'=true, Cats instead holds the file PATH (char)
    %            of each catalog's own .mat file (see 'CatsToDisk').
    %          - Summary: struct with .Field, .RefCat, .Nref, .Nglobal,
    %            .OriginCat, .Failed and a .PerCat table (one row per catalog,
    %            anchor first) with columns:
    %            Ncone - number of sources the catalog cone_search returned.
    %            Nmatched - how many of those Ncone sources matched a master
    %              source (<= Ncone).
    %            Norphan - how many of those Ncone sources had no master match
    %              (appended as new global sources unless OrphanHandling='none').
    %              Nmatched + Norphan = Ncone.
    %            MatchRadiusArcsec - match radius used for that catalog.
    %            If KeepExtraMatches=true, Summary also has .ExtraMatches - a
    %            struct with an Nglobal-by-1 cell per catalog holding the
    %            additional (non-nearest) native match indices per master row.
    % Note   : The FIRST output is a projection of the full result onto what the
    %          chosen container can represent, so its content depends on the
    %          format - but no information is lost, because Summary is the
    %          canonical record (Summary.OriginCat always holds provenance and,
    %          with KeepExtraMatches, Summary.ExtraMatches always holds the extra
    %          vectors). What each format carries:
    %            OutType='table' (default) - GlobalID, RA, Dec, OriginCat, then
    %              Ind_/Nmatch_/Dist_ (and CellID_/RowInCell_ when AddPointer)
    %              per catalog, plus IndExtra_<Cat> cell columns when
    %              KeepExtraMatches=true. The fullest form.
    %            OutType='astrocatalog' - the numeric columns only (this DOES
    %              include CellID_/RowInCell_, which are numeric). The text
    %              OriginCat and the ragged IndExtra_ CANNOT live in a numeric
    %              AstroCatalog, so they are dropped from the object and read
    %              instead from Summary.OriginCat / Summary.ExtraMatches.
    %            OutFile .mat - stores XidTable as the SAME type as the returned
    %              output (so it carries whatever that type carries), plus Cats
    %              and Summary.
    %            OutFile .csv - GlobalID, RA, Dec, OriginCat, Ind_/Nmatch_/Dist_
    %              (and CellID_/RowInCell_ when AddPointer); the ragged IndExtra_
    %              columns are NOT written (not serialisable to CSV).
    %            TableToDisk .mat - GlobalID, RA, Dec, OriginCat and the
    %              Ind_/Nmatch_/Dist_ columns as separate variables, plus Summary
    %              and Cats; KeepExtraMatches and AddPointer are not supported in
    %              this mode.
    %          The differences are forced by container limits: a numeric
    %          AstroCatalog cannot hold text or ragged columns, and CSV cannot
    %          hold ragged columns.
    % Author : Dana Kovaleva (Jul 2026)
    % Example:
    %  RAD = 180./pi;   % positional RA,Dec are in RADIANS (like cone_search)
    %  % The default output T is a MATLAB table (T.Ind_PS1 etc.). Pass
    %  % 'OutType','astrocatalog' for an AstroCatalog (read with getCol).
    %
    %  % (1) defaults: field RA=254, Dec=+64 deg, R=60"=1 arcmin, 2" matching,
    %  %     anchor GAIADR3 vs ALL available catsHTM catalogs:
    %  [T, Cats, S] = VO.prep.crossIDCatsHTM;
    %
    %  % (2) EXPLICIT catalog list (only these, in this order; skips the
    %  %     'all catalogs' enumeration), field (254,64) deg over a 1 deg cone:
    %  [T, Cats, S] = VO.prep.crossIDCatsHTM(254/RAD, 64/RAD, 1, ...
    %               'RadiusUnits','deg', 'CatList',{'PS1','APASS','GALEX','TMASS'});
    %
    %  % (3) different field + anchor + per-catalog match radii, 600" cone,
    %  %     written to disk (.mat keeps Cats, .csv is the flat table):
    %  [T, Cats, S] = VO.prep.crossIDCatsHTM(180/RAD, -30/RAD, 600, ...
    %               'RefCat','GAIADR3','CatList',{'PS1','FIRST','NVSS'},...
    %               'MatchRadius',2,'RadiusPerCat',{'FIRST',5;'NVSS',5},...
    %               'OutFile','~/tmp/xid_field');
    %
    %  % (4) you may also give the field in degrees via 'CooUnits', and take
    %  %     all catalogs EXCEPT a few, dropping the orphan appending:
    %  [T, Cats, S] = VO.prep.crossIDCatsHTM(254, 64, 60, 'CooUnits','deg',...
    %               'SkipCats',{'DECaLS10','unWISE'}, 'OrphanHandling','none');
    %
    %  % (5) resolve an index (default table output): the PS1 source matched
    %  %     to global source g:
    %  g = 7;  row = T.Ind_PS1(g);
    %  if ~isnan(row), src = Cats.PS1.Catalog(row,:); end
    %
    %  % (6) LARGE field: stream each catalog to disk to keep memory low.
    %  %     Cats then holds file paths; load one to resolve indices:
    %  [T, Cats, S] = VO.prep.crossIDCatsHTM(254/RAD, 64/RAD, 10, ...
    %               'RadiusUnits','deg', 'CatsToDisk',true,'CatsDir','~/tmp/xidcats');
    %  Lp = load(Cats.PS1);  ps1 = Lp.Cat;   % AstroCatalog for PS1
    %  src = ps1.Catalog(T.Ind_PS1(g),:);
    %
    %  % (7) keep the list of additional matches when a catalog has several
    %  %     sources within the radius of one master source (default table):
    %  [T, Cats, S] = VO.prep.crossIDCatsHTM(254/RAD, 64/RAD, 360, ...
    %               'KeepExtraMatches',true);
    %  T.Ind_PS1(g)         % nearest PS1 source; T.Nmatch_PS1(g) = actual count
    %  T.IndExtra_PS1{g}    % vector of the OTHER PS1 indices (NaN if <=1 match)
    %  S.ExtraMatches.PS1{g}% same list (also the only home under 'astrocatalog')

    arguments
        RA                             = 254.*pi./180;   % [rad] default field centre
        Dec                            = 64.*pi./180;    % [rad]
        Radius                         = 360;             % [arcsec] (0.1 deg)
        Args.CooUnits                  = 'rad';
        Args.RadiusUnits               = 'arcsec';
        Args.RefCat                    = 'GAIADR3';
        Args.CatList                   = {};
        Args.SkipCats                  = {};
        Args.MatchRadius               = 2;
        Args.MatchRadiusUnits          = 'arcsec';
        Args.RadiusPerCat              = {};
        Args.OrphanHandling            = 'growing';
        Args.Con                       = {};
        Args.AddDistCol logical        = true;
        Args.KeepExtraMatches logical  = true;
        Args.AddPointer logical        = true;
        Args.OutType                   = 'table';
        Args.OutFile                   = '';
        Args.OutFileFormat             = {'mat','csv'};
        Args.CatsToDisk logical        = false;
        Args.CatsDir                   = '';
        Args.TableToDisk logical       = false;
        Args.TableFile                 = '';
        Args.Verbose logical           = true;
    end

    OrphanHandling = validatestring(Args.OrphanHandling, {'growing','append','none'});
    UseGrown  = strcmp(OrphanHandling, 'growing');
    DoAppend  = ~strcmp(OrphanHandling, 'none');
    Stream    = Args.TableToDisk;
    KeepExtra = Args.KeepExtraMatches;
    if KeepExtra && Stream
        error('crossIDCatsHTM:extraStream', ...
            'KeepExtraMatches is not supported together with TableToDisk.');
    end
    % Stable per-source pointers add a second pass of cone-cell reads, so they
    % are computed only for the in-memory path (skipped when streaming).
    AddPointer = Args.AddPointer && ~Stream;
    if Args.AddPointer && Stream && Args.Verbose
        fprintf('crossIDCatsHTM: AddPointer skipped in TableToDisk mode.\n');
    end

    % field centre in radians (cone_search convention)
    RA_rad  = convert.angular(Args.CooUnits, 'rad', RA);
    Dec_rad = convert.angular(Args.CooUnits, 'rad', Dec);
    RA_deg  = convert.angular(Args.CooUnits, 'deg', RA);
    Dec_deg = convert.angular(Args.CooUnits, 'deg', Dec);

    % resolve the list of catalogs to cross-match
    CatList = localBuildCatList(Args.RefCat, Args.CatList, Args.SkipCats, Args.Verbose);

    % where to stream per-catalog catalogs, if requested
    CatsDir = '';
    Prefix  = '';
    if Args.CatsToDisk
        [CatsDir, Prefix] = localCatsTarget(Args.CatsDir, Args.OutFile);
        if ~isfolder(CatsDir)
            mkdir(CatsDir);
        end
    end

    % where to stream the table columns, if requested
    TableFile = '';
    ColTmpDir = '';
    if Stream
        TableFile = localTableTarget(Args.TableFile, Args.OutFile);
        ColTmpDir = tempname;
        mkdir(ColTmpDir);
    end

    % ---- seed the master list with the anchor catalog --------------------
    if Args.Verbose
        fprintf('crossIDCatsHTM: anchor %s cone_search (R=%g %s)...\n', ...
            Args.RefCat, Radius, Args.RadiusUnits);
    end
    RefCatH = catsHTM.cone_search(Args.RefCat, RA_rad, Dec_rad, Radius, ...
        'RadiusUnits', Args.RadiusUnits, 'Con', Args.Con, 'OutType', 'astrocatalog');
    [seedRA, seedDec] = getLonLat(RefCatH, 'deg');
    seedRA  = seedRA(:);
    seedDec = seedDec(:);
    Nref    = numel(seedRA);
    if Nref == 0
        error('crossIDCatsHTM:emptyAnchor', ...
            'Anchor catalog %s returned no sources in the field.', Args.RefCat);
    end

    % stable storage pointers for the anchor sources (before RefCatH is freed)
    if AddPointer
        [aRA, aDec] = getLonLat(RefCatH, 'rad');
        [AnchorCID, AnchorRIC] = catsHTM.sourcePointer(Args.RefCat, aRA, aDec, ...
            'MaxDist', Args.MatchRadius, 'MaxDistUnits', Args.MatchRadiusUnits);
    end

    % seedRA/seedDec already extracted, so the anchor catalog can be streamed
    % to disk (and freed) right away in CatsToDisk mode.
    Cats = struct();
    if Args.CatsToDisk
        Cats.(Args.RefCat) = localWriteCat(CatsDir, Prefix, Args.RefCat, RefCatH, Args.Verbose);
        RefCatH = []; %#ok<NASGU>  free the anchor catalog from memory
    else
        Cats.(Args.RefCat) = RefCatH;
    end

    % master (output) list — grows as orphans are appended
    L         = Nref;
    mRA       = seedRA;
    mDec      = seedDec;
    OriginCat = repmat({Args.RefCat}, L, 1);

    % per-catalog columns: kept in memory (structs) or streamed to temp files
    % (ColFiles{k} <-> ColNames{k}) when TableToDisk=true.
    Ind       = struct();
    Nmatch    = struct();
    Dist      = struct();
    Extra     = struct();     % only used when KeepExtra (never with Stream)
    CellID    = struct();     % only used when AddPointer (never with Stream)
    RowInCell = struct();
    ColFiles  = {};
    AnchorInd  = (1:L).';
    AnchorNm   = ones(L,1);
    AnchorDist = zeros(L,1);
    if Stream
        ColFiles{1} = localWriteCol(ColTmpDir, 1, AnchorInd, AnchorNm, AnchorDist);
    else
        Ind.(Args.RefCat)    = AnchorInd;
        Nmatch.(Args.RefCat) = AnchorNm;
        Dist.(Args.RefCat)   = AnchorDist;
        if KeepExtra
            Extra.(Args.RefCat) = repmat({NaN}, L, 1);   % anchor has no extras
        end
        if AddPointer
            CellID.(Args.RefCat)    = AnchorCID;
            RowInCell.(Args.RefCat) = AnchorRIC;
        end
    end

    % per-catalog summary accumulators (anchor first)
    ColNames  = {Args.RefCat};
    StatNcone = Nref;
    StatNmat  = Nref;
    StatNorph = 0;
    StatRad   = NaN;      % anchor is not "matched" with a radius
    Failed    = {};

    % ---- cross-match every other catalog ---------------------------------
    for Icat = 1:1:numel(CatList)
        Name    = CatList{Icat};
        RadMat  = localResolveRadius(Name, Args.MatchRadius, Args.RadiusPerCat);
        Success = true;

        if Args.Verbose
            fprintf('  [%d/%d] %s (match R=%g %s)...', Icat, numel(CatList), ...
                Name, RadMat, Args.MatchRadiusUnits);
        end

        try
            CatH = catsHTM.cone_search(Name, RA_rad, Dec_rad, Radius, ...
                'RadiusUnits', Args.RadiusUnits, 'Con', Args.Con, 'OutType', 'astrocatalog');
        catch ME
            Success = false;
            Failed{end+1} = Name; %#ok<AGROW>
            if Args.Verbose
                fprintf(' FAILED (%s)\n', ME.message);
            end
        end

        if Success
            Ncone = CatH.sizeCatalog;

            % full-length columns for the current output master (length L)
            FullInd  = nan(L,1);
            FullNm   = zeros(L,1);
            FullDist = nan(L,1);
            if KeepExtra
                FullExtra = repmat({NaN}, L, 1);
            end
            Norphan  = 0;

            if Ncone > 0
                % match against the master (grown or seed-only)
                if UseGrown
                    MatchRA  = mRA;
                    MatchDec = mDec;
                else
                    MatchRA  = seedRA;
                    MatchDec = seedDec;
                end
                MasterCat          = AstroCatalog;
                MasterCat.Catalog  = [MatchRA, MatchDec];
                MasterCat.ColNames = {'RA','Dec'};
                MasterCat.ColUnits = {'deg','deg'};

                % matchReturnIndices sorts its first argument by Dec; match on
                % a sorted COPY so CatH keeps native order, and capture the
                % permutation SI (SortedRow -> native row) to remap indices.
                DecColInd       = colnameDict2ind(CatH, CatH.DefNamesDec);
                SortedCat       = CatH.copy;
                [SortedCat, SI] = sortrows(SortedCat, DecColInd);

                ML = numel(MatchRA);
                if KeepExtra
                    % all matches per master row (nearest + additional list)
                    [IndVec, NmVec, DistVec, ExtraML, OrphSorted] = ...
                        localMultiMatch(SortedCat, MasterCat, RadMat, ...
                            Args.MatchRadiusUnits, SI, Ncone, ML);
                    FullExtra(1:ML) = ExtraML;
                else
                    % Obj1 = this catalog (sorted copy), Obj2 = master, so the
                    % per-master-row fields are exactly what we need.
                    M = imProc.match.matchReturnIndices(SortedCat, MasterCat, ...
                            'CooType','sphere', 'Radius',RadMat, 'RadiusUnits',Args.MatchRadiusUnits);
                    IndSorted  = M.Obj2_IndInObj1(:);
                    IndVec     = nan(ML,1);
                    Gd         = ~isnan(IndSorted);
                    IndVec(Gd) = SI(IndSorted(Gd));    % remap to native CatH rows
                    NmVec      = M.Obj2_NmatchObj1(:); % counts are order-independent
                    DistVec    = convert.angular('rad', 'arcsec', M.Obj2_Dist(:));
                    OrphSorted = find(isnan(M.Obj1_IndInObj2(:)));
                end

                FullInd(1:ML)  = IndVec;
                FullNm(1:ML)   = NmVec;
                FullDist(1:ML) = DistVec;

                % orphans: catalog rows not matched to any master source
                OrphNative = SI(OrphSorted);   % native rows
                Norphan    = numel(OrphNative);

                if DoAppend && Norphan > 0
                    [oRA, oDec] = getLonLat(CatH, 'deg');   % native order
                    NewIdx = (L+1):(L+Norphan);
                    mRA(NewIdx,1)       = oRA(OrphNative);
                    mDec(NewIdx,1)      = oDec(OrphNative);
                    OriginCat(NewIdx,1) = {Name};
                    % grow this catalog's own columns onto the new rows
                    FullInd(NewIdx,1)  = OrphNative;
                    FullNm(NewIdx,1)   = 1;
                    FullDist(NewIdx,1) = 0;
                    if KeepExtra
                        FullExtra(NewIdx,1) = repmat({NaN}, Norphan, 1);
                    end
                    L = L + Norphan;
                end
            end

            % stable storage pointers (cell id, row-in-cell) for the sources
            % this catalog contributes; computed while CatH is still available.
            FullCellID    = nan(L,1);
            FullRowInCell = nan(L,1);
            if AddPointer && Ncone > 0
                Ref = unique(FullInd(~isnan(FullInd)));       % referenced native rows
                [srcRA, srcDec] = getLonLat(CatH, 'rad');
                [CidRef, RicRef] = catsHTM.sourcePointer(Name, srcRA(Ref), srcDec(Ref), ...
                    'MaxDist', RadMat, 'MaxDistUnits', Args.MatchRadiusUnits);
                CidMap      = nan(Ncone,1);
                RicMap      = nan(Ncone,1);
                CidMap(Ref) = CidRef;
                RicMap(Ref) = RicRef;
                Gp                = ~isnan(FullInd);
                FullCellID(Gp)    = CidMap(FullInd(Gp));
                FullRowInCell(Gp) = RicMap(FullInd(Gp));
            end

            % CatH is no longer needed: keep it in the native-order Cats struct
            % (in memory), or stream it to disk and free it. Either way
            % Ind_<Cat> indexes this native-order catalog.
            if Args.CatsToDisk
                Cats.(Name) = localWriteCat(CatsDir, Prefix, Name, CatH, Args.Verbose);
                CatH = [];
            else
                Cats.(Name) = CatH;
            end

            if Stream
                ColFiles{end+1} = localWriteCol(ColTmpDir, numel(ColNames)+1, ...
                    FullInd, FullNm, FullDist); %#ok<AGROW>
            else
                Ind.(Name)    = FullInd;
                Nmatch.(Name) = FullNm;
                Dist.(Name)   = FullDist;
                if KeepExtra
                    Extra.(Name) = FullExtra;
                end
                if AddPointer
                    CellID.(Name)    = FullCellID;
                    RowInCell.(Name) = FullRowInCell;
                end
            end

            ColNames{end+1}  = Name;         %#ok<AGROW>
            StatNcone(end+1) = Ncone;        %#ok<AGROW>
            % Nmatched = cone sources that matched a master source, so that
            % Nmatched + Norphan = Ncone (both are catalog-source counts).
            StatNmat(end+1)  = Ncone - Norphan; %#ok<AGROW>
            StatNorph(end+1) = Norphan;      %#ok<AGROW>
            StatRad(end+1)   = convert.angular(Args.MatchRadiusUnits,'arcsec',RadMat); %#ok<AGROW>

            if Args.Verbose
                fprintf(' %d src, %d matched, %d orphan\n', Ncone, StatNmat(end), Norphan);
            end
        end
    end

    Nglobal = L;

    % ---- summary ---------------------------------------------------------
    Summary = struct();
    Summary.Field     = struct('RA',RA_deg,'Dec',Dec_deg,'Radius',Radius,'RadiusUnits',Args.RadiusUnits);
    Summary.RefCat    = Args.RefCat;
    Summary.Nref      = Nref;
    Summary.Nglobal   = Nglobal;
    Summary.OriginCat = OriginCat;
    Summary.Failed    = Failed;
    Summary.PerCat    = table(ColNames(:), StatNcone(:), StatNmat(:), StatNorph(:), StatRad(:), ...
        'VariableNames', {'Catalog','Ncone','Nmatched','Norphan','MatchRadiusArcsec'});

    % additional (non-nearest) matches per master row, as an Nglobal-by-1 cell
    % per catalog (NaN entries where there are 0 or 1 matches).
    if KeepExtra
        Summary.ExtraMatches = struct();
        for Icol = 1:1:numel(ColNames)
            Summary.ExtraMatches.(ColNames{Icol}) = localPadCell(Extra.(ColNames{Icol}), Nglobal);
        end
    end

    if Stream
        % ---- stream the table to a v7.3 .mat, one column at a time -------
        localAssembleTableFile(TableFile, ColNames, ColFiles, Nglobal, ...
            mRA, mDec, OriginCat, Args.AddDistCol, Summary, Cats, Args.Verbose);
        if isfolder(ColTmpDir)
            rmdir(ColTmpDir, 's');
        end
        XidTable = TableFile;
    else
        % ---- assemble the output table (in memory) ----------------------
        % pad every catalog column to the final global length
        VarData = {(1:Nglobal).', mRA, mDec, OriginCat};
        VarName = {'GlobalID','RA','Dec','OriginCat'};
        for Icol = 1:1:numel(ColNames)
            Name = ColNames{Icol};
            VarData{end+1} = localPad(Ind.(Name),    Nglobal, NaN); %#ok<AGROW>
            VarName{end+1} = ['Ind_' Name];                          %#ok<AGROW>
            VarData{end+1} = localPad(Nmatch.(Name), Nglobal, 0);    %#ok<AGROW>
            VarName{end+1} = ['Nmatch_' Name];                       %#ok<AGROW>
            if Args.AddDistCol
                VarData{end+1} = localPad(Dist.(Name), Nglobal, NaN); %#ok<AGROW>
                VarName{end+1} = ['Dist_' Name];                      %#ok<AGROW>
            end
            if AddPointer
                VarData{end+1} = localPad(CellID.(Name),    Nglobal, NaN); %#ok<AGROW>
                VarName{end+1} = ['CellID_' Name];                         %#ok<AGROW>
                VarData{end+1} = localPad(RowInCell.(Name), Nglobal, NaN); %#ok<AGROW>
                VarName{end+1} = ['RowInCell_' Name];                      %#ok<AGROW>
            end
        end
        TableForm = table(VarData{:}, 'VariableNames', VarName);

        % ---- select return type -----------------------------------------
        if strcmpi(Args.OutType, 'astrocatalog')
            % numeric-only AstroCatalog (text OriginCat -> Summary.OriginCat)
            Numeric = TableForm;
            Numeric.OriginCat = [];
            AC          = AstroCatalog;
            AC.Catalog  = table2array(Numeric);
            AC.ColNames = Numeric.Properties.VariableNames;
            XidTable    = AC;
        else
            XidTable = TableForm;
            % add the additional-matches cell columns (table output only)
            if KeepExtra
                for Icol = 1:1:numel(ColNames)
                    XidTable.(['IndExtra_' ColNames{Icol}]) = ...
                        Summary.ExtraMatches.(ColNames{Icol});
                end
            end
        end

        % ---- optional file output ---------------------------------------
        % .mat stores the SAME object as the return value (AstroCatalog by
        % default); .csv is always built from the full table so it keeps the
        % OriginCat column. Summary (incl. Summary.OriginCat) is saved too.
        if ~isempty(Args.OutFile)
            localWriteOut(Args.OutFile, Args.OutFileFormat, XidTable, TableForm, ...
                Cats, Summary, Args.Verbose);
        end
    end
end

% ======================================================================
function CatList = localBuildCatList(RefCat, UserList, SkipCats, Verbose)
    % Resolve the list of catalogs to cross-match against the anchor.
    Data      = catsHTM.catalogs;
    AvailName = {Data([Data.Status]).Name};

    if isempty(UserList)
        CatList = AvailName;
    else
        % explicit list is trusted (may be on-path catalogs not in the
        % registry); warn about unregistered names but keep them.
        if ischar(UserList) || isstring(UserList)
            UserList = cellstr(UserList);
        end
        Known = ismember(UserList, AvailName);
        if Verbose && any(~Known)
            fprintf('crossIDCatsHTM: catalog(s) not in catsHTM.catalogs registry (kept, resolved by path): %s\n', ...
                strjoin(UserList(~Known), ', '));
        end
        CatList = UserList;
    end

    % remove the anchor and any explicitly skipped catalogs
    Drop    = [{RefCat}, cellstr(SkipCats).'];
    CatList = CatList(~ismember(CatList, Drop));
    CatList = CatList(:).';
end

% ======================================================================
function Rad = localResolveRadius(Name, DefRad, Pairs)
    % Per-catalog matching radius, falling back to the default.
    Rad = DefRad;
    if ~isempty(Pairs)
        Idx = find(strcmp(Pairs(:,1), Name), 1);
        if ~isempty(Idx)
            Rad = Pairs{Idx, 2};
        end
    end
end

% ======================================================================
function V = localPad(V, N, FillVal)
    % Pad a column vector to length N with FillVal.
    V = V(:);
    if numel(V) < N
        V(numel(V)+1:N, 1) = FillVal;
    end
end

% ======================================================================
function C = localPadCell(C, N)
    % Pad a cell column to length N with {NaN}.
    C = C(:);
    if numel(C) < N
        C(numel(C)+1:N, 1) = repmat({NaN}, N-numel(C), 1);
    end
end

% ======================================================================
function [IndVec, NmVec, DistVec, ExtraML, OrphSorted] = ...
        localMultiMatch(SortedCat, MasterCat, RadMat, RadUnits, SI, Ncone, ML)
    % Per master row, return the nearest match plus the list of additional
    % (non-nearest) matches. Indices are remapped SortedCat -> native via SI.
    % IndVec/NmVec/DistVec are ML-by-1 (nearest, count, nearest dist [arcsec]);
    % ExtraML is an ML-by-1 cell (native index row vector, or NaN for <=1);
    % OrphSorted are SortedCat rows matched to no master source.
    RM = imProc.match.matchReturnIndicesMulti(SortedCat, MasterCat, ...
            'CooType','sphere', 'Radius',RadMat, 'RadiusUnits',RadUnits);
    MM = RM(1).Ind;                       % struct array, one per master row

    IndVec  = nan(ML,1);
    NmVec   = zeros(ML,1);
    DistVec = nan(ML,1);
    ExtraML = repmat({NaN}, ML, 1);
    Matched = false(Ncone,1);
    for Ig = 1:1:ML
        Ids = MM(Ig).Ind(:);
        if ~isempty(Ids)
            Matched(Ids)  = true;
            [Dsort, Ord]  = sort(MM(Ig).Dist(:));   % nearest first
            IdsN          = SI(Ids(Ord));           % native indices
            IndVec(Ig)    = IdsN(1);
            NmVec(Ig)     = numel(Ids);
            DistVec(Ig)   = convert.angular('rad', 'arcsec', Dsort(1));
            if numel(IdsN) > 1
                ExtraML{Ig} = IdsN(2:end).';        % additional native indices
            end
        end
    end
    OrphSorted = find(~Matched);
end

% ======================================================================
function [Dir, Prefix] = localCatsTarget(CatsDir, OutFile)
    % Resolve the directory and filename prefix for streamed catalog files.
    if isempty(CatsDir)
        if isempty(OutFile)
            Dir = pwd;
        else
            Dir = fileparts(OutFile);
            if isempty(Dir)
                Dir = pwd;
            end
        end
    else
        Dir = CatsDir;
    end
    if isempty(OutFile)
        Prefix = 'xidCats';
    else
        [~, Prefix] = fileparts(OutFile);
    end
end

% ======================================================================
function F = localTableTarget(TableFile, OutFile)
    % Resolve the .mat path for the streamed cross-id table.
    if ~isempty(TableFile)
        F = TableFile;
    elseif ~isempty(OutFile)
        [P, B] = fileparts(OutFile);
        if isempty(P)
            P = pwd;
        end
        F = fullfile(P, [B '_xidtable.mat']);
    else
        F = fullfile(pwd, 'xidTable.mat');
    end
    [~, ~, E] = fileparts(F);
    if ~strcmpi(E, '.mat')
        F = [F '.mat'];
    end
end

% ======================================================================
function P = localWriteCol(Dir, Idx, Ind, Nm, Dist)
    % Stream one catalog's finalized (pre-pad) columns to a temp .mat.
    P          = fullfile(Dir, sprintf('col_%05d.mat', Idx));
    Chunk.Ind  = Ind;
    Chunk.Nm   = Nm;
    Chunk.Dist = Dist;
    save(P, '-struct', 'Chunk', '-v7.3');
end

% ======================================================================
function localAssembleTableFile(TableFile, ColNames, ColFiles, Nglobal, ...
        mRA, mDec, OriginCat, AddDistCol, Summary, Cats, Verbose)
    % Write the cross-id table to a v7.3 .mat one column at a time via
    % matfile, so the full wide table is never held in memory. Variables:
    % GlobalID, RA, Dec, OriginCat, Ind_<Cat>, Nmatch_<Cat>[, Dist_<Cat>],
    % plus Summary and Cats.
    if isfile(TableFile)
        delete(TableFile);
    end
    Mf           = matfile(TableFile, 'Writable', true);
    Mf.GlobalID  = (1:Nglobal).';
    Mf.RA        = mRA;
    Mf.Dec       = mDec;
    Mf.OriginCat = OriginCat;
    for Icol = 1:1:numel(ColNames)
        Chunk = load(ColFiles{Icol});
        Mf.(['Ind_' ColNames{Icol}])    = localPad(Chunk.Ind, Nglobal, NaN);
        Mf.(['Nmatch_' ColNames{Icol}]) = localPad(Chunk.Nm,  Nglobal, 0);
        if AddDistCol
            Mf.(['Dist_' ColNames{Icol}]) = localPad(Chunk.Dist, Nglobal, NaN);
        end
    end
    Mf.Summary = Summary;
    Mf.Cats    = Cats;
    if Verbose
        fprintf('crossIDCatsHTM: wrote streamed table %s (%d rows, %d catalogs)\n', ...
            TableFile, Nglobal, numel(ColNames));
    end
end

% ======================================================================
function Path = localWriteCat(Dir, Prefix, Name, Cat, Verbose)
    % Write one cone_search catalog to its own .mat file; return the path.
    % Loaded back as: L = load(Path); L.Cat  (the AstroCatalog).
    Path       = fullfile(Dir, sprintf('%s_%s.mat', Prefix, Name));
    Payload.Cat = Cat;
    save(Path, '-struct', 'Payload', '-v7.3');
    if Verbose
        fprintf('crossIDCatsHTM: wrote %s\n', Path);
    end
end

% ======================================================================
function localWriteOut(OutFile, Formats, XidTable, TableForm, Cats, Summary, Verbose)
    % Write the cross-id results to disk (.mat and/or .csv).
    %   XidTable  - the object to store in the .mat (matches the return type:
    %               AstroCatalog or table).
    %   TableForm - the full MATLAB table (with OriginCat) used for the .csv.
    if ischar(Formats) || isstring(Formats)
        Formats = cellstr(Formats);
    end
    % strip any extension the user supplied; both files share the stem
    [Path, Base] = fileparts(OutFile);
    if isempty(Path)
        Path = pwd;
    end
    Stem = fullfile(Path, Base);

    if any(strcmpi(Formats, 'mat'))
        MatFile = [Stem '.mat'];
        save(MatFile, 'XidTable', 'Cats', 'Summary', '-v7.3');
        if Verbose
            fprintf('crossIDCatsHTM: wrote %s\n', MatFile);
        end
    end
    if any(strcmpi(Formats, 'csv'))
        CsvFile = [Stem '.csv'];
        writetable(TableForm, CsvFile);
        if Verbose
            fprintf('crossIDCatsHTM: wrote %s\n', CsvFile);
        end
    end
end
