function MS = matchEpochs(Input, Args)
    % Cross-match catalogs across epochs into a flat per-crop MatchedSources.
    % Description: For each crop, unifies the catalogs of all epochs into a
    %              MatchedSources object via imProc.match.unify, flags bad
    %              photometry to NaN, optionally applies a relative
    %              zero-point correction, and purges sources observed in too
    %              few epochs. Replaces the mode-keyed matchPhotEpochs and
    %              the visit-path wrapper matchVisitEpochs; the legacy
    %              calibration-mode layer is gone, so the result is a flat
    %              1xNcrop MatchedSources array (one per crop).
    %
    %              Input forms:
    %                (1) char/string visit directory - loadVisit reads the
    %                    individual per-sub-exposure catalogs;
    %                (2) cell of per-epoch AstroCatalog or AstroImage arrays
    %                    (each a 1xNcrop array);
    %                (3) a single AstroCatalog/AstroImage array (one epoch).
    %
    % Input  : - Input - visit-directory path, cell of per-epoch arrays, or
    %            one AstroCatalog/AstroImage array.
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices to match. Default [] (all).
    %            'Ncrop'          - Crop count. Default [] (inferred).
    %            'MatchRadius'    - Cross-match radius [arcsec]. Default 2.
    %            'MatchedColumns' - Columns propagated into MS.Data. Default
    %                               is the proc-catalog set including
    %                               FLUX_PSF; MAG_PSF must remain (the
    %                               bad-photometry flagging keys on it).
    %            'MagFields'      - Magnitude fields; the first is the
    %                               reference for the MinEpochs purge.
    %                               Default {'MAG_PSF','MAG_APER_3'}.
    %            'BadFlags'       - FLAGS bits NaN'd by setBadPhotToNan.
    %                               Default {'Saturated','NearEdge','Overlap'}.
    %            'MinEpochs'      - Minimum finite epochs per source; sources
    %                               below it are NaN'd out, and a crop with
    %                               fewer valid epochs is skipped. Default 1.
    %            'ApplyRelZP'     - Apply lcUtil.zp_meddiff relative-ZP
    %                               correction to MagFields. Default false.
    %            'MaxMagErr'      - Max mag error for the ZP correction.
    %                               Default 0.02.
    %            'PipelineVer'    - 'v0' | 'v1', forwarded to loadVisit for
    %                               the path input. Default 'v0'.
    %            'OutDir'         - Directory for the cached MS_all.mat.
    %                               Default '' (no caching).
    %            'ForceRecalc'    - Recompute even if a cache exists.
    %                               Default false.
    %            'Verbose'        - Print per-crop progress. Default false.
    % Output : - MS - 1xNcrop MatchedSources array (empty MatchedSources for
    %            crops that were skipped or failed to match).
    % Author : photCalib package refactor (2026-05)
    % Example: MS = pipeline.last.quality.photCalib.matchEpochs( ...
    %              '/data/2025/05/04/proc/000838v0');
    %          MS = pipeline.last.quality.photCalib.matchEpochs(R.Cats);

    arguments
        Input
        Args.CropsToAnalyze double = []
        Args.Ncrop                 = []
        Args.MatchRadius    (1,1) double {mustBePositive} = 2
        Args.MatchedColumns cell = {'RA','Dec','X1','Y1','SN', ...
                                    'MAG_PSF','MAG_APER_3', ...
                                    'MAGERR_PSF','MAGERR_APER_3', ...
                                    'FLUX_APER_3','FLUX_PSF','FLAGS'}
        Args.MagFields      cell = {'MAG_PSF','MAG_APER_3'}
        Args.BadFlags       cell = {'Saturated','NearEdge','Overlap'}
        Args.MinEpochs      (1,1) double {mustBeNonnegative} = 1
        Args.ApplyRelZP     logical = false
        Args.MaxMagErr      (1,1) double = 0.02
        Args.PipelineVer    (1,:) char {mustBeMember(Args.PipelineVer,{'v0','v1'})} = 'v0'
        Args.OutDir         {mustBeText} = ''
        Args.ForceRecalc    logical = false
        Args.Verbose        logical = false
    end

    % --- Cache load ----------------------------------------------------
    MSFile = '';
    if ~isempty(Args.OutDir)
        MSFile = fullfile(char(Args.OutDir), 'MS_all.mat');
    end
    if ~isempty(MSFile) && exist(MSFile,'file') && ~Args.ForceRecalc
        try
            S = load(MSFile, 'MS');
            if isfield(S,'MS') && isa(S.MS,'MatchedSources')
                MS = S.MS;
                if Args.Verbose
                    fprintf('matchEpochs: loaded cached %s\n', MSFile);
                end
                return;
            end
        catch ME
            if Args.Verbose
                fprintf('matchEpochs: cache %s unreadable (%s) - rematching\n', ...
                    MSFile, ME.message);
            end
        end
    end

    % --- Resolve input to Cats: a cell over epochs of AstroCatalog arrays
    if ischar(Input) || (isstring(Input) && isscalar(Input))
        VisitPath = char(Input);
        if ~isfolder(VisitPath)
            error('photCalib:matchEpochs:NoDir', ...
                'Visit directory not found: %s', VisitPath);
        end
        AllSI = pipeline.last.load.loadVisit(VisitPath, ...
            'TempName_Coadd', '', 'TempName_MS', '', ...
            'PipelineVer', Args.PipelineVer, 'GenError', true);
        if isempty(AllSI)
            error('photCalib:matchEpochs:NoImages', ...
                'No individual proc images found under %s', VisitPath);
        end
        Nepoch = size(AllSI, 1);
        Cats = cell(1, Nepoch);
        for Ie = 1:Nepoch
            Cats{Ie} = [AllSI(Ie,:).CatData];
        end
    else
        [Cell, ~] = resolveInput(Input);
        if isempty(Cell)
            error('photCalib:matchEpochs:BadInput', ...
                ['Input must be a visit-directory path, a cell of ', ...
                 'per-epoch AstroCatalog/AstroImage arrays, or such an array.']);
        end
        Nepoch = numel(Cell);
        Cats = cell(1, Nepoch);
        for Ie = 1:Nepoch
            El = Cell{Ie};
            if isa(El, 'AstroImage')
                Cats{Ie} = [El.CatData];
            elseif isa(El, 'AstroCatalog')
                Cats{Ie} = El;
            else
                Cats{Ie} = AstroCatalog.empty;
            end
        end
    end

    % --- Crop count ----------------------------------------------------
    if isempty(Args.Ncrop)
        Ncrop = 0;
        for Ie = 1:Nepoch
            if ~isempty(Cats{Ie}); Ncrop = max(Ncrop, numel(Cats{Ie})); end
        end
    else
        Ncrop = Args.Ncrop;
    end
    if Ncrop < 1
        error('photCalib:matchEpochs:NoCrops', 'No catalogs found to match.');
    end

    Crops = Args.CropsToAnalyze;
    if isempty(Crops); Crops = 1:Ncrop; end
    NeedEpochs = max(Args.MinEpochs, 1);

    % --- Per-crop cross-matching ---------------------------------------
    MS(1, Ncrop) = MatchedSources;

    for Ic = Crops
        if Ic < 1 || Ic > Ncrop; continue; end

        CatList     = AstroCatalog.empty(0, Nepoch);
        ValidEpochs = false(Nepoch, 1);
        for Ie = 1:Nepoch
            if isempty(Cats{Ie}); continue; end
            if Ic <= numel(Cats{Ie})
                Cat = Cats{Ie}(Ic);
                try
                    [Nr, ~] = Cat.sizeCatalog;
                    if Nr > 0
                        CatList(Ie)     = Cat;
                        ValidEpochs(Ie) = true;
                    end
                catch
                    % Catalog could not be queried - treat epoch as missing.
                end
            end
        end

        if sum(ValidEpochs) < NeedEpochs
            if Args.Verbose
                fprintf('  crop %d: %d valid epoch(s) (<%d), skipping\n', ...
                    Ic, sum(ValidEpochs), NeedEpochs);
            end
            continue;
        end

        try
            [~, ~, MSobj] = imProc.match.unify(CatList(ValidEpochs).', ...
                'Col',              Args.MatchedColumns, ...
                'MatchRadius',      Args.MatchRadius, ...
                'MatchRadiusUnits', 'arcsec');
        catch ME
            if Args.Verbose
                fprintf('  crop %d: matching failed (%s), skipping\n', ...
                    Ic, ME.message);
            end
            continue;
        end

        % Flag bad photometry to NaN
        MSobj = MSobj.setBadPhotToNan('BadFlags', Args.BadFlags, ...
            'MagField', 'MAG_PSF', 'CreateNewObj', false);

        % Optional relative zero-point correction
        if Args.ApplyRelZP
            for Imf = 1:numel(Args.MagFields)
                OrigField = Args.MagFields{Imf};
                if ~isfield(MSobj.Data, OrigField); continue; end
                ErrField = strrep(OrigField, 'MAG_', 'MAGERR_');
                if isfield(MSobj.Data, ErrField)
                    Rzp = lcUtil.zp_meddiff(MSobj, 'MagField', {OrigField}, ...
                        'MagErrField', {ErrField}, 'MaxMagErr', Args.MaxMagErr);
                else
                    Rzp = lcUtil.zp_meddiff(MSobj, 'MagField', {OrigField});
                end
                MSobj = MSobj.applyZP(Rzp, 'ApplyToMagField', {OrigField});
            end
        end

        % Purge sources observed in too few epochs
        if Args.MinEpochs > 0
            RefField = Args.MagFields{1};
            if isfield(MSobj.Data, RefField)
                Nvalid = sum(~isnan(MSobj.Data.(RefField)), 1);
                BadSrc = Nvalid < Args.MinEpochs;
                Flds   = fieldnames(MSobj.Data);
                for Ifl = 1:numel(Flds)
                    MSobj.Data.(Flds{Ifl})(:, BadSrc) = NaN;
                end
            end
        end

        MS(Ic) = MSobj;

        if Args.Verbose
            fprintf('  crop %02d: %d matched sources, %d epoch(s)\n', ...
                Ic, MSobj.Nsrc, sum(ValidEpochs));
        end
    end

    % --- Cache save ----------------------------------------------------
    if ~isempty(MSFile)
        if ~exist(char(Args.OutDir),'dir'); mkdir(char(Args.OutDir)); end
        saveResult(MSFile, MS, 'VarName', 'MS', 'Verbose', Args.Verbose);
    end
end
