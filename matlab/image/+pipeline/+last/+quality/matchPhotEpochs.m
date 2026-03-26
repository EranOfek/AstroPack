function MS = matchPhotEpochs(Cats, Args)
    % Match sources across epochs per crop per mode into MatchedSources
    % Description: For each mode and crop, collects catalogs across epochs,
    %              matches sources by position, flags bad photometry, and
    %              applies relative ZP correction to instrumental magnitudes.
    %              Results are cached in OutDir as MS_all.mat.
    %
    % Input  : - Cats struct with Cats.(mode){Iv}(Ic) AstroCatalog arrays
    %            (from calibratePhotModes .Cats output).
    %          * ...,key,val,...
    %            'Modes'     - Cell array of modes to match. Required.
    %            'CropsToAnalyze' - Crop indices to process. Default is [] (all).
    %            'Ncrop'     - Number of crops. Default is 24.
    %            'MatchRadius' - Matching radius [arcsec]. Default is 3.
    %            'MatchedColumns' - Columns to propagate into MatchedSources.
    %            'MagFields' - AB magnitude columns. Default is {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'BadFlags'  - Flags for setBadPhotToNan. Default is {'Saturated','NearEdge','Overlap'}.
    %            'MaxMagErr' - Max mag error for ZP correction. Default is 0.02.
    %            'MinEpochs' - Min non-NaN epochs per source; 0 = no filter. Default is 0.
    %            'OutDir'    - Directory for cached MS_all.mat. Default is '' (no caching).
    %            'ForceRecalc' - Recompute even if cached. Default is false.
    %            'Verbose'   - Print progress. Default is true.
    % Output : - MS struct with MS.(mode){crop} = MatchedSources.
    % Author : D. Kovaleva (Mar 2026)
    % Example: MS = pipeline.last.quality.matchPhotEpochs(Calib.Cats, ...
    %              'Modes', {'percrop','perimage'});

    arguments
        Cats struct
        Args.Modes cell
        Args.CropsToAnalyze = []
        Args.Ncrop          = 24
        Args.MatchRadius    = 3
        Args.MatchedColumns = {'RA','Dec','X1','Y1','SN', ...
                               'MAG_AB_PSF','MAG_AB_APER_3', ...
                               'MAG_PSF','MAG_APER_3', ...
                               'MAGERR_PSF','MAGERR_APER_3','FLAGS'}
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.BadFlags       = {'Saturated','NearEdge','Overlap'}
        Args.MaxMagErr      = 0.02
        Args.MinEpochs      = 0
        Args.ApplyRelZP logical = true  % Apply zp_meddiff to original (non-AB) mags
        Args.OutDir         = ''
        Args.ForceRecalc logical = false
        Args.Verbose logical = true
    end

    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end

    MSFile = '';
    if ~isempty(Args.OutDir)
        MSFile = fullfile(Args.OutDir, 'MS_all.mat');
    end

    if ~isempty(MSFile) && exist(MSFile, 'file') && ~Args.ForceRecalc
        try
            S = load(MSFile, 'MS_all');
            MissingModes = setdiff(Args.Modes, fieldnames(S.MS_all));
            if isempty(MissingModes)
                if Args.Verbose
                    fprintf('Loaded cached %s\n', MSFile);
                end
                MS = S.MS_all;
                return;
            else
                if Args.Verbose
                    fprintf('Cache %s missing modes: %s — rematching\n', ...
                        MSFile, strjoin(MissingModes, ', '));
                end
            end
        catch ME
            if Args.Verbose
                fprintf('Cache %s corrupt (%s) — rematching\n', MSFile, ME.message);
            end
        end
    end

    if Args.Verbose
        fprintf('\n=== Epoch Matching ===\n');
    end

    MS = struct();
    Nmodes = numel(Args.Modes);

    for Im = 1:Nmodes
        Mode = Args.Modes{Im};
        if ~isfield(Cats, Mode)
            if Args.Verbose
                fprintf('  Mode %s: no catalogs, skipping\n', Mode);
            end
            MS.(Mode) = cell(1, Args.Ncrop);
            continue;
        end
        Nvisits = numel(Cats.(Mode));
        MS.(Mode) = cell(1, Args.Ncrop);

        for Ic = Args.CropsToAnalyze
            CatList = AstroCatalog.empty(0, Nvisits);
            ValidEpochs = false(Nvisits, 1);

            for Iv = 1:Nvisits
                if isempty(Cats.(Mode){Iv}); continue; end
                if Ic <= numel(Cats.(Mode){Iv})
                    CatList(Iv) = Cats.(Mode){Iv}(Ic);
                    ValidEpochs(Iv) = true;
                end
            end

            if sum(ValidEpochs) < 3
                if Args.Verbose
                    fprintf('  %s crop %d: <3 valid epochs, skipping\n', Mode, Ic);
                end
                continue;
            end

            MSobj = MatchedSources;
            MSobj = MSobj.unifiedCatalogsIntoMatched(CatList(ValidEpochs).', ...
                'MatchedColums', Args.MatchedColumns, ...
                'Radius', Args.MatchRadius, 'RadiusUnits', 'arcsec');

            % Flag bad photometry
            MSobj = MSobj.setBadPhotToNan('BadFlags', Args.BadFlags, ...
                'MagField', 'MAG_PSF', 'CreateNewObj', false);

            % Apply relative ZP correction to original (non-AB) mag fields
            if Args.ApplyRelZP
                for Imf = 1:numel(Args.MagFields)
                    OrigField = strrep(Args.MagFields{Imf}, '_AB_', '_');
                    if strcmp(OrigField, Args.MagFields{Imf}); continue; end
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

            % Filter sources with too few valid epochs
            if Args.MinEpochs > 0
                RefField = Args.MagFields{1};
                if isfield(MSobj.Data, RefField)
                    Nvalid = sum(~isnan(MSobj.Data.(RefField)), 1);
                    BadSrc = Nvalid < Args.MinEpochs;
                    Flds = fieldnames(MSobj.Data);
                    for Ifl = 1:numel(Flds)
                        MSobj.Data.(Flds{Ifl})(:, BadSrc) = NaN;
                    end
                end
            end

            MS.(Mode){Ic} = MSobj;

            if Args.Verbose
                Ngood = 0;
                RefField = Args.MagFields{1};
                if isfield(MSobj.Data, RefField)
                    Ngood = sum(any(~isnan(MSobj.Data.(RefField)), 1));
                end
                fprintf('  %s crop %02d: %d matched sources (%d with >=%d epochs)\n', ...
                    Mode, Ic, MSobj.Nsrc, Ngood, max(Args.MinEpochs, 1));
            end
        end
    end

    if ~isempty(MSFile)
        MS_all = MS;
        try
            save(MSFile, 'MS_all', '-v7.3');
            if Args.Verbose
                fprintf('Saved %s\n', MSFile);
            end
        catch ME
            warning('matchPhotEpochs:SaveFailed', ...
                'Failed to save %s: %s', MSFile, ME.message);
        end
    end
end
