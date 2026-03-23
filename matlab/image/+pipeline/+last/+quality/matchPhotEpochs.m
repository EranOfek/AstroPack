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
    %            'OutDir'    - Directory for cached MS_all.mat. Default is '' (no caching).
    %            'ForceRecalc' - Recompute even if cached. Default is false.
    %            'Verbose'   - Print progress. Default is true.
    % Output : - MS struct with MS.(mode){crop} = MatchedSources.
    % Author : D. Kovaleva (Mar 2026)
    % Example: MS = pipeline.last.quality.matchPhotEpochs(Calib.Cats, ...
    %              'Modes', {'percrop','refzp'});

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
        if Args.Verbose
            fprintf('Loading cached %s\n', MSFile);
        end
        S = load(MSFile, 'MS_all');
        MS = S.MS_all;
        return;
    end

    if Args.Verbose
        fprintf('\n=== Epoch Matching ===\n');
    end

    MS = struct();
    Nmodes = numel(Args.Modes);

    for Im = 1:Nmodes
        Mode = Args.Modes{Im};
        Nvisits = numel(Cats.(Mode));

        for Ic = Args.CropsToAnalyze
            % Collect catalogs for this crop across epochs
            CatList = AstroCatalog.empty(0, Nvisits);
            ValidEpochs = false(Nvisits, 1);

            for Iv = 1:Nvisits
                if isempty(Cats.(Mode){Iv})
                    continue;
                end
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

            % Match across epochs
            MSobj = MatchedSources;
            MSobj = MSobj.unifiedCatalogsIntoMatched(CatList(ValidEpochs).', ...
                'MatchedColums', Args.MatchedColumns, ...
                'Radius', Args.MatchRadius, 'RadiusUnits', 'arcsec');

            % Flag bad photometry
            MSobj = MSobj.setBadPhotToNan('BadFlags', Args.BadFlags, ...
                'MagField', 'MAG_PSF', 'CreateNewObj', false);

            % Apply relative ZP correction to original (non-AB) mag fields
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

            MS.(Mode){Ic} = MSobj;

            if Args.Verbose
                fprintf('  %s crop %02d: %d matched sources\n', ...
                    Mode, Ic, MSobj.Nsrc);
            end
        end
    end

    if ~isempty(MSFile)
        MS_all = MS;
        save(MSFile, 'MS_all');
        if Args.Verbose
            fprintf('Saved %s\n', MSFile);
        end
    end
end
