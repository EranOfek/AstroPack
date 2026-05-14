function Result = batchPhotStability(BaseDir, Args)
    % Source-level stability across many visits, no recalibration
    % Description: Mirror of batchPhotCalib's gathering API
    %              (VisitGlob, Recursive, FileType, FieldId, CropID), but
    %              instead of fitting PhotCalibTrans, this function reads
    %              the already-calibrated catalogs in each visit,
    %              cross-matches sources by RA/Dec across visits with
    %              matchPhotEpochs, and renders the per-source Std-vs-
    %              Median scatter via plotPhotScatter. Use this when the
    %              AB-magnitude columns are already present in the
    %              catalogs and only the source-level stability check is
    %              needed.
    %
    %              Pipeline:
    %                1. dir-walk visit subdirectories under BaseDir,
    %                2. loadVisitCatHdr (Cat + Header only; FieldId /
    %                   CropID filters applied),
    %                3. Repackage AIvis catalogs as Cats.percrop{Iv}(Ic),
    %                4. matchPhotEpochs -> MS.(mode){crop} MatchedSources,
    %                5. plotPhotScatter (3-group default: RA/Dec [arcsec
    %                   with cos(Dec)], MAG_PSF/MAG_APER_3, AB versions).
    %
    % Input  : - BaseDir char/string - parent directory containing visit
    %            subdirectories (e.g. /data/.../2025/06/01/proc).
    %                    * ...,key,val,...
    %                      'VisitGlob'      - Pattern for visit dirs.
    %                                         Default '*v*'.
    %                      'Recursive'      - Walk at any depth under
    %                                         BaseDir. Default false.
    %                      'FileType'       - 'coadd' (default) | 'proc'.
    %                      'FieldId'        - AstroFileName.FieldID
    %                                         filter (e.g. '1781').
    %                                         Default '' (no filter).
    %                      'CropID'         - integer crop slot filter
    %                                         (1..Ncrop). Default []
    %                                         (no filter).
    %                      'ExcludeVisits'  - Cell / string array of
    %                                         visit dir basenames (or
    %                                         full paths) to drop from
    %                                         the discovered list.
    %                                         Default {} (drop nothing).
    %                                         Useful when one visit has
    %                                         a schema mismatch that
    %                                         breaks matchPhotEpochs.
    %                      'Ncrop'          - Crops per visit. Default 24.
    %                      'CropsToAnalyze' - Vector of crop indices to
    %                                         pass to matchPhotEpochs /
    %                                         plotPhotScatter. Default
    %                                         [] (all crops).
    %                      'MatchRadius'    - Cross-match radius
    %                                         [arcsec]. Default 3.
    %                      'MagFields'      - Magnitude column names
    %                                         used by matchPhotEpochs.
    %                                         IMPORTANT: the FIRST entry
    %                                         is the reference for the
    %                                         source-level MinEpochs
    %                                         purge — if that field is
    %                                         all-NaN (e.g. MAG_AB_* on
    %                                         visits without AB
    %                                         calibration), every source
    %                                         is dropped and every field
    %                                         is wiped to NaN. Default
    %                                         {'MAG_PSF','MAG_APER_3'}
    %                                         (always present).
    %                      'MatchedColumns' - Columns to propagate into
    %                                         MS. Default includes RA,
    %                                         Dec, X1, Y1, SN, MAG_*,
    %                                         MAGERR_*, FLUX_APER_3,
    %                                         FLUX_PSF, FLAGS.
    %                      'FluxFields'     - Optional cell of flux
    %                                         column names (e.g.
    %                                         {'FLUX_APER_3','FLUX_PSF'}).
    %                                         When non-empty: (1) these
    %                                         columns are added to
    %                                         MatchedColumns so the
    %                                         flux values are stored in
    %                                         MS, and (2) plotPhotScatter
    %                                         is called a SECOND time
    %                                         with these as a single
    %                                         QuantityGroup, producing
    %                                         a dedicated Std-vs-Median
    %                                         flux figure alongside the
    %                                         default 3-group output.
    %                                         Default {}.
    %                      'MinEpochs'      - Minimum non-NaN epochs per
    %                                         source. Default 1.
    %                                         NOTE: this is forwarded to
    %                                         matchPhotEpochs, where it
    %                                         IRREVERSIBLY purges every
    %                                         field for sources with too
    %                                         few finite epochs in the
    %                                         MagFields(1) reference
    %                                         column. Set 1 keeps every
    %                                         source that has at least
    %                                         one finite epoch, so the
    %                                         saved MS can be re-used
    %                                         downstream at higher
    %                                         MinEpochs values
    %                                         (compareStabilityMS /
    %                                         plotPhotScatter do their
    %                                         own per-source filtering
    %                                         at plot time without
    %                                         destroying data).
    %                      'OutFile'        - .mat path to save the
    %                                         Result struct. Default ''
    %                                         (no save).
    %                      'Plot'           - Call plotPhotScatter on
    %                                         the resulting MS. Default
    %                                         true.
    %                      'ShowDots'       - Forward to plotPhotScatter:
    %                                         when false, omit the per-
    %                                         source dot scatter (only
    %                                         the binned trend line is
    %                                         drawn). Default true.
    %                      'ShowStdBand'    - Forward to plotPhotScatter:
    %                                         when true, draw a shaded
    %                                         +-std envelope around the
    %                                         binned median trend line.
    %                                         The std (not 16-84%
    %                                         quantiles) is used so the
    %                                         band represents the full
    %                                         scatter including outliers.
    %                                         Default false.
    %                      'LinkAxes'       - 'xy' | 'x' | 'y' | 'off'.
    %                                         Forward to plotPhotScatter:
    %                                         align the scales of the
    %                                         coupled panels within each
    %                                         group (RA+Dec, the two
    %                                         FluxFields, the two MAG
    %                                         pairs, etc.) so visual
    %                                         comparison is meaningful.
    %                                         Default 'y' (align Std
    %                                         axis only; X is already
    %                                         fixed at [9, 22]).
    %                      'Verbose'        - Print per-stage progress.
    %                                         Default false.
    % Output : - Result struct with:
    %            .MS         - MS.(mode){crop} MatchedSources from
    %                          matchPhotEpochs.
    %            .VisitDirs  - {1 x Nvisits} list of visit paths walked.
    %            .NVisits    - count.
    %            .FileType   - echo of Args.FileType.
    %            .Args       - full echo.
    % Author : D. Kovaleva (May 2026)
    % Example: % All v0 coadds under one date dir, no recalibration:
    %          R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  '/archimedes/iss165/LAST.01.03.01/2025/06/01/proc', ...
    %                  'VisitGlob', '*v0', 'Recursive', true, ...
    %                  'FileType', 'coadd');
    %
    %          % Loop v0/v1/v2 x coadd/proc into separate output mats:
    %          DataDir = '/archimedes/iss165/LAST.01.03.01/2025/06/01/proc';
    %          for v = 0:2
    %              for ft = ["coadd","proc"]
    %                  R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                          DataDir, ...
    %                          'VisitGlob', sprintf('*v%d', v), ...
    %                          'Recursive', true, ...
    %                          'FileType',  char(ft), ...
    %                          'OutFile',   sprintf('~/stab_v%d_%s.mat', v, ft));
    %              end
    %          end
    %
    %          % Restrict to one observation field + a single crop:
    %          R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DateDir, 'VisitGlob','*v[0-9]*', 'Recursive',true, ...
    %                  'FileType','coadd', 'FieldId','1781', 'CropID',10);
    %
    %          % Drop one or two visits that have a schema mismatch
    %          % (different column count) breaking the cross-match:
    %          R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DateDir, 'VisitGlob','*v1', 'Recursive',true, ...
    %                  'FileType','coadd', 'FieldId','1781', ...
    %                  'ExcludeVisits', {'190735v1','215120v1'});
    %
    %          % Add a dedicated flux figure (FLUX_APER_3 + FLUX_PSF):
    %          R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DateDir, 'VisitGlob','*v1', 'Recursive',true, ...
    %                  'FileType','coadd', ...
    %                  'FluxFields', {'FLUX_APER_3','FLUX_PSF'});
    %
    %          % Median line with +-std band, no per-source dots:
    %          R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DateDir, 'VisitGlob','*v1', 'Recursive',true, ...
    %                  'FileType','coadd', ...
    %                  'ShowDots', false, 'ShowStdBand', true);
    %
    %          % Align scales of coupled panels (RA<->Dec, the two MAGs,
    %          % the two FluxFields). Default already does this for Y;
    %          % use 'xy' to also share X (useful when comparing FluxFields):
    %          R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DateDir, 'VisitGlob','*v1', 'Recursive',true, ...
    %                  'FileType','coadd', ...
    %                  'FluxFields', {'FLUX_APER_3','FLUX_PSF'}, ...
    %                  'LinkAxes', 'xy');
    %
    %          % Disable axis-linking so each panel auto-scales:
    %          R = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DateDir, 'VisitGlob','*v1', 'Recursive',true, ...
    %                  'FileType','coadd', 'LinkAxes','off');

    arguments
        BaseDir              {mustBeText}
        Args.VisitGlob       {mustBeText}                          = '*v*'
        Args.Recursive       logical                               = false
        Args.FileType        {mustBeMember(Args.FileType, {'coadd','proc'})} = 'coadd'
        Args.FieldId                                                = ''
        Args.CropID          double {mustBeInteger, mustBeNonnegative} = []
        Args.ExcludeVisits   {mustBeText}                           = {}
        Args.Ncrop                                                  = 24
        Args.CropsToAnalyze                                         = []
        Args.MatchRadius                                            = 3
        Args.MagFields       cell                                   = {'MAG_PSF','MAG_APER_3'}
        Args.MatchedColumns  cell                                   = ...
            {'RA','Dec','X1','Y1','SN', ...
             'MAG_AB_PSF','MAG_AB_APER_3', ...
             'MAG_PSF','MAG_APER_3', ...
             'MAGERR_PSF','MAGERR_APER_3', ...
             'FLUX_APER_3','FLUX_PSF', ...
             'FLAGS'}
        Args.FluxFields      cell                                   = {}
        Args.MinEpochs                                              = 1
        Args.OutFile         {mustBeText}                          = ''
        Args.Plot            logical                                = true
        Args.ShowDots        logical                                = true
        Args.ShowStdBand     logical                                = false
        Args.LinkAxes        (1,:) char ...
            {mustBeMember(Args.LinkAxes, {'xy','x','y','off'})}      = 'y'
        Args.Verbose         logical                                = false
    end

    BaseDir = char(BaseDir);

    % Expand MatchedColumns with any requested flux columns (deduped).
    if ~isempty(Args.FluxFields)
        Args.MatchedColumns = unique([Args.MatchedColumns(:); ...
                                      Args.FluxFields(:)], 'stable').';
    end

    % --- Discover visit subdirectories ---------------------------------
    if Args.Recursive
        Listing = dir(fullfile(BaseDir, '**', Args.VisitGlob));
    else
        Listing = dir(fullfile(BaseDir, Args.VisitGlob));
    end
    Listing = Listing([Listing.isdir] & ~startsWith({Listing.name}, '.'));

    if isempty(Listing)
        Scope = BaseDir;
        if Args.Recursive; Scope = [Scope ' (recursive)']; end
        error('batchPhotStability:NoVisits', ...
            'No subdirectories matching "%s" found under %s', Args.VisitGlob, Scope);
    end

    % ExcludeVisits filter (match by basename or by full path).
    if ~isempty(Args.ExcludeVisits)
        ExNames = cellstr(string(Args.ExcludeVisits(:).'));
        Names   = {Listing.name};                                       % 1 x N
        Paths   = arrayfun(@(d) fullfile(d.folder, d.name), ...
                           Listing(:).', 'Uni', 0);                     % 1 x N
        Drop    = ismember(Names, ExNames) | ismember(Paths, ExNames);  % 1 x N
        if Args.Verbose && any(Drop)
            fprintf('Excluding %d visit dir(s): %s\n', sum(Drop), ...
                strjoin(Names(Drop), ', '));
        end
        Listing = Listing(~Drop);
        if isempty(Listing)
            error('batchPhotStability:AllExcluded', ...
                'ExcludeVisits dropped every matching visit dir.');
        end
    end

    Nvisits   = numel(Listing);
    VisitDirs = string(arrayfun(@(d) fullfile(d.folder, d.name), Listing(:), 'Uni', 0));

    if Args.Verbose
        fprintf('batchPhotStability: %d visit dirs under %s (FileType=%s)\n', ...
            Nvisits, BaseDir, Args.FileType);
    end

    % --- Load Cat + Header (no fitting) -------------------------------
    AIc = pipeline.last.load.loadVisitCatHdr( ...
        'VisitDirs', VisitDirs, ...
        'FileType',  Args.FileType, ...
        'FieldId',   Args.FieldId, ...
        'CropID',    Args.CropID, ...
        'Verbose',   Args.Verbose);

    % --- Repackage AIvis catalogs as Cats.percrop{Iv}(Ic) --------------
    Cats = struct();
    Cats.percrop = cell(1, numel(AIc));
    NValid = 0;
    for Iv = 1:numel(AIc)
        if isempty(AIc{Iv}); continue; end
        AIvis  = AIc{Iv};
        CatVec = AstroCatalog.empty(0, numel(AIvis));
        for Ic = 1:numel(AIvis)
            CatVec(Ic) = AIvis(Ic).CatData;
        end
        Cats.percrop{Iv} = CatVec;
        NValid = NValid + 1;
    end
    if NValid == 0
        error('batchPhotStability:NoCats', ...
            'No catalogs loaded after FieldId/CropID filtering.');
    end

    % --- Cross-match across visits ------------------------------------
    MS = pipeline.last.quality.photCalib.matchPhotEpochs(Cats, ...
        'Modes',          {'percrop'}, ...
        'Ncrop',          Args.Ncrop, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'MatchRadius',    Args.MatchRadius, ...
        'MagFields',      Args.MagFields, ...
        'MatchedColumns', Args.MatchedColumns, ...
        'MinEpochs',      Args.MinEpochs, ...
        'Verbose',        Args.Verbose);

    % --- Assemble Result ----------------------------------------------
    Result.MS        = MS;
    Result.VisitDirs = VisitDirs;
    Result.NVisits   = Nvisits;
    Result.FileType  = Args.FileType;
    Result.Args      = Args;

    if ~isempty(Args.OutFile)
        try
            save(char(Args.OutFile), 'Result', '-v7.3');
            if Args.Verbose
                fprintf('Saved %s\n', Args.OutFile);
            end
        catch ME
            warning('batchPhotStability:SaveFailed', 'Save failed: %s', ME.message);
        end
    end

    % --- Plot ---------------------------------------------------------
    if Args.Plot
        pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
            'Modes',          {'percrop'}, ...
            'CropsToAnalyze', Args.CropsToAnalyze, ...
            'MinEpochs',      Args.MinEpochs, ...
            'ShowDots',       Args.ShowDots, ...
            'ShowStdBand',    Args.ShowStdBand, ...
            'LinkAxes',       Args.LinkAxes);

        % Extra figure for the requested flux columns (single group).
        if ~isempty(Args.FluxFields)
            pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
                'Modes',          {'percrop'}, ...
                'QuantityGroups', {Args.FluxFields(:).'}, ...
                'CropsToAnalyze', Args.CropsToAnalyze, ...
                'MinEpochs',      Args.MinEpochs, ...
                'ShowDots',       Args.ShowDots, ...
                'ShowStdBand',    Args.ShowStdBand);
        end
    end
end
