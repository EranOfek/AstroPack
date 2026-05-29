function Result = batchARMSHistograms(BaseDir, Args)
    % Per-crop ARMS distributions across many visits, one or two cohorts
    % Description: Walks visit subdirectories under BaseDir (mirroring
    %              batchPhotStability's discovery API), loads each
    %              visit's MatchedSources via pipeline.last.load.loadVisit
    %              (or loadMergedMat when available), computes a per-crop
    %              ARMS = median of the ARMS_N lowest per-source Std
    %              values for each requested quantity, and renders the
    %              ARMS distribution(s) as histograms. Two-cohort
    %              comparison: pass two patterns via 'CompareGlobs' and
    %              both populations are overlaid per quantity.
    %
    %              ARMS is computed per (visit, crop, quantity) so a
    %              single visit contributes Ncrop entries to the
    %              histogram. Angular quantities (RA, Dec) include the
    %              arcsec rescale and cos(median Dec) projection per
    %              source. FluxAsMag columns are converted via
    %              -2.5*log10(Flux) per epoch before the Std reduction.
    %
    % Input  : - BaseDir char/string — parent dir whose subdirectories
    %            (recursive by default) are searched for visits. May be
    %            '' when 'MSData' supplies pre-built MatchedSources
    %            instead of directory discovery.
    %                    * ...,key,val,...
    %                      'MSData'        - struct array of pre-built
    %                                        visit MatchedSources; when
    %                                        non-empty, directory
    %                                        discovery is skipped and
    %                                        BaseDir is ignored. Fields:
    %                                          .MS     - a MatchedSources
    %                                                    array or a cell
    %                                                    of MatchedSources
    %                                                    (e.g. the output
    %                                                    of
    %                                                    matchVisitEpochs);
    %                                          .Visit  - char label
    %                                                    (optional;
    %                                                    default
    %                                                    'visitK');
    %                                          .Cohort - char label
    %                                                    (optional;
    %                                                    default
    %                                                    'prebuilt') used
    %                                                    to group / colour
    %                                                    the histograms.
    %                                        Use this to feed visit MS
    %                                        carrying columns the merged
    %                                        products lack (e.g.
    %                                        FLUX_PSF). Default {} struct.
    %                      'VisitGlob'     - single-cohort pattern,
    %                                        ignored if CompareGlobs is
    %                                        set. Default '*v*'.
    %                      'CompareGlobs'  - {pat_A, pat_B, ...} cell of
    %                                        2+ patterns; produces a
    %                                        cohort per pattern,
    %                                        overlaid in each histogram.
    %                                        Default {} (single cohort).
    %                      'CompareLabels' - one label per cohort for
    %                                        the legend. Default {} (=
    %                                        derived from CompareGlobs).
    %                      'Recursive'     - dir() walk recursively.
    %                                        Default true.
    %                      'FieldId'       - AstroFileName.FieldID filter
    %                                        (peek-file test). Default ''.
    %                      'CropID'        - integer crop slot. Default
    %                                        [] (no per-file slice).
    %                      'ExcludeVisits' - cell/string array of visit
    %                                        basenames to drop. Default {}.
    %                      'LoaderMode'    - 'auto' | 'loadVisit' |
    %                                        'loadMergedMat'.
    %                                        'auto' prefers MergedMat
    %                                        when *.hdf5 are present in
    %                                        the visit dir, else falls
    %                                        back to loadVisit. Default
    %                                        'auto'.
    %                      'FileType'      - 'any' | 'proc' | 'coadd'.
    %                                        When not 'any', applies TWO
    %                                        filters:
    %                                        (1) discovered visit dirs
    %                                            must have '/proc/' or
    %                                            '/coadd/' as a path
    %                                            component (matching the
    %                                            LAST tree where proc and
    %                                            coadd live as sibling
    %                                            buckets);
    %                                        (2) the merged HDF5 file
    %                                            loaded inside each visit
    %                                            must contain the literal
    %                                            token '_proc_' or
    %                                            '_coadd_' in its name.
    %                                        Visits failing either filter
    %                                        are silently skipped (logged
    %                                        when Verbose). Default 'any'.
    %                      'Quantities'    - cell of column names.
    %                                        Default
    %                                        {'RA','Dec','FLUX_APER_3'}.
    %                      'PanelGroups'   - cell of cells; each inner
    %                                        cell is one figure
    %                                        rendered as a row of
    %                                        histograms (one panel per
    %                                        quantity in the group).
    %                                        Default
    %                                        {{'RA','Dec'},{'FLUX_APER_3'}}.
    %                      'AngularQuantities' - names treated as
    %                                        angular. Default
    %                                        {'RA','Dec'}.
    %                      'FluxAsMag'     - cell of names converted
    %                                        to instrumental mag via
    %                                        -2.5*log10(Flux). Default
    %                                        {'FLUX_PSF','FLUX_APER_3'}.
    %                      'RefMag'        - reference mag for the
    %                                        angular faint cut. Default
    %                                        'MAG_APER_3'.
    %                      'BackgroundMag' - faint cut on RefMag.
    %                                        Default 22.
    %                      'ARMS_N'        - lowest-N strictly-positive
    %                                        Std values used to compute
    %                                        ARMS = median of them per
    %                                        crop. Exact zeros are
    %                                        excluded (float32
    %                                        quantization artifacts).
    %                                        Default 20.
    %                      'MinEpochs'     - min finite epochs per
    %                                        source to be eligible.
    %                                        Default 2.
    %                      'BadFlags'      - cell of FLAGS bit names;
    %                                        any (epoch,source) entry
    %                                        carrying one of these bits
    %                                        is set to NaN before the
    %                                        Std reduction (via
    %                                        MatchedSources.searchFlags).
    %                                        Default
    %                                        {'Saturated','NearEdge',
    %                                         'Overlap'}; {} disables
    %                                        FLAGS filtering.
    %                      'Bins'          - histogram bin count or
    %                                        explicit edges. Default 30.
    %                      'OutDir'        - directory for saved
    %                                        figures / Result.mat.
    %                                        Default '~/results_ARMS'.
    %                      'Plot','SaveFig','Verbose' - logical
    %                                        defaults true / false / true.
    % Output : - Result struct:
    %            .ARMS        table - one row per (cohort, visit, crop),
    %                          one column per Quantity (ARMS value).
    %            .Quantities  echo.
    %            .CohortNames cellstr of cohort labels.
    %            .Args        echo.
    % Author : D. Kovaleva (May 2026)
    % Example: % Single cohort, all v0 visits under a date tree:
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms( ...
    %                  '/archimedes/iss165/LAST.01.03.01/2025/05/03/proc', ...
    %                  'VisitGlob','*v0', 'Recursive',true);
    %
    %          % Two-cohort overlay (v0 vs v2):
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms( ...
    %                  '/archimedes/iss165/LAST.01.03.01/2025/05/03/proc', ...
    %                  'CompareGlobs', {'*v0','*v2'}, ...
    %                  'CompareLabels', {'v0','v2'}, ...
    %                  'Recursive', true);
    %
    %          % Custom ARMS depth (30 best per crop) and a wider
    %          % bad-flag list; inspect the resulting table:
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms( ...
    %                  BaseDir, 'VisitGlob','*v0', ...
    %                  'ARMS_N', 30, ...
    %                  'BadFlags', {'Saturated','NearEdge','Overlap', ...
    %                               'CR_DeltaHT','Negative'});
    %          disp(R.ARMS);                        % per (visit,crop) rows
    %          fprintf('median ARMS(RA) = %.3g arcsec\n', ...
    %                  median(R.ARMS.RA_ARMS, 'omitnan'));
    %
    %          % Disable FLAGS filtering entirely (use raw photometry):
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms( ...
    %                  BaseDir, 'VisitGlob','*v0', 'BadFlags', {});
    %
    %          % Single cohort with extra quantities + matching panel
    %          % groups (e.g. add MAG_PSF if your MS carries it):
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms( ...
    %                  BaseDir, 'VisitGlob','*v2', ...
    %                  'Quantities',  {'RA','Dec','FLUX_APER_3','MAG_PSF'}, ...
    %                  'PanelGroups', {{'RA','Dec'},{'FLUX_APER_3'},{'MAG_PSF'}});
    %
    %          % Pre-built MS (carrying FLUX_PSF) via matchVisitEpochs —
    %          % the merged products do not store FLUX_PSF, so build the
    %          % visit MS yourself, then upload it through 'MSData':
    %          MSData = struct('MS',{},'Visit',{},'Cohort',{});
    %          for k = 1:numel(VisitPaths)
    %              M = pipeline.last.quality.photCalib.matchVisitEpochs( ...
    %                      VisitPaths{k});
    %              [~,vn] = fileparts(VisitPaths{k});
    %              MSData(end+1) = struct('MS',M,'Visit',vn,'Cohort','v0');
    %          end
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms('', ...
    %                  'MSData', MSData, ...
    %                  'Quantities',  {'RA','Dec','FLUX_PSF'}, ...
    %                  'PanelGroups', {{'RA','Dec'},{'FLUX_PSF'}});

    arguments
        BaseDir              {mustBeText} = ''
        Args.MSData          struct       = struct('MS', {}, 'Visit', {}, 'Cohort', {})
        Args.VisitGlob       {mustBeText} = '*v*'
        Args.CompareGlobs    cell         = {}
        Args.CompareLabels   cell         = {}
        Args.Recursive       logical      = true
        Args.FieldId                       = ''
        Args.CropID          double {mustBeInteger, mustBeNonnegative} = []
        Args.ExcludeVisits   {mustBeText} = {}
        Args.LoaderMode      (1,:) char ...
            {mustBeMember(Args.LoaderMode, {'auto','loadVisit','loadMergedMat'})} = 'auto'
        Args.FileType        (1,:) char ...
            {mustBeMember(Args.FileType, {'any','proc','coadd'})} = 'any'
        Args.Quantities      cell = {'RA','Dec','FLUX_APER_3', 'FLUX_PSF'}
        Args.PanelGroups     cell = {{'RA','Dec'}, {'FLUX_APER_3', 'FLUX_PSF'}}
        Args.AngularQuantities cell = {'RA','Dec'}
        Args.FluxAsMag       cell = {'FLUX_APER_3', 'FLUX_PSF'}
        Args.RefMag          (1,:) char = 'MAG_APER_3'
        Args.BackgroundMag   (1,1) double = 22
        Args.ARMS_N          (1,1) double {mustBePositive, mustBeInteger} = 20
        Args.MinEpochs       (1,1) double = 10
        Args.BadFlags        cell = {'Saturated','NearEdge','Overlap'}
        Args.Bins                         = 30
        Args.OutDir          {mustBeText} = '~/results_ARMS'
        Args.Plot            logical      = true
        Args.SaveFig         logical      = false
        Args.Verbose         logical      = true
    end

    % --- Input mode: pre-built MS upload vs directory discovery -------
    UseMSData = ~isempty(Args.MSData);
    if ~UseMSData && isempty(char(BaseDir))
        error('batchARMSHistograms:NoInput', ...
            ['Provide BaseDir for directory discovery, or Args.MSData ', ...
             'with pre-built MatchedSources (e.g. from matchVisitEpochs).']);
    end
    if UseMSData && ~isfield(Args.MSData, 'MS')
        error('batchARMSHistograms:BadMSData', ...
            'Args.MSData must be a struct array with at least an .MS field.');
    end

    % Accumulate into a cell of scalar structs (all carry the same field
    % set: Cohort/Visit/Crop + one *_ARMS field per quantity), then
    % concatenate at the end.
    RecCell = {};

    if UseMSData
        % Pre-built MatchedSources: one struct-array element per visit,
        % each with .MS plus optional .Visit / .Cohort labels.
        CLabels = cell(1, numel(Args.MSData));
        for Im = 1:numel(Args.MSData)
            Entry      = Args.MSData(Im);
            Cohort     = fieldOrDefault(Entry, 'Cohort', 'prebuilt');
            Visit      = fieldOrDefault(Entry, 'Visit',  sprintf('visit%d', Im));
            CLabels{Im} = Cohort;
            RecCell    = [RecCell, recordsFromMS(Entry.MS, Cohort, Visit, Args)]; %#ok<AGROW>
        end
        CohortLabels = unique(CLabels, 'stable');
        if Args.Verbose
            fprintf('batchARMSHistograms: %d pre-built visit MS, %d cohort(s)\n', ...
                numel(Args.MSData), numel(CohortLabels));
        end
    else
        % --- Cohort setup ---------------------------------------------
        if isempty(Args.CompareGlobs)
            CohortGlobs  = {char(Args.VisitGlob)};
            CohortLabels = {char(Args.VisitGlob)};
        else
            CohortGlobs  = cellfun(@char, Args.CompareGlobs(:).', 'Uni', 0);
            if isempty(Args.CompareLabels)
                CohortLabels = CohortGlobs;
            else
                CohortLabels = cellfun(@char, Args.CompareLabels(:).', 'Uni', 0);
            end
            if numel(CohortLabels) ~= numel(CohortGlobs)
                error('batchARMSHistograms:LabelMismatch', ...
                    'CompareLabels must have one entry per CompareGlobs.');
            end
        end

        % --- Loop cohorts, build per-row ARMS records -----------------
        for Ic = 1:numel(CohortGlobs)
            VD = discoverVisits(char(BaseDir), CohortGlobs{Ic}, Args);
            if isempty(VD)
                if Args.Verbose
                    fprintf('  cohort %s: 0 visits under %s\n', CohortLabels{Ic}, BaseDir);
                end
                continue;
            end
            if Args.Verbose
                fprintf('  cohort %s: %d visits\n', CohortLabels{Ic}, numel(VD));
            end
            for Iv = 1:numel(VD)
                VPath = VD{Iv};
                MS    = loadOneVisitMS(VPath, Args);
                if isempty(MS); continue; end
                [~, Visit] = fileparts(VPath);
                RecCell = [RecCell, ...
                    recordsFromMS(MS, CohortLabels{Ic}, Visit, Args)]; %#ok<AGROW>
            end
        end
    end

    if isempty(RecCell)
        Records = struct('Cohort', {}, 'Visit', {}, 'Crop', {});
    else
        Records = [RecCell{:}];   % all share the same field set
    end

    Result.ARMS        = struct2table(Records);
    Result.Quantities  = Args.Quantities;
    Result.CohortNames = CohortLabels;
    Result.Args        = Args;

    if Args.Verbose
        fprintf('batchARMSHistograms: %d rows in ARMS table\n', height(Result.ARMS));
    end

    if Args.SaveFig
        OutDir = char(Args.OutDir);
        if ~exist(OutDir, 'dir'); mkdir(OutDir); end
        try
            save(fullfile(OutDir, 'batchARMSHistograms.mat'), 'Result', '-v7.3');
        catch ME
            warning('batchARMSHistograms:SaveFailed', 'Save failed: %s', ME.message);
        end
    end

    if Args.Plot && ~isempty(Records)
        pipeline.last.quality.photCalib.plotARMSHistograms(Result, ...
            'PanelGroups',       Args.PanelGroups, ...
            'GroupColumn',       'Cohort', ...
            'AngularQuantities', Args.AngularQuantities, ...
            'FluxAsMag',         Args.FluxAsMag, ...
            'Bins',              Args.Bins, ...
            'SaveFig',           Args.SaveFig, ...
            'OutDir',            Args.OutDir);
    end
end

% =========================================================================
function VD = discoverVisits(BaseDir, Glob, Args)
    if Args.Recursive
        D = dir(fullfile(BaseDir, '**', Glob));
    else
        D = dir(fullfile(BaseDir, Glob));
    end
    D = D([D.isdir] & ~startsWith({D.name}, '.'));
    Paths = arrayfun(@(d) fullfile(d.folder, d.name), D(:).', 'Uni', 0);
    if ~isempty(Args.ExcludeVisits)
        Drop = ismember({D.name}, cellstr(string(Args.ExcludeVisits))) ...
             | ismember(Paths,    cellstr(string(Args.ExcludeVisits)));
        Paths = Paths(~Drop);
    end
    % FileType path-bucket filter: keep only visits whose path contains
    % the matching '/proc/' or '/coadd/' directory component.
    if ~strcmpi(Args.FileType, 'any')
        Tag = ['/' lower(Args.FileType) '/'];
        Keep = contains(Paths, Tag);
        Paths = Paths(Keep);
    end
    % FieldId peek per visit dir (one file).
    if ~isempty(Args.FieldId)
        Keep = false(1, numel(Paths));
        for I = 1:numel(Paths)
            Listing = dir(fullfile(Paths{I}, '*_sci_*_*_1.fits'));
            if isempty(Listing); continue; end
            try
                Tok = AstroFileName({fullfile(Listing(1).folder, Listing(1).name)}).FieldID;
                Keep(I) = strcmp(string(Tok), string(Args.FieldId));
            catch
            end
        end
        Paths = Paths(Keep);
    end
    VD = Paths;
end

% =========================================================================
function MS = loadOneVisitMS(VPath, Args)
    MS = [];

    % FileType filename-token filter (empty when FileType='any').
    Tag = '';
    if ~strcmpi(Args.FileType, 'any')
        Tag = sprintf('_%s_', lower(Args.FileType));   % '_proc_' or '_coadd_'
    end

    % Candidate MergedMat HDF5 files in the visit dir, after Tag filter.
    DM = dir(fullfile(VPath, '*_sci_merged_MergedMat_*.hdf5'));
    if ~isempty(Tag) && ~isempty(DM)
        DM = DM(contains({DM.name}, Tag));
    end
    HasMerged = ~isempty(DM);

    UseMerged = strcmp(Args.LoaderMode, 'loadMergedMat') || ...
        (strcmp(Args.LoaderMode, 'auto') && HasMerged);

    try
        if UseMerged
            if ~HasMerged
                if Args.Verbose
                    fprintf('  visit %s: no MergedMat HDF5 matching tag %s\n', VPath, Tag);
                end
                return;
            end
            S = pipeline.last.load.loadMergedMat( ...
                'MergedMatDir', VPath, 'Verbose', false);
            if isstruct(S) && isfield(S, 'percrop')
                MS = S.percrop;                 % cell of MatchedSources
            else
                MS = S;
            end
        else
            % loadVisit's default MS template ('*_sci_merged_MatchedMat_*.hdf5')
            % is overridden when a tag is requested.
            if isempty(Tag)
                [~, ~, MS] = pipeline.last.load.loadVisit(VPath);
            else
                Tmpl = sprintf('LAST*%s*MatchedMat*.hdf5', Tag);
                if isempty(dir(fullfile(VPath, Tmpl)))
                    Tmpl = sprintf('LAST*MatchedMat*%s*.hdf5', Tag);
                end
                if isempty(dir(fullfile(VPath, Tmpl)))
                    if Args.Verbose
                        fprintf('  visit %s: no MatchedMat HDF5 matching tag %s\n', VPath, Tag);
                    end
                    return;
                end
                [~, ~, MS] = pipeline.last.load.loadVisit(VPath, 'TempName_MS', Tmpl);
            end
        end
    catch ME
        if Args.Verbose
            fprintf('  visit %s: load failed (%s)\n', VPath, ME.message);
        end
    end
end

% =========================================================================
function CCell = normaliseMSToCellOfCrops(MS)
    % Accept either a MatchedSources array or a cell of MatchedSources.
    if isa(MS, 'MatchedSources')
        CCell = num2cell(MS(:).');
    elseif iscell(MS)
        CCell = MS(:).';
    else
        CCell = {};
    end
end

% =========================================================================
function ARMS = cropARMS(Msi, Q, Args)
    ARMS = NaN;
    if isempty(Msi) || ~isfield(Msi.Data, Q); return; end
    Y = Msi.Data.(Q);
    if isempty(Y); return; end
    % Per-epoch FLAGS rejection: NaN out (epoch,source) entries carrying
    % any of the BadFlags bits before the Std reduction.
    if ~isempty(Args.BadFlags) && isfield(Msi.Data, 'FLAGS')
        try
            Bad = Msi.searchFlags('FlagsList', Args.BadFlags);   % Nepoch x Nsrc
            Y(Bad) = NaN;
        catch
        end
    end
    IsAng = ismember(Q, Args.AngularQuantities);
    if ismember(Q, Args.FluxAsMag)
        Y(Y <= 0) = NaN;
        Y = -2.5 * log10(Y);
    end
    S = std(Y, 0, 1, 'omitnan').';
    NValid = sum(~isnan(Y), 1).';
    Keep = NValid >= Args.MinEpochs;
    if IsAng
        S = S * 3600;
        if strcmpi(Q, 'RA') && isfield(Msi.Data, 'Dec')
            Dm = median(Msi.Data.Dec, 1, 'omitnan').';
            S  = S .* cosd(Dm);
        end
        if isfield(Msi.Data, Args.RefMag)
            Rm = median(Msi.Data.(Args.RefMag), 1, 'omitnan').';
            Keep = Keep & (Rm <= Args.BackgroundMag);
        end
    end
    Svec = S(Keep);
    % Exclude exact zeros: std==0 is a float32 quantization artifact
    % (RA ~273 deg stored as single has a ~0.12" step), not a real
    % noise floor — keeping them would drag ARMS to 0.
    Svec = Svec(isfinite(Svec) & Svec > 0);
    if numel(Svec) < Args.ARMS_N; return; end
    Svec = sort(Svec);
    ARMS = median(Svec(1:Args.ARMS_N));
end

% =========================================================================
function V = varName(Q)
    % Make a struct-field-safe column name from a quantity name.
    V = matlab.lang.makeValidName([char(Q) '_ARMS']);
end

% =========================================================================
function Recs = recordsFromMS(MS, Cohort, Visit, Args)
    % Per-crop ARMS records for one visit's MatchedSources, as a cell of
    % scalar structs (Cohort/Visit/Crop + one *_ARMS field per quantity).
    Recs  = {};
    CCell = normaliseMSToCellOfCrops(MS);
    if isempty(CCell); return; end
    if ~isempty(Args.CropID)
        if Args.CropID <= numel(CCell)
            CCell = CCell(Args.CropID);
        else
            return;
        end
    end
    for Cc = 1:numel(CCell)
        R = struct();
        R.Cohort = Cohort;
        R.Visit  = Visit;
        if ~isempty(Args.CropID)
            R.Crop = Args.CropID;
        else
            R.Crop = Cc;
        end
        for Iqq = 1:numel(Args.Quantities)
            Q = Args.Quantities{Iqq};
            R.(varName(Q)) = cropARMS(CCell{Cc}, Q, Args);
        end
        Recs{end+1} = R; %#ok<AGROW>
    end
end

% =========================================================================
function V = fieldOrDefault(S, Name, Default)
    % Char value of struct field Name, or Default when absent / empty.
    if isfield(S, Name) && ~isempty(S.(Name))
        V = char(string(S.(Name)));
    else
        V = Default;
    end
end

