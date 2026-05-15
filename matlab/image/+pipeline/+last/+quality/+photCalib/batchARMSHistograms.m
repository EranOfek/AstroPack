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
    %            (recursive by default) are searched for visits.
    %                    * ...,key,val,...
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
    %                      'Mode'          - mode field name in the
    %                                        normalised MS struct.
    %                                        Default 'percrop'.
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

    arguments
        BaseDir              {mustBeText}
        Args.VisitGlob       {mustBeText} = '*v*'
        Args.CompareGlobs    cell         = {}
        Args.CompareLabels   cell         = {}
        Args.Recursive       logical      = true
        Args.FieldId                       = ''
        Args.CropID          double {mustBeInteger, mustBeNonnegative} = []
        Args.ExcludeVisits   {mustBeText} = {}
        Args.LoaderMode      (1,:) char ...
            {mustBeMember(Args.LoaderMode, {'auto','loadVisit','loadMergedMat'})} = 'auto'
        Args.Quantities      cell = {'RA','Dec','FLUX_APER_3'}
        Args.PanelGroups     cell = {{'RA','Dec'}, {'FLUX_APER_3'}}
        Args.AngularQuantities cell = {'RA','Dec'}
        Args.FluxAsMag       cell = {'FLUX_PSF','FLUX_APER_3'}
        Args.RefMag          (1,:) char = 'MAG_APER_3'
        Args.BackgroundMag   (1,1) double = 22
        Args.ARMS_N          (1,1) double {mustBePositive, mustBeInteger} = 20
        Args.MinEpochs       (1,1) double = 2
        Args.BadFlags        cell = {'Saturated','NearEdge','Overlap'}
        Args.Mode            (1,:) char = 'percrop'
        Args.Bins                         = 30
        Args.OutDir          {mustBeText} = '~/results_ARMS'
        Args.Plot            logical      = true
        Args.SaveFig         logical      = false
        Args.Verbose         logical      = true
    end

    % --- Cohort setup --------------------------------------------------
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
    Ncohorts = numel(CohortGlobs);

    % --- Loop cohorts, build per-row ARMS records ---------------------
    % Accumulate into a cell of scalar structs (all carry the same field
    % set: Cohort/Visit/Crop + one *_ARMS field per quantity), then
    % concatenate at the end.
    RecCell = {};

    for Ic = 1:Ncohorts
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
            MS = loadOneVisitMS(VPath, Args);
            if isempty(MS); continue; end
            CCell = normaliseMSToCellOfCrops(MS, Args.Mode);
            Ncrop = numel(CCell);
            % FieldId / CropID file-level filter already applied at
            % discovery — apply CropID at MS level too.
            if ~isempty(Args.CropID)
                if Args.CropID <= Ncrop
                    CCell = CCell(Args.CropID);
                else
                    continue;
                end
            end
            for Cc = 1:numel(CCell)
                R = struct();
                R.Cohort = CohortLabels{Ic};
                [~, R.Visit] = fileparts(VPath);
                if ~isempty(Args.CropID)
                    R.Crop = Args.CropID;
                else
                    R.Crop = Cc;
                end
                for Iqq = 1:numel(Args.Quantities)
                    Q = Args.Quantities{Iqq};
                    R.(varName(Q)) = cropARMS(CCell{Cc}, Q, Args);
                end
                RecCell{end+1} = R; %#ok<AGROW>
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
        plotARMSHistograms(Result, Args);
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
    UseMerged = strcmp(Args.LoaderMode, 'loadMergedMat') || ...
        (strcmp(Args.LoaderMode, 'auto') && ...
         ~isempty(dir(fullfile(VPath, '*_sci_merged_MergedMat_*.hdf5'))));
    try
        if UseMerged
            MS = pipeline.last.load.loadMergedMat( ...
                'MergedMatDir', VPath, 'Verbose', false);
        else
            [~, ~, MS] = pipeline.last.load.loadVisit(VPath);
        end
    catch ME
        if Args.Verbose
            fprintf('  visit %s: load failed (%s)\n', VPath, ME.message);
        end
    end
end

% =========================================================================
function CCell = normaliseMSToCellOfCrops(MS, Mode)
    % Accept MatchedSources array, cell of MS, or mode-keyed struct.
    if isa(MS, 'MatchedSources')
        CCell = num2cell(MS(:).');
    elseif iscell(MS)
        CCell = MS(:).';
    elseif isstruct(MS) && isfield(MS, Mode)
        CCell = MS.(Mode);
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
function plotARMSHistograms(Result, Args)
    OutDir = char(Args.OutDir);
    if Args.SaveFig && ~exist(OutDir, 'dir'); mkdir(OutDir); end
    T = Result.ARMS;
    Cohorts = unique(T.Cohort, 'stable');
    Cmap = lines(numel(Cohorts));
    for Ig = 1:numel(Args.PanelGroups)
        QGroup = Args.PanelGroups{Ig};
        Np = numel(QGroup);
        F = figure('Name', sprintf('ARMS: %s', strjoin(QGroup, ' / ')), ...
            'Position', [60 60 420*Np 380]);
        for Ip = 1:Np
            Ax = subplot(1, Np, Ip); hold(Ax, 'on');
            Q = QGroup{Ip};
            Col = varName(Q);
            if ~ismember(Col, T.Properties.VariableNames); continue; end
            Vals = T.(Col);
            Fin  = isfinite(Vals);
            if ~any(Fin)
                title(Ax, sprintf('%s (no data)', Q), 'Interpreter','none');
                continue;
            end
            if isscalar(Args.Bins)
                Edges = linspace(min(Vals(Fin)), max(Vals(Fin)), Args.Bins + 1);
            else
                Edges = Args.Bins;
            end
            for Ic = 1:numel(Cohorts)
                Sel = strcmp(T.Cohort, Cohorts{Ic}) & Fin;
                if ~any(Sel); continue; end
                histogram(Ax, Vals(Sel), Edges, ...
                    'FaceColor', Cmap(Ic,:), 'FaceAlpha', 0.55, ...
                    'EdgeColor', Cmap(Ic,:) * 0.6, 'LineWidth', 0.8, ...
                    'Normalization', 'probability', ...
                    'DisplayName', sprintf('%s (N=%d, med=%.3g)', ...
                        Cohorts{Ic}, sum(Sel), median(Vals(Sel))));
            end
            grid(Ax, 'on'); box(Ax, 'on');
            xlabel(Ax, sprintf('ARMS(%s)', Q), 'Interpreter', 'none');
            ylabel(Ax, 'Fraction');
            title(Ax, Q, 'Interpreter', 'none');
            if numel(Cohorts) > 1
                legend(Ax, 'Location', 'best', 'Interpreter', 'none');
            end
        end
        sgtitle(sprintf('ARMS distribution: %s', strjoin(QGroup, ' / ')), ...
            'Interpreter', 'none');
        if Args.SaveFig
            Base = fullfile(OutDir, sprintf('arms_%s', ...
                matlab.lang.makeValidName(strjoin(QGroup, '_'))));
            savefig(F, [Base '.fig']);
            saveas(F, [Base '.png']);
        end
    end
end
