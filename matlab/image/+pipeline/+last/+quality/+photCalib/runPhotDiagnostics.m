function [Result, Fig] = runPhotDiagnostics(Input, Args)
    % Run a configurable set of photometric-calibration diagnostics.
    % Description: One-stop runner that calibrates a set of visits (path or
    %              in-memory) via calibrateVisits, optionally cross-matches
    %              the resulting catalogs via matchEpochs, and dispatches a
    %              user-selected list of plot families. Replaces the legacy
    %              testPhotCalib / testPhotStability / testPhotFitQuality
    %              trio; the test* prefix (which wrongly implied unit tests)
    %              is gone.
    %
    %              Plot families ('Plots' argument):
    %                'fitquality'   plotPhotResiduals (mag, airmass,
    %                               background, X, Y) + plotPhotResidualsColor
    %                'series'       plotPhotSeries (fitted params, ARMS,
    %                               integral T, FWHM vs epoch)
    %                'transmission' plotPhotTransmission (per-crop curves)
    %                'mosaic'       plotPhotParamMosaic (per-crop focal-plane
    %                               maps)
    %                'synchrony'    plotPhotParamSynchrony (cross-crop)
    %                'params'       plotPhotParamHist (per-quantity)
    %                'stability'    plotPhotStability (Std vs Mag, needs
    %                               cross-matched MS - auto-enables matching)
    %
    % Input  : - Input - one of:
    %              * char/string BaseDir - visit subdirectories are
    %                discovered and calibrated via calibrateVisits;
    %              * cell of AstroImage arrays - one pre-loaded visit per
    %                cell;
    %              * a single AstroImage array - one visit;
    %              * a pre-existing Result struct from calibrateVisits or a
    %                previous runPhotDiagnostics run - calibration is
    %                skipped.
    %          * ...,key,val,...
    %            'Plots'              - cell of family names (see above).
    %                                   Default {'fitquality','series', ...
    %                                           'transmission','stability'}.
    %            'MatchEpochs'        - true | false | [] (auto - true when
    %                                   'stability' is in Plots). Default [].
    %            'CalibArgs'          - NV pairs forwarded to fitPhotCalibTrans.
    %                                   Default {}.
    %            'CropsToAnalyze'     - Crop indices to include. Default [].
    %            'RefCrop'            - Reference crop. Default 10.
    %            'TileOrder'          - 'rowmajor' | 'colmajor'. Default
    %                                   'rowmajor'.
    %            'MatchRadius'        - Cross-match radius [arcsec].
    %                                   Default 2.
    %            'MagFields'          - Magnitude fields for matchEpochs +
    %                                   stability. Default
    %                                   {'MAG_PSF','MAG_APER_3'}.
    %            'SeriesQuantities'   - Quantities for the 'series' family.
    %                                   Default {'TauAod500','PWV_cm',
    %                                            'Center_Ang','Norm',
    %                                            'ARMS','IntegralT'}.
    %            'MosaicQuantities'   - Quantities for the 'mosaic' family.
    %                                   Default {'TauAod500','PWV_cm',
    %                                            'Norm','IntegralT'}.
    %            'ParamHistQuantities' - Quantities for the 'params' family.
    %                                   Default {'Chi2_DOF',
    %                                            'NCalibRetention'}.
    %            'ResidualXAxis'      - X axes for the 'fitquality' residuals.
    %                                   Default {'mag','airmass',
    %                                            'background','X','Y'}.
    %            'OutDir'             - Directory for cached calibration /
    %                                   saved figures. Default ''.
    %            'SaveFig'            - Save all figures to OutDir on exit
    %                                   via saveFigures. Default false.
    %            'ForceRecalc'        - Recompute the calibration cache.
    %                                   Default false.
    %            'Verbose'            - Per-step progress. Default false.
    %            'VisitGlob'          - Visit glob (path input). Default '*v*'.
    %            'Recursive'          - Recurse for visits (path input).
    %                                   Default false.
    %            'FileType'           - 'coadd' | 'proc' (path input).
    %                                   Default 'coadd'.
    %            'FieldId'            - Field-id filter (path input).
    %                                   Default ''.
    % Output : - Result - struct with .Calib (calibrateVisits Result),
    %            .MS (MatchedSources array; only if MatchEpochs ran), and
    %            .Fig (handle array). Echoes .Args.
    %          - Fig - the figure-handle array (also at Result.Fig).
    % Author : photCalib package refactor (2026-05)
    % Example: % Path input, defaults:
    %          R = pipeline.last.quality.photCalib.runPhotDiagnostics( ...
    %                  '/data/2025/07/08');
    %
    %          % Fit-quality only on a pre-calibrated Result:
    %          R = pipeline.last.quality.photCalib.runPhotDiagnostics(Calib, ...
    %                  'Plots', {'fitquality'});
    %
    %          % Stability + synchrony, save figures:
    %          R = pipeline.last.quality.photCalib.runPhotDiagnostics( ...
    %                  '/data/2025/07/08', ...
    %                  'Plots', {'stability','synchrony'}, ...
    %                  'OutDir', '~/results', 'SaveFig', true);

    arguments
        Input
        Args.Plots               cell    = {'fitquality','series','transmission','stability'}
        Args.MatchEpochs                 = []
        Args.CalibArgs           cell    = {}
        Args.CropsToAnalyze      double  = []
        Args.RefCrop             (1,1) double = 10
        Args.TileOrder           {mustBeTextScalar} = 'rowmajor'
        Args.MatchRadius         (1,1) double = 2
        Args.MagFields           cell    = {'MAG_PSF','MAG_APER_3'}
        Args.SeriesQuantities    cell    = {'TauAod500','PWV_cm','Center_Ang','Norm','ARMS','IntegralT'}
        Args.MosaicQuantities    cell    = {'TauAod500','PWV_cm','Norm','IntegralT'}
        Args.ParamHistQuantities cell    = {'Chi2_DOF','NCalibRetention'}
        Args.ResidualXAxis       cell    = {'mag','airmass','background','X','Y'}
        Args.OutDir              {mustBeText} = ''
        Args.SaveFig             logical = false
        Args.ForceRecalc         logical = false
        Args.Verbose             logical = false
        Args.VisitGlob           {mustBeTextScalar} = '*v*'
        Args.Recursive           logical = false
        Args.FileType            {mustBeMember(Args.FileType,{'coadd','proc'})} = 'coadd'
        Args.FieldId                     = ''
    end

    Result = struct();
    Fig    = gobjects(0);

    % --- Step 1: Calibration ------------------------------------------
    if isstruct(Input) && isscalar(Input) && isfield(Input,'PC') && isfield(Input,'Cats')
        % Pre-existing calibration Result - reuse as-is.
        Calib = Input;
        if Args.Verbose
            fprintf('runPhotDiagnostics: reusing pre-built Calib struct.\n');
        end
    else
        CacheFile = '';
        if ~isempty(Args.OutDir)
            CacheFile = fullfile(char(Args.OutDir), 'calibrateVisits.mat');
        end
        Calib = pipeline.last.quality.photCalib.calibrateVisits(Input, ...
            'VisitGlob',   Args.VisitGlob, ...
            'Recursive',   Args.Recursive, ...
            'FileType',    Args.FileType, ...
            'FieldId',     Args.FieldId, ...
            'CalibArgs',   Args.CalibArgs, ...
            'OutFile',     CacheFile, ...
            'ForceRecalc', Args.ForceRecalc, ...
            'Verbose',     Args.Verbose);
    end
    Result.Calib = Calib;

    PlotSet = lower(Args.Plots);

    % --- Step 2: Cross-match epochs (when needed) ---------------------
    DoMatch = Args.MatchEpochs;
    if isempty(DoMatch)
        DoMatch = any(ismember({'stability'}, PlotSet));
    end
    if DoMatch
        if Args.Verbose
            fprintf('runPhotDiagnostics: cross-matching epochs ...\n');
        end
        Result.MS = pipeline.last.quality.photCalib.matchEpochs(Calib.Cats, ...
            'MatchRadius', Args.MatchRadius, ...
            'MagFields',   Args.MagFields, ...
            'Verbose',     Args.Verbose);
    end

    % --- Step 3: Plot dispatch ----------------------------------------
    if any(strcmp(PlotSet,'fitquality'))
        for I = 1:numel(Args.ResidualXAxis)
            Fig = i_capture(Fig, @() ...
                pipeline.last.quality.photCalib.plotPhotResiduals(Calib.PC, ...
                    'XAxis', Args.ResidualXAxis{I}, ...
                    'CropsToAnalyze', Args.CropsToAnalyze));
        end
        % residual-vs-colour - try; needs catsHTM and may be unavailable.
        try
            Fig = i_capture(Fig, @() ...
                pipeline.last.quality.photCalib.plotPhotResidualsColor(Calib.PC, ...
                    'CropsToAnalyze', Args.CropsToAnalyze, 'Verbose', false));
        catch ME
            if Args.Verbose
                fprintf('  plotPhotResidualsColor skipped: %s\n', ME.message);
            end
        end
    end

    if any(strcmp(PlotSet,'series'))
        Fig = i_capture(Fig, @() ...
            pipeline.last.quality.photCalib.plotPhotSeries(Calib.PC, ...
                'Quantity', Args.SeriesQuantities, ...
                'HeaderData', i_headerData(Calib), ...
                'CropsToAnalyze', Args.CropsToAnalyze, ...
                'TileOrder', Args.TileOrder));
    end

    if any(strcmp(PlotSet,'mosaic'))
        Fig = i_capture(Fig, @() ...
            pipeline.last.quality.photCalib.plotPhotParamMosaic(Calib.PC, ...
                'ParamNames', Args.MosaicQuantities, ...
                'CropsToAnalyze', Args.CropsToAnalyze, ...
                'TileOrder', Args.TileOrder));
    end

    if any(strcmp(PlotSet,'synchrony'))
        Fig = i_capture(Fig, @() ...
            pipeline.last.quality.photCalib.plotPhotParamSynchrony(Calib.PC, ...
                'CropsToAnalyze', Args.CropsToAnalyze, ...
                'TileOrder', Args.TileOrder, ...
                'Verbose', Args.Verbose));
    end

    if any(strcmp(PlotSet,'transmission'))
        Fig = i_capture(Fig, @() ...
            pipeline.last.quality.photCalib.plotPhotTransmission(Calib.PC, ...
                'EpochIdx', 1, ...
                'CropsToAnalyze', Args.CropsToAnalyze, ...
                'RefCrop', Args.RefCrop));
    end

    if any(strcmp(PlotSet,'params'))
        for I = 1:numel(Args.ParamHistQuantities)
            Fig = i_capture(Fig, @() ...
                pipeline.last.quality.photCalib.plotPhotParamHist(Calib.PC, ...
                    'Param', Args.ParamHistQuantities{I}, ...
                    'CropsToAnalyze', Args.CropsToAnalyze));
        end
    end

    if any(strcmp(PlotSet,'stability'))
        if isfield(Result,'MS') && ~isempty(Result.MS)
            Fig = i_capture(Fig, @() ...
                pipeline.last.quality.photCalib.plotPhotStability(Result.MS, ...
                    'CropsToAnalyze', Args.CropsToAnalyze));
        else
            warning('photCalib:runPhotDiagnostics:NoMS', ...
                'stability requested but cross-matched MS unavailable - skipping.');
        end
    end

    Result.Fig  = Fig;
    Result.Args = Args;

    % --- Step 4: Optional save ----------------------------------------
    if Args.SaveFig && ~isempty(Args.OutDir)
        saveFigures(char(Args.OutDir), Fig, 'Verbose', Args.Verbose);
        saveResult(fullfile(char(Args.OutDir),'runPhotDiagnostics.mat'), ...
            Result, 'VarName','Result', 'Verbose', Args.Verbose);
    end
end

% =========================================================================
function NewFig = i_capture(ExistingFig, PlotFun)
    % Snapshot open figures before/after calling PlotFun and append the new ones.
    % Lets us collect figure handles from plotters that don't return them yet.
    Pre = findall(0, 'Type', 'figure');
    try
        PlotFun();
    catch ME
        warning('photCalib:runPhotDiagnostics:PlotFailed', ...
            'A plot call failed: %s', ME.message);
    end
    Post  = findall(0, 'Type', 'figure');
    Added = setdiff(Post, Pre);
    NewFig = [ExistingFig; Added(:)];
end

% =========================================================================
function HD = i_headerData(Calib)
    % Best-effort assembly of a HeaderData struct from the calibrated AI.
    % Currently empty - the new feeders do not collect a HeaderData matrix.
    % Plotters that need a header keyword (e.g. plotPhotSeries with 'FWHM')
    % should be called separately with an explicit HeaderData input.
    HD = [];
    if isfield(Calib, 'HeaderData')
        HD = Calib.HeaderData;
    end
end
