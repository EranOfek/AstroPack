function Result = testPhotFitQuality(Input, Args)
    % Calibration fit quality diagnostics
    % Description: Runs diagnostic plots for photometric calibration quality.
    %              Does NOT require cross-matching across epochs or multiple
    %              epochs. Accepts either:
    %                - PC struct with PC.percrop{Iv}(Ic) (from testPhotCalib Result)
    %                - Full Result struct (from Result.mat)
    %                - AstroImage array (calibrates internally with fitPhotCalibTrans)
    %                - Cell array of AstroImage arrays (multi-epoch, calibrates all)
    %
    %              Plots generated:
    %                1. Calibrator residuals vs magnitude.
    %                2. Calibrator residual RMS vs magnitude.
    %                3. Calibrator residuals vs GAIA BP-RP color (+mag-color).
    %                4. Calibrator residuals vs 1/Flux (background check).
    %                5. Calibrator residuals vs X-XPEAK, Y-YPEAK.
    %                6. Calibrator residuals vs airmass.
    %                7. Calibrator residuals vs any user-specified feature.
    %
    % Input  : - PC struct, Result struct, AstroImage array, or cell of
    %            AstroImage arrays.
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'OverlayTrend'   - 'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'  - Bin width [mag]. Default is 0.5.
    %            'ExtraXFields'   - Additional XField values for plotPhotResidualsVs.
    %                               Cell array. Default is {}.
    %            'PlotColor'      - Plot residuals vs GAIA color. Default is true.
    %            'RefCrop'        - Reference crop for calibration. Default is 10.
    %            'CalibArgs'      - Extra args for fitPhotCalibTrans (AstroImage input).
    %                               Default is {}.
    %            'ApplyConstBand' - Apply constant-band correction. Default is false.
    %            'ConstBandParams'- CBP struct or .mat path. Default is [].
    %            'SaveFig'        - Save figures to OutDir/figures/. Default is false.
    %            'OutDir'         - Output directory for figures. Default is pwd.
    %            'Verbose'        - Print progress. Default is true.
    % Output : - Result struct with .PC field (for reuse).
    % Author : D. Kovaleva (Mar 2026)
    % Example: % From cached Result.mat:
    %          S = load('results/Result.mat'); R = S.Result;
    %          pipeline.last.quality.testPhotFitQuality(R);
    %
    %          % From PC struct (e.g., from testPhotStability output):
    %          pipeline.last.quality.testPhotFitQuality(R.PC);
    %
    %          % From a single AstroImage (24 crops, calibrates internally):
    %          pipeline.last.quality.testPhotFitQuality(AI);
    %
    %          % Multi-epoch AstroImage cell (calibrates all epochs):
    %          pipeline.last.quality.testPhotFitQuality(AI_cell);
    %
    %          % With extra diagnostic fields:
    %          pipeline.last.quality.testPhotFitQuality(R.PC, ...
    %              'ExtraXFields', {'FLAGS', 'PSF_CHI2DOF', 'SN'});
    %
    %          % Skip GAIA color plot (no catsHTM needed):
    %          pipeline.last.quality.testPhotFitQuality(R.PC, 'PlotColor', false);
    %
    %          % Save figures:
    %          pipeline.last.quality.testPhotFitQuality(R.PC, ...
    %              'SaveFig', true, 'OutDir', '~/results');
    %
    %          % Individual plot functions (called internally):
    %          pipeline.last.quality.plotPhotResiduals(R.PC);
    %          pipeline.last.quality.plotPhotResidualsRMS(R.PC);
    %          pipeline.last.quality.plotPhotResidualsColor(R.PC, 'PlotMagColor', true);
    %          pipeline.last.quality.plotPhotResidualsBg(R.PC);
    %          pipeline.last.quality.plotPhotResidualsVs(R.PC, 'XField', 'FLAGS');
    %          pipeline.last.quality.plotPhotResidualsAirmass(R.PC);

    arguments
        Input
        Args.CropsToAnalyze = []
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
        Args.ExtraXFields cell = {}
        Args.PlotColor logical = true
        Args.RefCrop        = 10
        Args.CalibArgs cell = {}
        Args.ApplyConstBand logical = false
        Args.ConstBandParams = []
        Args.SaveFig logical = false
        Args.OutDir         = pwd
        Args.Verbose logical = true
    end

    % === Resolve input → PC struct ===
    PC = resolveInputPC(Input, Args);
    if isempty(PC)
        warning('testPhotFitQuality:NoPC', 'Could not obtain PC objects.');
        return;
    end

    Result.PC = PC;

    % === Diagnostic plots ===
    if Args.Verbose
        fprintf('\n=== Fit Quality Diagnostics ===\n');
    end

    % 1. Residuals vs magnitude
    pipeline.last.quality.plotPhotResiduals(PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend, ...
        'TrendBinWidth', Args.TrendBinWidth);

    % 2. Residual RMS vs magnitude
    pipeline.last.quality.plotPhotResidualsRMS(PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze);

    % 3. Residuals vs GAIA color
    if Args.PlotColor
        pipeline.last.quality.plotPhotResidualsColor(PC, ...
            'CropsToAnalyze', Args.CropsToAnalyze, ...
            'OverlayTrend', Args.OverlayTrend, ...
            'Verbose', Args.Verbose);
    end

    % 4. Residuals vs 1/Flux (background check)
    pipeline.last.quality.plotPhotResidualsBg(PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend);

    % 5. Residuals vs X-XPEAK, Y-YPEAK
    pipeline.last.quality.plotPhotResidualsVs(PC, ...
        'XField', 'X-XPEAK', ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend);
    pipeline.last.quality.plotPhotResidualsVs(PC, ...
        'XField', 'Y-YPEAK', ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend);

    % 6. Residuals vs airmass
    pipeline.last.quality.plotPhotResidualsAirmass(PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend);

    % 7. Extra user-specified fields
    for If = 1:numel(Args.ExtraXFields)
        pipeline.last.quality.plotPhotResidualsVs(PC, ...
            'XField', Args.ExtraXFields{If}, ...
            'CropsToAnalyze', Args.CropsToAnalyze, ...
            'OverlayTrend', Args.OverlayTrend);
    end

    % === Save figures ===
    if Args.SaveFig
        FigDir = fullfile(Args.OutDir, 'figures');
        if ~exist(FigDir, 'dir'); mkdir(FigDir); end
        AllFigs = findall(0, 'Type', 'figure');
        for If = 1:numel(AllFigs)
            Fig = AllFigs(If);
            if ~isvalid(Fig); continue; end
            FigName = matlab.lang.makeValidName(Fig.Name);
            if isempty(FigName); FigName = sprintf('fig_%d', Fig.Number); end
            savefig(Fig, fullfile(FigDir, [FigName '.fig']));
            saveas(Fig, fullfile(FigDir, [FigName '.jpg']));
        end
        if Args.Verbose
            fprintf('Saved %d figures to %s\n', numel(AllFigs), FigDir);
        end
    end
end

% =========================================================================
function PC = resolveInputPC(Input, Args)
    % Resolve various input types into a PC struct with .percrop field
    PC = [];

    if isstruct(Input)
        if isfield(Input, 'PC')
            % Full Result struct
            PC = Input.PC;
        elseif isfield(Input, 'percrop')
            % PC struct directly
            PC = Input;
        end
    elseif isa(Input, 'AstroImage')
        % Single AstroImage array (one epoch, multiple crops)
        PC = calibrateSingle(Input, Args);
    elseif iscell(Input)
        % Cell array of AstroImage arrays (multi-epoch)
        PC = calibrateMulti(Input, Args);
    end
end

% =========================================================================
function PC = calibrateSingle(AI, Args)
    % Calibrate a single AstroImage array → PC struct
    if Args.Verbose
        fprintf('Calibrating single epoch (%d crops)...\n', numel(AI));
    end

    CalibArgs = Args.CalibArgs;
    if Args.ApplyConstBand && ~isempty(Args.ConstBandParams)
        CalibArgs = [CalibArgs, ...
            'ApplyConstBand', true, 'ConstBandParams', Args.ConstBandParams];
    end

    [~, PCarray] = imProc.calib.fitPhotCalibTrans(AI, ...
        'PhotSys', 'percrop', 'RefCrop', Args.RefCrop, ...
        'Verbose', Args.Verbose, CalibArgs{:});

    PC.percrop = {PCarray};
end

% =========================================================================
function PC = calibrateMulti(AIcell, Args)
    % Calibrate multiple epochs → PC struct
    Nvisits = numel(AIcell);
    if Args.Verbose
        fprintf('Calibrating %d epochs...\n', Nvisits);
    end

    CalibArgs = Args.CalibArgs;
    if Args.ApplyConstBand && ~isempty(Args.ConstBandParams)
        CalibArgs = [CalibArgs, ...
            'ApplyConstBand', true, 'ConstBandParams', Args.ConstBandParams];
    end

    PC_all = cell(Nvisits, 1);
    for Iv = 1:Nvisits
        if isempty(AIcell{Iv}); continue; end
        AIcopy = AIcell{Iv}.copy();
        [~, PC_all{Iv}] = imProc.calib.fitPhotCalibTrans(AIcopy, ...
            'PhotSys', 'percrop', 'RefCrop', Args.RefCrop, ...
            'Verbose', false, CalibArgs{:});
        if Args.Verbose
            fprintf('  Epoch %d: %d/%d success\n', Iv, ...
                sum([PC_all{Iv}.Success]), numel(PC_all{Iv}));
        end
    end
    PC.percrop = PC_all;
end
