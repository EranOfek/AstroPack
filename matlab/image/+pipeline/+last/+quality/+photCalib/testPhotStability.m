function Result = testPhotStability(Args)
    % Epoch-to-epoch photometric stability analysis
    % Description: Loads multi-epoch data, calibrates, cross-matches sources
    %              across epochs, and generates stability diagnostic plots.
    %              For calibration fit quality diagnostics (single-epoch),
    %              see testPhotFitQuality.
    %
    %              Plots generated:
    %                1. Mag vs Std scatter per mode (and an extra scatter
    %                   from MergedMat when MergedMatDir is given).
    %                2. Std difference between modes (or AB vs CB).
    %                3. RMS & ZP mosaic.
    %                4. ZP map mosaic per mode.
    %                5. Transmission curves per mode.
    %                6. Integral T mosaic + T vs epoch time series.
    %                7. FWHM vs epoch.
    %                8. Fitted atmospheric parameters vs epoch.
    %                9. Asymptotic RMS (PT_ARMS vs PH_RMS) vs epoch.
    %
    % Input  : * ...,key,val,...
    %          --- Data loading ---
    %            'DataDir','OutDir','Visits','VisitDirs','ListFile',
    %            'ListFields','VisitIdx','FileType' — see pipeline.last.load.loadVisitCatHdr.
    %            'FileType' accepts 'proc' (default) or 'coadd'.
    %            'MergedMatDir' — directory containing MergedMat HDF5
    %                             files; loaded into Result.MS_merged
    %                             ('' = skip, default).
    %          --- Calibration ---
    %            'Modes','RefCrop','Ncrop','ForceRecalc','CalibArgs',
    %            'ApplyConstBand','ConstBandParams',
    %            'VisitRefZP','VisitRefZPEpoch' — see calibratePhotModes.
    %          --- Epoch matching ---
    %            'CropsToAnalyze','MatchRadius','MagFields','MatchedColumns',
    %            'BadFlags','MaxMagErr','MinEpochs','ApplyRelZP'.
    %          --- Plotting ---
    %            'Plot','SaveFig','LayoutMode','OverlayTrend',
    %            'TrendBinWidth','TileOrder'.
    %            'LayoutMode' is forwarded to plotPhotScatter:
    %            'PerQuantity' (default) or 'Combined'.
    %          --- General ---
    %            'Verbose'.
    % Output : - Result struct with .PC, .Cats, .MS, .FitRMS, .ZPcenter,
    %            .HeaderData, .PersetInfo. .MS_merged is added when
    %            MergedMatDir is non-empty.
    % Author : D. Kovaleva (Mar 2026)
    % Example: % Default (MergedMat + propagation, auto-detected):
    %          R = pipeline.last.quality.photCalib.testPhotStability('DataDir', '~/222625v1');
    %
    %          % Explicit MergedMat directory:
    %          R = pipeline.last.quality.photCalib.testPhotStability('DataDir', '~/222625v1', ...
    %              'MergedMatDir', '~/222625v1');
    %
    %          % Force full re-calibration and re-matching (skip caches):
    %          R = pipeline.last.quality.photCalib.testPhotStability('DataDir', '~/222625v1', ...
    %              'ForceRecalc', true);
    %
    %          % Coadd files from visit list:
    %          R = pipeline.last.quality.photCalib.testPhotStability('DataDir', '', ...
    %              'ListFile', '~/N3_M2C4Jul2_7_list.mat', ...
    %              'ListFields', {'M2C4Jul2p1'}, 'FileType', 'coadd', ...
    %              'OutDir', '~/results_coadd');
    %
    %          % ConstBand mode:
    %          CBP = [R.PC.percrop{:}].buildConstBandParams();
    %          R = pipeline.last.quality.photCalib.testPhotStability('Modes', {'percrop'}, ...
    %              'ApplyConstBand', true, 'ConstBandParams', CBP, ...
    %              'ForceRecalc', true);
    %
    %          % Replot from cached Result:
    %          S = load('results/Result.mat'); R = S.Result;
    %          pipeline.last.quality.photCalib.plotPhotScatter(R.MS, 'Modes', {'percrop'});
    %          pipeline.last.quality.photCalib.plotPhotFWHM(R);
    %          pipeline.last.quality.photCalib.plotPhotIntegralT(R.PC);
    %
    %          % Restrict to a single observation field (token after '_clear_'):
    %          R = pipeline.last.quality.photCalib.testPhotStability('DataDir', '~/222625v1', ...
    %                  'FieldId', '1781');

    arguments
        Args.DataDir        = '~/222625v1'
        Args.OutDir         = ''
        Args.Visits         = 1:20
        Args.VisitDirs      = []
        Args.ListFile       = ''
        Args.ListFields     = {}
        Args.VisitIdx       = []
        Args.FileType       = 'proc'
        Args.FieldId        = ''      % see pipeline.last.load.loadVisitCatHdr
        Args.CropID         double {mustBeInteger, mustBeNonnegative} = []
        Args.Modes          = {'percrop'}
        Args.RefCrop        = 10
        Args.Ncrop          = 24
        Args.CropsToAnalyze = []
        Args.MatchRadius    = 3
        Args.MagFields      = {'MAG_AB_APER_3', 'MAG_AB_PSF'}
        Args.MatchedColumns = {'RA','Dec','X1','Y1','SN', ...
                               'MAG_AB_PSF','MAG_AB_APER_3', ...
                               'MAG_PSF','MAG_APER_3', ...
                               'MAGERR_PSF','MAGERR_APER_3','FLAGS'}
        Args.BadFlags       = {'Saturated','NearEdge','Overlap'}
        Args.MaxMagErr      = 0.02
        Args.MinEpochs      = 2
        Args.ApplyRelZP logical = false
        Args.MergedMatDir = ''   % Load MergedMat HDF5 files into R.MS_merged ('' = skip)
        Args.ForceRecalc logical = false
        Args.CalibArgs cell = {}
        Args.ApplyConstBand logical = false
        Args.ConstBandParams = []
        Args.Plot logical   = true
        Args.SaveFig logical = true
        Args.LayoutMode     {mustBeMember(Args.LayoutMode, ...
                              {'PerQuantity','Combined'})} = 'PerQuantity'
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
        Args.TileOrder      = 'rowmajor'
        Args.VisitRefZP     = 'epoch'
        Args.VisitRefZPEpoch = 1
        Args.Verbose logical = true
    end

    % === Setup ===
    if isempty(Args.OutDir)
        if ~isempty(Args.VisitDirs) || ~isempty(Args.ListFile)
            Args.OutDir = fullfile(pwd, 'results');
        elseif ~isempty(Args.DataDir)
            Args.OutDir = fullfile(Args.DataDir, 'results');
        else
            Args.OutDir = fullfile(pwd, 'results');
        end
    end
    if ~exist(Args.OutDir, 'dir')
        mkdir(Args.OutDir);
    end
    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end

    % Expand for ConstBand
    CBP = [];
    if Args.ApplyConstBand
        CbFields = strrep(Args.MagFields, 'MAG_AB_', 'MAG_CB_');
        Args.MagFields = [Args.MagFields, CbFields];
        Args.MatchedColumns = [Args.MatchedColumns, CbFields];

        if isstruct(Args.ConstBandParams)
            CBP = Args.ConstBandParams;
        elseif ischar(Args.ConstBandParams) || isstring(Args.ConstBandParams)
            if ~isempty(Args.ConstBandParams)
                S = load(char(Args.ConstBandParams));
                if isfield(S, 'CBP')
                    CBP = S.CBP;
                elseif isfield(S, 'PC_all')
                    PCflat = [S.PC_all{:}];   % flatten cell-of-arrays to flat array
                    CBP = PCflat.buildConstBandParams('Verbose', Args.Verbose);
                end
            end
        elseif iscell(Args.ConstBandParams)
            PCflat = [Args.ConstBandParams{:}];
            CBP = PCflat.buildConstBandParams('Verbose', Args.Verbose);
        end
        if isempty(CBP)
            warning('testPhotStability:NoCBP', 'Cannot build ConstBandParams. Disabling.');
            Args.ApplyConstBand = false;
        end
    end

    % === Load ===
    AI = pipeline.last.load.loadVisitCatHdr( ...
        'DataDir', Args.DataDir, ...
        'Visits', Args.Visits, ...
        'VisitDirs', Args.VisitDirs, ...
        'ListFile', Args.ListFile, ...
        'ListFields', Args.ListFields, ...
        'VisitIdx', Args.VisitIdx, ...
        'FileType', Args.FileType, ...
        'FieldId',  Args.FieldId, ...
        'CropID',   Args.CropID, ...
        'Verbose', Args.Verbose);

    Nvisits = numel(AI);
    if numel(Args.Visits) ~= Nvisits
        Args.Visits = 1:Nvisits;
    end

    % Extract headers
    HeaderData = pipeline.last.load.extractHeaderData(AI, ...
        'Ncrop', Args.Ncrop, 'Verbose', Args.Verbose);

    % === Calibrate ===
    Calib = pipeline.last.quality.photCalib.calibratePhotModes(AI, ...
        'Modes', Args.Modes, ...
        'Visits', Args.Visits, ...
        'RefCrop', Args.RefCrop, ...
        'Ncrop', Args.Ncrop, ...
        'OutDir', Args.OutDir, ...
        'ForceRecalc', Args.ForceRecalc, ...
        'CalibArgs', Args.CalibArgs, ...
        'ApplyConstBand', Args.ApplyConstBand, ...
        'ConstBandParams', CBP, ...
        'VisitRefZP', Args.VisitRefZP, ...
        'VisitRefZPEpoch', Args.VisitRefZPEpoch, ...
        'MagFields', Args.MagFields, ...
        'Verbose', Args.Verbose);

    % === Epoch Matching ===
    MS = pipeline.last.quality.photCalib.matchPhotEpochs(Calib.Cats, ...
        'Modes', Args.Modes, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
        'MatchRadius', Args.MatchRadius, ...
        'MatchedColumns', Args.MatchedColumns, ...
        'MagFields', Args.MagFields, ...
        'BadFlags', Args.BadFlags, ...
        'MaxMagErr', Args.MaxMagErr, ...
        'MinEpochs', Args.MinEpochs, ...
        'ApplyRelZP', Args.ApplyRelZP, ...
        'OutDir', Args.OutDir, ...
        'ForceRecalc', Args.ForceRecalc, ...
        'Verbose', Args.Verbose);

    % === Load MergedMat (optional) ===
    MS_merged = [];
    if ~isempty(Args.MergedMatDir)
        if Args.Verbose
            fprintf('\n=== Loading MergedMat ===\n');
        end
        MS_merged = pipeline.last.load.loadMergedMat( ...
            'MergedMatDir', Args.MergedMatDir, ...
            'CropsToAnalyze', Args.CropsToAnalyze, ...
            'Ncrop', Args.Ncrop, ...
            'Verbose', Args.Verbose);
    end

    % === Assemble Result ===
    Result.PC         = Calib.PC;
    Result.Cats       = Calib.Cats;
    Result.FitRMS     = Calib.FitRMS;
    Result.ZPcenter   = Calib.ZPcenter;
    if isfield(Calib, 'PersetInfo')
        Result.PersetInfo = Calib.PersetInfo;
    end
    Result.MS         = MS;
    Result.HeaderData = HeaderData;
    if ~isempty(MS_merged)
        Result.MS_merged = MS_merged;
    end

    % === Save ===
    ResultFile = fullfile(Args.OutDir, 'Result.mat');
    try
        save(ResultFile, 'Result', '-v7.3');
        if Args.Verbose
            fprintf('Saved %s\n', ResultFile);
        end
    catch ME
        warning('testPhotStability:SaveFailed', ...
            'Failed to save %s: %s', ResultFile, ME.message);
    end

    % === Plots ===
    if ~Args.Plot
        return;
    end

    % MergedMat scatter (relative photometry from pipeline matching)
    if ~isempty(MS_merged)
        pipeline.last.quality.photCalib.plotPhotScatter(MS_merged, ...
            'Modes', {'percrop'}, ...
            'MagFields', {'MAG_APER_3'}, ...
            'LayoutMode', Args.LayoutMode, ...
            'CropsToAnalyze', Args.CropsToAnalyze, ...
            'OverlayTrend', Args.OverlayTrend, ...
            'TrendBinWidth', Args.TrendBinWidth, ...
            'MinEpochs', Args.MinEpochs);
    end

    pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
        'Modes', Args.Modes, ...
        'MagFields', Args.MagFields, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'LayoutMode', Args.LayoutMode, ...
        'OverlayTrend', Args.OverlayTrend, ...
        'TrendBinWidth', Args.TrendBinWidth, ...
        'MinEpochs', Args.MinEpochs);

    pipeline.last.quality.photCalib.plotPhotStdDiff(MS, ...
        'Modes', Args.Modes, ...
        'MagFields', Args.MagFields, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend, ...
        'TrendBinWidth', Args.TrendBinWidth, ...
        'MinEpochs', Args.MinEpochs);

    % ConstBand: AB vs CB
    if Args.ApplyConstBand
        AbFields = Args.MagFields(contains(Args.MagFields, '_AB_'));
        CbFields = strrep(AbFields, '_AB_', '_CB_');
        for If = 1:numel(AbFields)
            pipeline.last.quality.photCalib.plotPhotStdDiff(MS, ...
                'Modes', Args.Modes, ...
                'CompareFields', {AbFields{If}, CbFields{If}}, ...
                'CropsToAnalyze', Args.CropsToAnalyze, ...
                'OverlayTrend', Args.OverlayTrend, ...
                'TrendBinWidth', Args.TrendBinWidth, ...
                'MinEpochs', Args.MinEpochs);
        end
    end

    pipeline.last.quality.photCalib.plotPhotMosaic(Calib, ...
        'Modes', Args.Modes, ...
        'Visits', Args.Visits, ...
        'Ncrop', Args.Ncrop, ...
        'RefCrop', Args.RefCrop, ...
        'TileOrder', Args.TileOrder);

    pipeline.last.quality.photCalib.plotPhotTransmission(Calib, ...
        'Modes', Args.Modes, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'RefCrop', Args.RefCrop);

    pipeline.last.quality.photCalib.plotPhotIntegralT(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
        'TileOrder', Args.TileOrder);

    pipeline.last.quality.photCalib.plotPhotFWHM(HeaderData, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
        'TileOrder', Args.TileOrder);

    pipeline.last.quality.photCalib.plotPhotAsympRMS(Calib.PC, ...
        'HeaderData', HeaderData, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
        'TileOrder', Args.TileOrder);

    pipeline.last.quality.photCalib.plotPhotFittedParams(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'TileOrder', Args.TileOrder);

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
