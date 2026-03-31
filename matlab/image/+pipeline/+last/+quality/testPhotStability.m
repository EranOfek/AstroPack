function Result = testPhotStability(Args)
    % Epoch-to-epoch photometric stability analysis
    % Description: Loads multi-epoch data, calibrates, cross-matches sources
    %              across epochs, and generates stability diagnostic plots.
    %              For calibration fit quality diagnostics (single-epoch),
    %              see testPhotFitQuality.
    %
    %              Plots generated:
    %                1. Mag vs Std scatter per mode.
    %                2. Std difference between modes (or AB vs CB).
    %                3. RMS & ZP mosaic.
    %                4. ZP map mosaic per mode.
    %                5. Transmission curves per mode.
    %                6. Integral T mosaic + T vs epoch time series.
    %                7. FWHM vs epoch.
    %
    % Input  : * ...,key,val,...
    %          --- Data loading ---
    %            'DataDir','OutDir','Visits','VisitDirs','ListFile',
    %            'ListFields','VisitIdx','FileType' — see loadVisitData.
    %          --- Calibration ---
    %            'Modes','RefCrop','Ncrop','ForceRecalc','CalibArgs',
    %            'ApplyConstBand','ConstBandParams',
    %            'VisitRefZP','VisitRefZPEpoch' — see calibratePhotModes.
    %          --- Epoch matching ---
    %            'CropsToAnalyze','MatchRadius','MagFields','MatchedColumns',
    %            'BadFlags','MaxMagErr','MinEpochs','ApplyRelZP'.
    %          --- Plotting ---
    %            'Plot','SaveFig','ShowOrigMag','OverlayTrend',
    %            'TrendBinWidth','TileOrder'.
    %          --- General ---
    %            'Verbose'.
    % Output : - Result struct with .PC, .Cats, .MS, .FitRMS, .ZPcenter,
    %            .HeaderData, .PersetInfo.
    % Author : D. Kovaleva (Mar 2026)
    % Example: % Default (proc files from DataDir):
    %          R = pipeline.last.quality.testPhotStability();
    %
    %          % Coadd files from visit list:
    %          R = pipeline.last.quality.testPhotStability('DataDir', '', ...
    %              'ListFile', '~/N3_M2C4Jul2_7_list.mat', ...
    %              'ListFields', {'M2C4Jul2p1'}, 'FileType', 'coadd', ...
    %              'OutDir', '~/results_coadd');
    %
    %          % Compare modes with ConstBand:
    %          CBP = PhotCalibTrans.buildConstBandParams(R.PC.percrop);
    %          R = pipeline.last.quality.testPhotStability('Modes', {'percrop'}, ...
    %              'ApplyConstBand', true, 'ConstBandParams', CBP, ...
    %              'ForceRecalc', true);
    %
    %          % Replot from cached Result:
    %          S = load('results/Result.mat'); R = S.Result;
    %          pipeline.last.quality.plotPhotScatter(R.MS, 'Modes', {'percrop'});
    %          pipeline.last.quality.plotPhotFWHM(R);
    %          pipeline.last.quality.plotPhotIntegralT(R.PC);

    arguments
        Args.DataDir        = '~/222625v1'
        Args.OutDir         = ''
        Args.Visits         = 1:20
        Args.VisitDirs      = []
        Args.ListFile       = ''
        Args.ListFields     = {}
        Args.VisitIdx       = []
        Args.FileType       = 'proc'
        Args.Modes          = {'percrop', 'shapeimage', 'perimage', 'shapeset', 'perset'}
        Args.RefCrop        = 10
        Args.Ncrop          = 24
        Args.CropsToAnalyze = []
        Args.MatchRadius    = 1
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.MatchedColumns = {'RA','Dec','X1','Y1','SN', ...
                               'MAG_AB_PSF','MAG_AB_APER_3', ...
                               'MAG_PSF','MAG_APER_3', ...
                               'MAGERR_PSF','MAGERR_APER_3','FLAGS'}
        Args.BadFlags       = {'Saturated','NearEdge','Overlap'}
        Args.MaxMagErr      = 0.02
        Args.MinEpochs      = 3
        Args.ApplyRelZP logical = false
        Args.ForceRecalc logical = false
        Args.CalibArgs cell = {}
        Args.ApplyConstBand logical = false
        Args.ConstBandParams = []
        Args.Plot logical   = true
        Args.SaveFig logical = false
        Args.ShowOrigMag logical = true
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
                    CBP = PhotCalibTrans.buildConstBandParams(S.PC_all, 'Verbose', Args.Verbose);
                end
            end
        elseif iscell(Args.ConstBandParams)
            CBP = PhotCalibTrans.buildConstBandParams(Args.ConstBandParams, 'Verbose', Args.Verbose);
        end
        if isempty(CBP)
            warning('testPhotStability:NoCBP', 'Cannot build ConstBandParams. Disabling.');
            Args.ApplyConstBand = false;
        end
    end

    % === Load ===
    AI = pipeline.last.quality.loadVisitData( ...
        'DataDir', Args.DataDir, ...
        'Visits', Args.Visits, ...
        'VisitDirs', Args.VisitDirs, ...
        'ListFile', Args.ListFile, ...
        'ListFields', Args.ListFields, ...
        'VisitIdx', Args.VisitIdx, ...
        'FileType', Args.FileType, ...
        'Verbose', Args.Verbose);

    Nvisits = numel(AI);
    if numel(Args.Visits) ~= Nvisits
        Args.Visits = 1:Nvisits;
    end

    % Extract headers
    HeaderData = pipeline.last.quality.extractHeaderData(AI, ...
        'Ncrop', Args.Ncrop, 'Verbose', Args.Verbose);

    % === Calibrate ===
    Calib = pipeline.last.quality.calibratePhotModes(AI, ...
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
    MS = pipeline.last.quality.matchPhotEpochs(Calib.Cats, ...
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

    pipeline.last.quality.plotPhotScatter(MS, ...
        'Modes', Args.Modes, ...
        'MagFields', Args.MagFields, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'ShowOrigMag', Args.ShowOrigMag, ...
        'OverlayTrend', Args.OverlayTrend, ...
        'TrendBinWidth', Args.TrendBinWidth, ...
        'MinEpochs', Args.MinEpochs);

    pipeline.last.quality.plotPhotStdDiff(MS, ...
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
            pipeline.last.quality.plotPhotStdDiff(MS, ...
                'Modes', Args.Modes, ...
                'CompareFields', {AbFields{If}, CbFields{If}}, ...
                'CropsToAnalyze', Args.CropsToAnalyze, ...
                'OverlayTrend', Args.OverlayTrend, ...
                'TrendBinWidth', Args.TrendBinWidth, ...
                'MinEpochs', Args.MinEpochs);
        end
    end

    pipeline.last.quality.plotPhotMosaic(Calib, ...
        'Modes', Args.Modes, ...
        'Visits', Args.Visits, ...
        'Ncrop', Args.Ncrop, ...
        'RefCrop', Args.RefCrop, ...
        'TileOrder', Args.TileOrder);

    pipeline.last.quality.plotPhotTransmission(Calib, ...
        'Modes', Args.Modes, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'RefCrop', Args.RefCrop);

    pipeline.last.quality.plotPhotIntegralT(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
        'TileOrder', Args.TileOrder);

    pipeline.last.quality.plotPhotFWHM(HeaderData, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
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
