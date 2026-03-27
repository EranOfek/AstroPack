function Result = testPhotCalib(Args)
    % Compare PhotSys modes by epoch-to-epoch photometric repeatability
    % Description: Orchestrator that loads visit data, calibrates all requested
    %              modes, matches sources across epochs, and generates diagnostic
    %              plots. Each step is delegated to a standalone function in
    %              +pipeline.+last.+quality:
    %                loadVisitData      — load FITS into AstroImage arrays
    %                calibratePhotModes — run calibration, compute FitRMS/ZPcenter
    %                matchPhotEpochs    — cross-epoch source matching
    %                plotPhotScatter    — mag vs std scatter
    %                plotPhotStdDiff    — std difference (percrop vs others)
    %                plotPhotMosaic     — RMS/ZP mosaics and ZP maps
    %                plotPhotResiduals  — calibrator fit residuals vs magnitude
    %                plotPhotResidualsRMS — calibrator residual RMS vs magnitude
    %                plotPhotResidualsColor — calibrator residuals vs GAIA BP-RP color
    %                plotPhotResidualsBg — residuals vs 1/Flux (additive background check)
    %                plotPhotResidualsXY — residuals vs X, Y position
    %                plotPhotResidualsAirmass — residuals vs airmass
    %                plotPhotTransmission — transmission curves per mode
    %                plotPhotIntegralT  — integral T mosaic + T vs epoch time series
    %
    %              Calibration results are cached in OutDir as PC_<mode>.mat;
    %              delete or use ForceRecalc=true to recompute.
    %
    %              Plots generated (when Plot=true):
    %                1. Mag vs Std scatter — epoch-to-epoch std vs magnitude per mode.
    %                2. Std difference — Std(percrop) - Std(other) vs magnitude.
    %                3. RMS & ZP mosaic — median fit RMS and center ZP std per crop.
    %                4. ZP map mosaic — side-by-side ZP maps for first visit.
    %                   Per-epoch modes show spatial ZP from PC objects.
    %                   Visit-level modes evaluate ZP with adjusted params.
    %                5. Calibrator residuals vs magnitude (percrop only).
    %                6. Calibrator residual RMS vs magnitude (binned RMS + median).
    %                7. Calibrator residuals vs GAIA BP-RP color (+optional mag-color).
    %                8. Calibrator residuals vs 1/Flux (additive background check,
    %                   with linear fit and bg estimate; top axis shows MAG_AB).
    %                9. Calibrator residuals vs X, Y position.
    %               10. Calibrator residuals vs airmass (median shift + linear fit).
    %               11. Transmission curves per mode (per-crop + reference overlay).
    %               12. Integral T mosaic (per crop) + T vs epoch time series.
    %
    % Input  : * ...,key,val,...
    %          --- Data loading (see also loadVisitData) ---
    %            'DataDir' - Directory with all visits' FITS files in one folder.
    %                        Default is '/home/dana/222625v1'.
    %            'OutDir'  - Directory for cached results. Default is DataDir/results.
    %            'Visits'  - Vector of visit indices (DataDir mode). Default is 1:20.
    %            'VisitDirs' - String array of visit folder paths. When non-empty,
    %                        overrides DataDir mode. Default is [].
    %            'ListFile' - Path to .mat file containing visit folder lists
    %                        (string arrays). Default is ''.
    %            'ListFields' - Field name(s) to read from ListFile. String or
    %                        cell array. Default is {} (all fields, concatenated).
    %            'VisitIdx' - Indices into the folder list to load.
    %                        Default is [] (all folders).
    %            'FileType' - 'proc' or 'coadd'. Default is 'proc'.
    %          --- Calibration ---
    %            'Modes'   - Cell array of PhotSys modes:
    %                        Per-epoch (each epoch calibrated independently):
    %                          'percrop'      - per-crop transmission (default pipeline)
    %                          'shapeimage'   - RefCrop spectral shape, per-crop Norm + Tran2D
    %                          'perimage'     - RefCrop full params, center-normalized Tran2D
    %                          'perimage_raw' - same without Tran2D center-normalization
    %                        Visit-level (single transmission from all epochs):
    %                          'shapeset'     - visit-averaged shape, per-crop Norm + Tran2D
    %                          'perset'       - visit-averaged shape, adjusted Norm, center-normalized Tran2D
    %                          'perset_raw'   - same without Tran2D center-normalization
    %                        Default is {'percrop','shapeimage','perimage','perimage_raw'}.
    %            'RefCrop' - Reference crop (0=weighted mean). Default is 10.
    %            'Ncrop'   - Number of crops per visit. Default is 24.
    %            'ForceRecalc' - Recalculate even if cached. Default is false.
    %            'CalibArgs'   - Extra args for fitPhotCalibTrans. Default is {}.
    %            'VisitRefZP'  - ZP norm for perset/perset_raw modes:
    %                        'crop_median'|'crop_mean'|'global_median'|'global_mean'|'epoch'.
    %                        Default is 'epoch'.
    %            'VisitRefZPEpoch' - Epoch index for VisitRefZP='epoch'. Default is 1.
    %          --- Epoch matching ---
    %            'CropsToAnalyze' - Subset of crops for matching/plots.
    %                        Default is [] (all).
    %            'MatchRadius' - Matching radius [arcsec]. Default is 3.
    %            'MagFields'   - AB magnitude columns. Default is
    %                        {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'MatchedColumns' - Columns propagated into MatchedSources.
    %            'BadFlags' - Flags for setBadPhotToNan. Default is
    %                        {'Saturated','NearEdge','Overlap'}.
    %            'MaxMagErr' - Max mag error for filtering. Default is 0.02.
    %            'MinEpochs' - Min non-NaN epochs per source. Sources with
    %                        fewer valid detections are NaN-ed out. 0 = no
    %                        filter. Default is 0.
    %            'ApplyRelZP' - Apply zp_meddiff relative ZP correction to
    %                        original (non-AB) magnitudes. Default is true.
    %          --- Plotting ---
    %            'Plot'     - Generate diagnostic plots. Default is true.
    %            'ShowOrigMag' - Overlay instrumental mag scatter. Default is true.
    %            'OverlayTrend'- Binned trend: 'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'- Bin width [mag]. Default is 0.5.
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    %          --- General ---
    %            'Verbose'  - Print progress. Default is true.
    % Output : - Result struct with fields:
    %            .PC       - struct per mode with PC_all{Nvisits}(1xNcrop)
    %            .Cats     - struct per mode with Cats_all{Nvisits}(1xNcrop)
    %            .MS       - struct per mode with MS{Ncrop} MatchedSources
    %            .FitRMS   - [Nvisits x Ncrop] fit RMS matrix
    %            .ZPcenter - struct per mode with [Nvisits x Ncrop] center ZP.
    %                        Per-epoch modes share percrop values;
    %                        perset modes store the effective target ZP;
    %                        shapeset stores percrop values (per-crop Norm).
    %            .PersetInfo - struct per visit-level mode with VisitRefParams,
    %                        NormIdx, TargetZP, ZPcenterPercrop, DoNormTran2D,
    %                        IsShapeset. Used by plotPhotMosaic for ZP rendering.
    %          The full Result struct is also saved as OutDir/Result.mat
    %          (before plotting, so it survives plot errors).
    % Author : D. Kovaleva (Mar 2026)
    % Example: % DataDir mode (original):
    %          R = pipeline.last.quality.testPhotCalib();
    %
    %          % Compare modes on specific crops:
    %          R = pipeline.last.quality.testPhotCalib('Modes', {'percrop','perimage'}, ...
    %              'CropsToAnalyze', [10 19]);
    %
    %          % Visit-level modes:
    %          R = pipeline.last.quality.testPhotCalib('Modes', ...
    %              {'percrop','perset'}, 'VisitRefZP', 'crop_median');
    %          R = pipeline.last.quality.testPhotCalib('Modes', ...
    %              {'percrop','shapeset'});
    %
    %          % Load coadd files from .mat visit list:
    %          R = pipeline.last.quality.testPhotCalib('DataDir', '', ...
    %              'ListFile', '/home/dana/N3_M2C4Jul2_7_list.mat', ...
    %              'ListFields', 'M2C4Jul2p1', 'FileType', 'coadd', ...
    %              'OutDir', '/home/dana/results_coadd');
    %
    %          % Explicit visit directories, coadd files:
    %          R = pipeline.last.quality.testPhotCalib('VisitDirs', ...
    %              ["/path/to/visit1", "/path/to/visit2"], 'FileType', 'coadd');
    %
    %          % Skip zp_meddiff on original mags (requires ForceRecalc):
    %          R = pipeline.last.quality.testPhotCalib('ApplyRelZP', false, ...
    %              'ForceRecalc', true);
    %
    %          % --- Standalone replotting from cached results ---
    %          S = load('results/MS_all.mat');
    %          Modes = {'percrop','perimage'};
    %          pipeline.last.quality.plotPhotScatter(S.MS_all, 'Modes', Modes, 'MinEpochs', 15);
    %          pipeline.last.quality.plotPhotStdDiff(S.MS_all, 'Modes', Modes, 'MinEpochs', 15);
    %
    %          % Load full Result from saved .mat (includes PersetInfo):
    %          S = load('results/Result.mat'); R = S.Result;
    %
    %          % Standalone calibrator diagnostics:
    %          pipeline.last.quality.plotPhotResiduals(R.PC);
    %          pipeline.last.quality.plotPhotResidualsRMS(R.PC);
    %          pipeline.last.quality.plotPhotResidualsColor(R.PC, 'PlotMagColor', true);
    %          pipeline.last.quality.plotPhotResidualsBg(R.PC);
    %          pipeline.last.quality.plotPhotResidualsXY(R.PC, 'Normalize', true);
    %          pipeline.last.quality.plotPhotResidualsAirmass(R.PC);
    %
    %          % Transmission plots (R struct needed for visit-level modes):
    %          pipeline.last.quality.plotPhotTransmission(R, ...
    %              'Modes', {'percrop','perset'});
    %          pipeline.last.quality.plotPhotIntegralT(R.PC);
    %          pipeline.last.quality.plotPhotIntegralT(R.PC, 'MosaicEpoch', 5);
    %
    %          % Replotting with other arguments (within the scope of performed calibration):
    %
    %{         
                 S = load('/home/dana/results_coadd/MS_all.mat');
                 Modes = {'percrop', 'perset'};
                 pipeline.last.quality.plotPhotScatter(S.MS_all, 'Modes', Modes, 'MinEpochs', 15);
                 pipeline.last.quality.plotPhotStdDiff(S.MS_all, 'Modes', Modes, 'MinEpochs', 15);
    %}
    %
    %{
                 R = pipeline.last.quality.testPhotCalib('DataDir', '', ...
                    'ListFile', '~/N3_M2C4Jul2_7_list.mat', ...
                    'ListFields', {'M2C4Jul2p1'},  'FileType', 'coadd', ...
                    'OutDir', '/home/dana/results_coadd', 'TileOrder', 'colmajor',...
                    'ForceRecalc', true, 'Modes', {'percrop','perimage', 'perset'},...
                    'VisitRefZP', 'global_median');
    %}
    %
                 
    arguments
        Args.DataDir        = '~/222625v1'
        Args.OutDir         = ''
        Args.Visits         = 1:20
        Args.VisitDirs      = []       % string array of visit folder paths
        Args.ListFile       = ''       % .mat file with visit folder lists
        Args.ListFields     = {}       % field name(s) from ListFile
        Args.VisitIdx       = []       % indices into folder list
        Args.FileType       = 'proc'   % 'proc' | 'coadd'
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
        Args.MinEpochs      = 0    % Min non-NaN epochs per source; 0 = no filter
        Args.ApplyRelZP logical = true  % Apply zp_meddiff to original (non-AB) mags
        Args.ForceRecalc logical = false
        Args.CalibArgs cell = {}
        Args.Plot logical   = true
        Args.ShowOrigMag logical = true
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
        Args.TileOrder      = 'rowmajor'
        Args.VisitRefZP     = 'epoch'
        Args.VisitRefZPEpoch = 1
        Args.Verbose logical = true
    end

    if isempty(Args.OutDir)
        if ~isempty(Args.DataDir)
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

    % Update Visits to match actual loaded count (VisitDirs mode)
    Nvisits = numel(AI);
    if numel(Args.Visits) ~= Nvisits
        Args.Visits = 1:Nvisits;
    end

    % === Calibrate ===
    Calib = pipeline.last.quality.calibratePhotModes(AI, ...
        'Modes', Args.Modes, ...
        'Visits', Args.Visits, ...
        'RefCrop', Args.RefCrop, ...
        'Ncrop', Args.Ncrop, ...
        'OutDir', Args.OutDir, ...
        'ForceRecalc', Args.ForceRecalc, ...
        'CalibArgs', Args.CalibArgs, ...
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
    Result.PC       = Calib.PC;
    Result.Cats     = Calib.Cats;
    Result.FitRMS   = Calib.FitRMS;
    Result.ZPcenter = Calib.ZPcenter;
    if isfield(Calib, 'PersetInfo')
        Result.PersetInfo = Calib.PersetInfo;
    end
    Result.MS       = MS;

    % === Save full Result ===
    ResultFile = fullfile(Args.OutDir, 'Result.mat');
    try
        save(ResultFile, 'Result', '-v7.3');
        if Args.Verbose
            fprintf('Saved %s\n', ResultFile);
        end
    catch ME
        warning('testPhotCalib:SaveFailed', ...
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

    pipeline.last.quality.plotPhotMosaic(Calib, ...
        'Modes', Args.Modes, ...
        'Visits', Args.Visits, ...
        'Ncrop', Args.Ncrop, ...
        'RefCrop', Args.RefCrop, ...
        'TileOrder', Args.TileOrder);

    pipeline.last.quality.plotPhotResiduals(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend, ...
        'TrendBinWidth', Args.TrendBinWidth);

    pipeline.last.quality.plotPhotResidualsRMS(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze);

    pipeline.last.quality.plotPhotResidualsColor(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend, ...
        'Verbose', Args.Verbose);

    pipeline.last.quality.plotPhotResidualsBg(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend);

    pipeline.last.quality.plotPhotResidualsXY(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend);

    pipeline.last.quality.plotPhotResidualsAirmass(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'OverlayTrend', Args.OverlayTrend);

    pipeline.last.quality.plotPhotTransmission(Calib, ...
        'Modes', Args.Modes, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'RefCrop', Args.RefCrop);

    pipeline.last.quality.plotPhotIntegralT(Calib.PC, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
        'TileOrder', Args.TileOrder);
end
