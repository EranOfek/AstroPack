function Result = testPhotSys(Args)
    % Compare PhotSys modes by epoch-to-epoch photometric repeatability
    % Description: For each PhotSys mode, calibrates all visits of the same
    %              field independently using fitPhotCalibTrans, then matches
    %              sources across epochs per crop and computes the epoch-to-epoch
    %              magnitude scatter (std). Lower std = better calibration mode.
    %              Calibration results are cached in OutDir as PC_<mode>.mat;
    %              delete or use ForceRecalc=true to recompute.
    %
    %              PhotSys modes:
    %                'percrop'   - Each crop uses its own fitted transmission (default pipeline).
    %                'refshape'  - Reference crop's spectral shape, per-crop Norm + Tran2D.
    %                'refzp'     - Full reference params (incl. Norm), center-normalized Tran2D.
    %                'refzp_raw' - Same as refzp but without Tran2D center-normalization.
    %              RefCrop=0 uses weighted mean (1/RMS^2) of all crops instead of a single reference.
    %
    %              Plots generated (when Plot=true):
    %                1. Mag vs Std scatter — one panel per mode. Shows epoch-to-epoch
    %                   std vs median magnitude. When ShowOrigMag=true, original
    %                   (instrumental) mag scatter is shown in gray underneath the
    %                   AB-calibrated scatter in color.
    %                2. Std difference — Std(percrop) - Std(other) vs magnitude.
    %                   Points >0 mean the non-percrop mode is better.
    %                3. ZP mosaic — side-by-side ZP maps for the first visit.
    %                Binned trend lines (median or mean) are overlaid on scatter plots.
    %
    % Input  : * ...,key,val,...
    %            'DataDir' - Directory with proc FITS files (Image + Cat).
    %                        Default is '/home/dana/222625v1'.
    %            'OutDir' - Directory for saving cached results (.mat files).
    %                        Default is DataDir/results.
    %            'Visits' - Vector of visit indices to process. Default is 1:20.
    %            'Modes' - Cell array of PhotSys modes to compare.
    %                        Default is {'percrop','refshape','refzp'}.
    %            'RefCrop' - Reference crop index for non-percrop modes.
    %                        0 = weighted mean over all crops. Default is 10.
    %            'Ncrop' - Number of crops per visit. Default is 24.
    %            'CropsToAnalyze' - Subset of crops for epoch matching and plots.
    %                        Default is [] (all crops).
    %            'MatchRadius' - Cross-epoch source matching radius [arcsec]. Default is 3.
    %            'MagFields' - Cell array of AB magnitude columns to compare.
    %                        Default is {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'MatchedColumns' - Columns propagated into MatchedSources.
    %                        Must include MagFields and their original counterparts.
    %            'BadFlags' - Flags for setBadPhotToNan. Default is {'Saturated','NearEdge','Overlap'}.
    %            'MaxMagErr' - Max magnitude error for filtering. Default is 0.02.
    %            'ForceRecalc' - Recalculate even if cached .mat exists. Default is false.
    %            'CalibArgs' - Additional key-value args forwarded to fitPhotCalibTrans.
    %                        Default is {}.
    %            'Plot' - Generate diagnostic plots. Default is true.
    %            'ShowOrigMag' - On AB scatter panels, overlay original (instrumental)
    %                        mag scatter in gray for comparison. Default is true.
    %            'OverlayTrend' - Binned trend line on scatter plots:
    %                        'median' (default), 'mean', or 'none'.
    %            'TrendBinWidth' - Magnitude bin width for trend line. Default is 0.5.
    %            'TileOrder' - Crop tiling order in mosaic plots:
    %                        'colmajor' (old pipeline) - bottom-to-top, column by column.
    %                        'rowmajor' (new pipeline) - left-to-right, row by row.
    %                        Default is 'rowmajor'.
    %            'Verbose' - Print progress messages. Default is true.
    % Output : - Result struct with fields:
    %            .PC     - struct with PC_all{Nvisits}(1xNcrop) PhotCalibTrans per mode
    %            .Cats   - struct with Cats_all{Nvisits}(1xNcrop) AstroCatalog per mode
    %            .MS     - struct with MS{Ncrop} MatchedSources per mode
    %            .FitRMS - struct with RMS(Nvisits x Ncrop) matrix per mode
    %            .CLim   - color limits used for ZP mosaic plots
    % Author : D. Kovaleva (Mar 2026)
    % Example: % Run all defaults (3 modes, 20 visits, all crops):
    %          R = pipeline.last.quality.testPhotSys();
    %
    %          % Compare only percrop vs refzp on specific crops:
    %          R = pipeline.last.quality.testPhotSys('Modes', {'percrop','refzp'}, ...
    %              'CropsToAnalyze', [10 19]);
    %
    %          % Use weighted mean transmission instead of single reference crop:
    %          R = pipeline.last.quality.testPhotSys('RefCrop', 0);
    %
    %          % Include refzp_raw (without Tran2D normalization):
    %          R = pipeline.last.quality.testPhotSys('Modes', ...
    %              {'percrop','refzp','refzp_raw'});
    %
    %          % Force recalculation with custom CalibArgs:
    %          R = pipeline.last.quality.testPhotSys('ForceRecalc', true, ...
    %              'CalibArgs', {'UseTran2D', false});
    %
    %          % Custom data directory, fewer visits, mean trend line:
    %          R = pipeline.last.quality.testPhotSys('DataDir', '/path/to/data', ...
    %              'Visits', 1:5, 'OverlayTrend', 'mean');
    %
    %          % Disable original mag overlay and trend lines:
    %          R = pipeline.last.quality.testPhotSys('ShowOrigMag', false, ...
    %              'OverlayTrend', 'none');
    %
    %          % Use new pipeline tiling order for mosaic plots:
    %          R = pipeline.last.quality.testPhotSys('TileOrder', 'rowmajor');

    arguments
        Args.DataDir        = '/home/dana/222625v1'
        Args.OutDir         = ''
        Args.Visits         = 1:20
        Args.Modes          = {'percrop', 'refshape', 'refzp'}
        Args.RefCrop        = 10
        Args.Ncrop          = 24
        Args.CropsToAnalyze = []
        Args.MatchRadius    = 3
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.MatchedColumns = {'RA','Dec','X1','Y1','SN', ...
                               'MAG_AB_PSF','MAG_AB_APER_3', ...
                               'MAG_PSF','MAG_APER_3', ...
                               'MAGERR_PSF','MAGERR_APER_3','FLAGS'}
        Args.BadFlags       = {'Saturated','NearEdge','Overlap'}
        Args.MaxMagErr      = 0.02
        Args.ForceRecalc logical = false
        Args.CalibArgs cell = {}
        Args.Plot logical   = true
        Args.ShowOrigMag logical = true  % Overlay original (non-AB) mag scatter on AB panels
        Args.OverlayTrend   = 'median'  % 'median' | 'mean' | 'none' — binned trend line on scatter plots
        Args.TrendBinWidth  = 0.5       % mag bin width for trend line
        Args.TileOrder      = 'rowmajor'  % 'colmajor' (old) | 'rowmajor' (new pipeline)
        Args.Verbose logical = true
    end

    if isempty(Args.OutDir)
        Args.OutDir = fullfile(Args.DataDir, 'results');
    end
    if ~exist(Args.OutDir, 'dir')
        mkdir(Args.OutDir);
    end
    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end

    Nvisits = numel(Args.Visits);
    Nmodes  = numel(Args.Modes);

    % ================================================================
    % LOAD ALL VISITS (once, reuse across modes)
    % ================================================================
    if Args.Verbose
        fprintf('Loading %d visits from %s\n', Nvisits, Args.DataDir);
    end
    AI = cell(Nvisits, 1);

    % Glob Cat files once, then filter per visit by parsing LAST filename:
    % ..._<visit>_<mount>_<crop>_sci_proc_Cat_<ver>.fits
    % Only Cat files are needed — calibration uses catalog + header, not pixels.
    AllCatFiles = io.files.filelist(fullfile(Args.DataDir, '*_sci_proc_Cat_1.fits'));
    AllImFiles  = io.files.filelist(fullfile(Args.DataDir, '*_sci_proc_Image_1.fits'));

    for Iv = 1:Nvisits
        VisitNum = Args.Visits(Iv);
        VStr = sprintf('%03d', VisitNum);

        % Extract visit number: 7th underscore-delimited token from end
        CatKeep = false(numel(AllCatFiles), 1);
        for If = 1:numel(AllCatFiles)
            [~, Name] = fileparts(AllCatFiles{If});
            Tokens = strsplit(Name, '_');
            if numel(Tokens) >= 7
                CatKeep(If) = str2double(Tokens{end-6}) == VisitNum;
            end
        end
        ImKeep = false(numel(AllImFiles), 1);
        for If = 1:numel(AllImFiles)
            [~, Name] = fileparts(AllImFiles{If});
            Tokens = strsplit(Name, '_');
            if numel(Tokens) >= 7
                ImKeep(If) = str2double(Tokens{end-6}) == VisitNum;
            end
        end
        CatFiles = AllCatFiles(CatKeep);
        ImFiles  = AllImFiles(ImKeep);

        if isempty(CatFiles)
            if Args.Verbose
                fprintf('  Visit %s: no files, skipping\n', VStr);
            end
            continue;
        end

        % Build AstroImage with Header from Image FITS (HDU 1, no pixels)
        % and catalog from Cat FITS — skips loading image/mask/PSF data
        Ncf = numel(CatFiles);
        AIv = AstroImage([1, Ncf]);
        for Ic = 1:Ncf
            AIv(Ic).CatData = AstroCatalog(CatFiles{Ic});
            if Ic <= numel(ImFiles)
                AIv(Ic).HeaderData = AstroHeader(ImFiles{Ic}, 1);
            end
        end
        AI{Iv} = AIv;

        if Args.Verbose
            fprintf('  Visit %s: %d crops\n', VStr, Ncf);
        end
    end

    % ================================================================
    % CALIBRATE EACH MODE — save PC + calibrated catalogs
    % ================================================================
    Result.PC   = struct();
    Result.Cats = struct();
    Result.FitRMS = struct();

    for Im = 1:Nmodes
        Mode = Args.Modes{Im};
        OutFile = fullfile(Args.OutDir, sprintf('PC_%s.mat', Mode));

        if exist(OutFile, 'file') && ~Args.ForceRecalc
            if Args.Verbose
                fprintf('Loading cached %s\n', OutFile);
            end
            S = load(OutFile, 'PC_all', 'Cats_all');
            Result.PC.(Mode)   = S.PC_all;
            Result.Cats.(Mode) = S.Cats_all;
        else
            if Args.Verbose
                fprintf('\n=== Calibrating: PhotSys = %s ===\n', Mode);
            end
            PC_all   = cell(Nvisits, 1);
            Cats_all = cell(Nvisits, 1);

            for Iv = 1:Nvisits
                if isempty(AI{Iv})
                    continue;
                end
                t0 = tic;

                % Fresh copy — AstroImage is a handle class
                tcopy = tic;
                AIcopy = AI{Iv}.copy();
                if Args.Verbose
                    fprintf('    copy: %.1f s, ', toc(tcopy));
                end

                [Res, PC_all{Iv}] = imProc.calib.fitPhotCalibTrans(AIcopy, ...
                    'PhotSys', Mode, 'RefCrop', Args.RefCrop, ...
                    'Verbose', false, Args.CalibArgs{:});

                % Extract calibrated catalogs (lightweight)
                Ncrop = numel(Res);
                Cats_all{Iv} = AstroCatalog.empty(0, Ncrop);
                for Ic = 1:Ncrop
                    Cats_all{Iv}(Ic) = Res(Ic).CatData;
                end

                if Args.Verbose
                    Nsuccess = sum([PC_all{Iv}.Success]);
                    fprintf('  Visit %03d: %d/%d success, %.1f s\n', ...
                        Args.Visits(Iv), Nsuccess, Ncrop, toc(t0));
                end
            end

            save(OutFile, 'PC_all', 'Cats_all');
            if Args.Verbose
                fprintf('Saved %s\n', OutFile);
            end
            Result.PC.(Mode)   = PC_all;
            Result.Cats.(Mode) = Cats_all;
        end

        % Fit RMS summary
        RMSmat = nan(Nvisits, Args.Ncrop);
        for Iv = 1:Nvisits
            if isempty(Result.PC.(Mode){Iv}); continue; end
            for Ic = 1:numel(Result.PC.(Mode){Iv})
                if Result.PC.(Mode){Iv}(Ic).Success
                    RMSmat(Iv, Ic) = Result.PC.(Mode){Iv}(Ic).TransModel.RMS;
                end
            end
        end
        Result.FitRMS.(Mode) = RMSmat;
    end

    if Args.Verbose
        fprintf('\n=== Fit RMS Summary ===\n');
        fprintf('%-10s %8s %8s %8s\n', 'Mode', 'Median', 'Mean', 'Max');
        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
            vals = Result.FitRMS.(Mode)(isfinite(Result.FitRMS.(Mode)));
            fprintf('%-10s %8.4f %8.4f %8.4f\n', Mode, median(vals), mean(vals), max(vals));
        end
    end

    % ================================================================
    % EPOCH MATCHING — per crop, per mode
    % ================================================================
    if Args.Verbose
        fprintf('\n=== Epoch Matching ===\n');
    end

    Result.MS = struct();

    for Im = 1:Nmodes
        Mode = Args.Modes{Im};

        for Ic = Args.CropsToAnalyze
            % Collect catalogs for this crop across epochs
            CatList = AstroCatalog.empty(0, Nvisits);
            ValidEpochs = false(Nvisits, 1);

            for Iv = 1:Nvisits
                if isempty(Result.Cats.(Mode){Iv})
                    continue;
                end
                if Ic <= numel(Result.Cats.(Mode){Iv})
                    CatList(Iv) = Result.Cats.(Mode){Iv}(Ic);
                    ValidEpochs(Iv) = true;
                end
            end

            if sum(ValidEpochs) < 3
                if Args.Verbose
                    fprintf('  %s crop %d: <3 valid epochs, skipping\n', Mode, Ic);
                end
                continue;
            end

            % Match across epochs — we only need the MatchedSources object
            % Use unifiedCatalogsIntoMatched directly to avoid mergeCatalogs'
            % sortrows-by-Dec which fails when FitPM=false
            MS = MatchedSources;
            MS = MS.unifiedCatalogsIntoMatched(CatList(ValidEpochs).', ...
                'MatchedColums', Args.MatchedColumns, ...
                'Radius', Args.MatchRadius, 'RadiusUnits', 'arcsec');

            % Flag bad photometry
            MS = MS.setBadPhotToNan('BadFlags', Args.BadFlags, ...
                'MagField', 'MAG_PSF', 'CreateNewObj', false);

            % Apply relative ZP correction to original (non-AB) mag fields
            % only — AB magnitudes already carry the calibrated ZP
            for Imf = 1:numel(Args.MagFields)
                OrigField = strrep(Args.MagFields{Imf}, '_AB_', '_');
                if strcmp(OrigField, Args.MagFields{Imf}); continue; end
                if ~isfield(MS.Data, OrigField); continue; end
                ErrField = strrep(OrigField, 'MAG_', 'MAGERR_');
                if isfield(MS.Data, ErrField)
                    Rzp = lcUtil.zp_meddiff(MS, 'MagField', {OrigField}, ...
                        'MagErrField', {ErrField}, 'MaxMagErr', Args.MaxMagErr);
                else
                    Rzp = lcUtil.zp_meddiff(MS, 'MagField', {OrigField});
                end
                MS = MS.applyZP(Rzp, 'ApplyToMagField', {OrigField});
            end

            Result.MS.(Mode){Ic} = MS;

            if Args.Verbose
                fprintf('  %s crop %02d: %d matched sources\n', ...
                    Mode, Ic, MS.Nsrc);
            end
        end
    end

    % ================================================================
    % PLOTS
    % ================================================================
    if ~Args.Plot
        return;
    end

    % --- Mag vs Std scatter — separate panel per mode ---
    Colors = lines(Nmodes);

    for Imf = 1:numel(Args.MagFields)
        MagField = Args.MagFields{Imf};

        figure('Name', sprintf('Mag vs Std — %s', MagField), ...
               'Position', [50, 50, 400*Nmodes, 500]);

        % Derive original (non-AB) mag field name
        OrigMagField = '';
        if Args.ShowOrigMag && contains(MagField, '_AB_')
            OrigMagField = strrep(MagField, '_AB_', '_');
        end

        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
            subplot(1, Nmodes, Im);
            hold on;

            % --- Original mag scatter (background, gray) ---
            if ~isempty(OrigMagField)
                allMedOrig = [];
                allStdOrig = [];
                for Ic = Args.CropsToAnalyze
                    if Ic > numel(Result.MS.(Mode)) || isempty(Result.MS.(Mode){Ic})
                        continue;
                    end
                    MS = Result.MS.(Mode){Ic};
                    if ~isfield(MS.Data, OrigMagField); continue; end
                    OrigData = MS.Data.(OrigMagField);
                    allMedOrig = [allMedOrig, nanmedian(OrigData, 1)];
                    allStdOrig = [allStdOrig, nanstd(OrigData, 0, 1)];
                end
                if ~isempty(allMedOrig)
                    plot(allMedOrig, allStdOrig, '.', 'Color', [0.75 0.75 0.75], 'MarkerSize', 3);
                    % Trend line for original
                    if ~strcmp(Args.OverlayTrend, 'none')
                        BinEdges = 9:Args.TrendBinWidth:22;
                        BinCenters = BinEdges(1:end-1) + Args.TrendBinWidth/2;
                        [~, ~, BinIdx] = histcounts(allMedOrig, BinEdges);
                        TrendOrig = nan(size(BinCenters));
                        for Ib = 1:numel(BinCenters)
                            Mask = BinIdx == Ib;
                            if sum(Mask) > 5
                                if strcmp(Args.OverlayTrend, 'median')
                                    TrendOrig(Ib) = nanmedian(allStdOrig(Mask));
                                else
                                    TrendOrig(Ib) = nanmean(allStdOrig(Mask));
                                end
                            end
                        end
                        ValidBins = isfinite(TrendOrig);
                        plot(BinCenters(ValidBins), TrendOrig(ValidBins), '--', ...
                            'Color', [0.5 0.5 0.5], 'LineWidth', 1.5);
                    end
                end
            end

            % --- AB mag scatter (foreground, color) ---
            allMedMag = [];
            allStdMag = [];

            for Ic = Args.CropsToAnalyze
                if Ic > numel(Result.MS.(Mode)) || isempty(Result.MS.(Mode){Ic})
                    continue;
                end
                MS = Result.MS.(Mode){Ic};

                if ~isfield(MS.Data, MagField)
                    continue;
                end

                MagData = MS.Data.(MagField);  % [Nepochs x Nsrc]
                MedMag = nanmedian(MagData, 1);
                StdMag = nanstd(MagData, 0, 1);

                allMedMag = [allMedMag, MedMag];
                allStdMag = [allStdMag, StdMag];
            end

            if ~isempty(allMedMag)
                plot(allMedMag, allStdMag, '.', 'Color', Colors(Im,:), 'MarkerSize', 4);
                % Trend line for AB
                if ~strcmp(Args.OverlayTrend, 'none')
                    BinEdges = 9:Args.TrendBinWidth:22;
                    BinCenters = BinEdges(1:end-1) + Args.TrendBinWidth/2;
                    [~, ~, BinIdx] = histcounts(allMedMag, BinEdges);
                    TrendVal = nan(size(BinCenters));
                    for Ib = 1:numel(BinCenters)
                        Mask = BinIdx == Ib;
                        if sum(Mask) > 5
                            if strcmp(Args.OverlayTrend, 'median')
                                TrendVal(Ib) = nanmedian(allStdMag(Mask));
                            else
                                TrendVal(Ib) = nanmean(allStdMag(Mask));
                            end
                        end
                    end
                    ValidBins = isfinite(TrendVal);
                    plot(BinCenters(ValidBins), TrendVal(ValidBins), '-k', 'LineWidth', 2);
                end
            end
            set(gca, 'YScale', 'log');
            box on; grid on;
            xlabel('Median Magnitude');
            ylabel('Std [mag]');
            xlim([9 22]);
            ylim([1e-3 10]);
            title(sprintf('%s', Mode));
        end
        sgtitle(sprintf('Epoch-to-epoch scatter: %s', strrep(MagField, '_', '\_')));
    end

    % --- Std difference: percrop vs other modes ---
    if ismember('percrop', Args.Modes) && Nmodes > 1
        for Imf = 1:numel(Args.MagFields)
            MagField = Args.MagFields{Imf};
            OtherModes = setdiff(Args.Modes, {'percrop'}, 'stable');
            Nother = numel(OtherModes);

            figure('Name', sprintf('Std difference — %s', MagField), ...
                   'Position', [50, 50, 400*Nother, 500]);

            for Io = 1:Nother
                Mode = OtherModes{Io};
                allMedMag = [];
                allDeltaStd = [];

                for Ic = Args.CropsToAnalyze
                    % Check both modes have data for this crop
                    if Ic > numel(Result.MS.percrop) || isempty(Result.MS.percrop{Ic})
                        continue;
                    end
                    if Ic > numel(Result.MS.(Mode)) || isempty(Result.MS.(Mode){Ic})
                        continue;
                    end

                    MS_pc = Result.MS.percrop{Ic};
                    MS_other = Result.MS.(Mode){Ic};

                    if ~isfield(MS_pc.Data, MagField) || ~isfield(MS_other.Data, MagField)
                        continue;
                    end

                    % Sources align by column index — same input catalogs,
                    % same matching radius and algorithm
                    Nsrc = min(MS_pc.Nsrc, MS_other.Nsrc);
                    Mag_pc    = MS_pc.Data.(MagField)(:, 1:Nsrc);
                    Mag_other = MS_other.Data.(MagField)(:, 1:Nsrc);

                    Std_pc    = nanstd(Mag_pc, 0, 1);
                    Std_other = nanstd(Mag_other, 0, 1);
                    MedMag    = nanmedian(Mag_pc, 1);

                    allMedMag = [allMedMag, MedMag];
                    allDeltaStd = [allDeltaStd, Std_pc - Std_other];
                end

                subplot(1, Nother, Io);
                if ~isempty(allMedMag)
                    plot(allMedMag, allDeltaStd, '.', 'MarkerSize', 4);
                    hold on;
                    plot(xlim, [0 0], 'k--');
                    % Overlay binned trend line
                    if ~strcmp(Args.OverlayTrend, 'none')
                        BinEdges = 9:Args.TrendBinWidth:22;
                        BinCenters = BinEdges(1:end-1) + Args.TrendBinWidth/2;
                        [~, ~, BinIdx] = histcounts(allMedMag, BinEdges);
                        TrendVal = nan(size(BinCenters));
                        for Ib = 1:numel(BinCenters)
                            Mask = BinIdx == Ib;
                            if sum(Mask) > 5
                                if strcmp(Args.OverlayTrend, 'median')
                                    TrendVal(Ib) = nanmedian(allDeltaStd(Mask));
                                else
                                    TrendVal(Ib) = nanmean(allDeltaStd(Mask));
                                end
                            end
                        end
                        ValidBins = isfinite(TrendVal);
                        plot(BinCenters(ValidBins), TrendVal(ValidBins), '-r', 'LineWidth', 2);
                    end
                end
                box on; grid on;
                xlabel('Median Magnitude');
                ylabel(sprintf('Std(percrop) - Std(%s) [mag]', Mode));
                xlim([9 22]);
                title(sprintf('percrop - %s', Mode));
            end
            sgtitle(sprintf('Std difference (>0 = %s better): %s', ...
                'non-percrop', strrep(MagField, '_', '\_')));
        end
    end

    % --- ZP Mosaic comparison for selected visit ---
    VisitIdx = find(Args.Visits == min(Args.Visits), 1);
    if ~isempty(Result.PC.(Args.Modes{1}){VisitIdx})
        PCref = Result.PC.(Args.Modes{1}){VisitIdx};
        ZPvals = nan(Args.Ncrop, 1);
        for Ic = 1:numel(PCref)
            if PCref(Ic).Success
                ZPvals(Ic) = PCref(Ic).evaluateZP('X', 863, 'Y', 863);
            end
        end
        CLim = [min(ZPvals) - 0.05, max(ZPvals) + 0.05];
        Result.CLim = CLim;

        figure('Position', [50, 50, 500*Nmodes, 500], ...
               'Name', sprintf('ZP Mosaic — Visit %d', Args.Visits(VisitIdx)));
        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
            if isempty(Result.PC.(Mode){VisitIdx}); continue; end
            subplot(1, Nmodes, Im);
            Result.PC.(Mode){VisitIdx}.plotZPMap('NewFigure', false, ...
                'CLim', CLim, 'SmoothSigma', 0, ...
                'PhotSys', Mode, 'RefCrop', Args.RefCrop, ...
                'TileOrder', Args.TileOrder);
            title(Mode);
        end
    end
end
