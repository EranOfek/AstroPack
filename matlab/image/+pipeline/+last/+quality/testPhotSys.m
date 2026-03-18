function Result = testPhotSys(Args)
    % Compare PhotSys modes by epoch-to-epoch photometric repeatability
    % Description: For each PhotSys mode, calibrates all visits independently,
    %              then matches sources across epochs per crop and computes
    %              mag vs std (scatter diagram). Lower std = better mode.
    % Input  : * ...,key,val,...
    %            'DataDir' - Directory with proc FITS files. Default is '/home/dana/222625v1'.
    %            'OutDir' - Directory for saving results. Default is DataDir/results.
    %            'Visits' - Vector of visit indices. Default is 1:20.
    %            'Modes' - Cell array of PhotSys modes. Default is {'percrop','refshape','refzp'}.
    %            'RefCrop' - Reference crop index. Default is 10.
    %            'Ncrop' - Number of crops per visit. Default is 24.
    %            'CropsToAnalyze' - Crops for epoch matching. Default is [] (all).
    %            'MatchRadius' - Cross-epoch matching radius [arcsec]. Default is 3.
    %            'MagFields' - Mag columns to compare. Default is {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'MatchedColumns' - Columns to propagate into MatchedSources.
    %            'BadFlags' - Flags for setBadPhotToNan. Default is {'Saturated','NearEdge','Overlap'}.
    %            'MaxMagErr' - Max mag error for ZP-free comparison. Default is 0.02.
    %            'ForceRecalc' - Recalculate even if .mat exists. Default is false.
    %            'CalibArgs' - Additional args for fitPhotCalibTrans. Default is {}.
    %            'Plot' - Generate plots. Default is true.
    %            'Verbose' - Print progress. Default is true.
    % Output : - Result struct with fields:
    %            .PC     - struct with PC_all{Nvisits}(1xNcrop) per mode
    %            .Cats   - struct with Cats{Nvisits}(1xNcrop) AstroCatalog per mode
    %            .MS     - struct with MatchedSources per mode per crop
    %            .FitRMS - struct with fit RMS(Nvisits x Ncrop) per mode
    % Author : D. Kovaleva (Mar 2026)
    % Example: R = pipeline.last.quality.testPhotSys();
    %          R = pipeline.last.quality.testPhotSys('CropsToAnalyze', [10 19]);
    %          R = pipeline.last.quality.testPhotSys('Modes', {'percrop','refzp'}, 'ForceRecalc', true);

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
        Args.MatchedColumns = {'RA','Dec','X1','Y1','SN','MAG_AB_PSF','MAG_AB_APER_3', ...
                               'MAGERR_PSF','MAGERR_APER_3','FLAGS'}
        Args.BadFlags       = {'Saturated','NearEdge','Overlap'}
        Args.MaxMagErr      = 0.02
        Args.ForceRecalc logical = false
        Args.CalibArgs cell = {}
        Args.Plot logical   = true
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

    for Iv = 1:Nvisits
        VStr = sprintf('%03d', Args.Visits(Iv));
        ImPattern  = fullfile(Args.DataDir, sprintf('*_%s_001_*_proc_Image_1.fits', VStr));
        CatPattern = fullfile(Args.DataDir, sprintf('*_%s_001_*_proc_Cat_1.fits', VStr));

        ImFiles  = io.files.filelist(ImPattern);
        CatFiles = io.files.filelist(CatPattern);

        if isempty(ImFiles)
            if Args.Verbose
                fprintf('  Visit %s: no files, skipping\n', VStr);
            end
            continue;
        end

        AI{Iv} = AstroImage(ImFiles, 'Cat', CatFiles);
        if Args.Verbose
            fprintf('  Visit %s: %d crops\n', VStr, numel(AI{Iv}));
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
                AIcopy = AI{Iv}.copy();

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

            save(OutFile, 'PC_all', 'Cats_all', '-v7.3');
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

        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
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

            subplot(1, Nmodes, Im);
            if ~isempty(allMedMag)
                plot(allMedMag, allStdMag, '.', 'Color', Colors(Im,:), 'MarkerSize', 4);
            end
            set(gca, 'YScale', 'log');
            hold on; box on; grid on;
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

                    % Both MatchedSources have same source ordering per crop
                    % (same matching), so sources align by column index
                    Nsrc = min(MS_pc.Nsrc, MS_other.Nsrc);
                    Mag_pc = MS_pc.Data.(MagField)(:, 1:Nsrc);
                    Mag_other = MS_other.Data.(MagField)(:, 1:Nsrc);

                    Std_pc = nanstd(Mag_pc, 0, 1);
                    Std_other = nanstd(Mag_other, 0, 1);
                    MedMag = nanmedian(Mag_pc, 1);

                    allMedMag = [allMedMag, MedMag];
                    allDeltaStd = [allDeltaStd, Std_pc - Std_other];
                end

                subplot(1, Nother, Io);
                if ~isempty(allMedMag)
                    plot(allMedMag, allDeltaStd, '.', 'MarkerSize', 4);
                    hold on;
                    plot(xlim, [0 0], 'k--');
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
                'CLim', CLim, 'PhotSys', Mode, 'RefCrop', Args.RefCrop);
            title(Mode);
        end
    end
end
