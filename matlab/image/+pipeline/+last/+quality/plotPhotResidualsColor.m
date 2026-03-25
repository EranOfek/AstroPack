function plotPhotResidualsColor(PC, Args)
    % Plot calibration residuals vs GAIA BP-RP color
    % Description: Collects calibrator RA/Dec and residuals from percrop
    %              PhotCalibTrans objects, cross-matches with GAIADR3 via
    %              catsHTM to obtain BP-RP color, and plots:
    %                Panel 1: Residual (or Residual/MagErr) vs color
    %                Panel 2 (optional): Magnitude vs color
    %              Cross-match is done once per crop (calibrators are the
    %              same across epochs for a given crop).
    %
    % Input  : - PC struct with PC.percrop{Iv}(Ic) PhotCalibTrans arrays
    %            (from calibratePhotModes .PC output or Result.PC).
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'GaiaCat'     - catsHTM catalog name. Default is 'GAIADR3'.
    %            'ColorCol'    - Color column name in GAIA catalog.
    %                            Default is 'bp_rp'.
    %            'MatchRadius' - Cross-match radius [arcsec]. Default is 3.
    %            'Normalize'   - Plot Residual/MagErr instead of Residual.
    %                            Default is false.
    %            'PlotMagColor'- Add magnitude vs color panel. Default is false.
    %            'MagField'    - Magnitude for mag-color panel: 'MAG_AB' (default),
    %                            'instrumental' (-2.5*log10(Flux)), or any
    %                            column name present in SourceData.
    %            'OverlayTrend'- Binned trend on residual panel:
    %                            'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'- Bin width for trend [color units]. Default is 0.2.
    %            'YLim'        - Y-axis limits for residual panel. Default is [] (auto).
    %            'Verbose'     - Print progress. Default is true.
    % Author : D. Kovaleva (Mar 2026)
    % Example: % Residual vs color:
    %          pipeline.last.quality.plotPhotResidualsColor(R.PC);
    %
    %          % Normalized residuals:
    %          pipeline.last.quality.plotPhotResidualsColor(R.PC, 'Normalize', true);
    %
    %          % With magnitude-color panel:
    %          pipeline.last.quality.plotPhotResidualsColor(R.PC, 'PlotMagColor', true);
    %
    %          % Normalized + mag-color with instrumental magnitudes:
    %          pipeline.last.quality.plotPhotResidualsColor(R.PC, 'Normalize', true, ...
    %              'PlotMagColor', true, 'MagField', 'instrumental');

    arguments
        PC struct
        Args.CropsToAnalyze = []
        Args.GaiaCat        = 'GAIADR3'
        Args.ColorCol       = 'bp_rp'
        Args.MatchRadius    = 3       % [arcsec]
        Args.Normalize logical = false
        Args.PlotMagColor logical = false  % Additional panel: magnitude vs color
        Args.MagField       = 'MAG_AB'    % Magnitude column for mag-color plot
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.2
        Args.YLim           = []
        Args.Verbose logical = true
    end

    if ~isfield(PC, 'percrop')
        return;
    end

    allColor = [];
    allRes   = [];
    allMag   = [];

    Nvisits = numel(PC.percrop);

    % Find first valid visit to determine crop count
    FirstValid = find(~cellfun(@isempty, PC.percrop), 1);
    if isempty(FirstValid); return; end

    CropsToUse = Args.CropsToAnalyze;
    if isempty(CropsToUse)
        CropsToUse = 1:numel(PC.percrop{FirstValid});
    end

    MatchRadiusRad = Args.MatchRadius / 206264.806;  % arcsec to radians

    for Ic = CropsToUse
        % --- Cross-match calibrators with GAIA once per crop ---
        % Use the first valid epoch to get calibrator positions
        RefPCobj = [];
        for Iv = 1:Nvisits
            if isempty(PC.percrop{Iv}); continue; end
            if Ic > numel(PC.percrop{Iv}); continue; end
            if PC.percrop{Iv}(Ic).Success && ~isempty(PC.percrop{Iv}(Ic).SourceData)
                RefPCobj = PC.percrop{Iv}(Ic);
                break;
            end
        end
        if isempty(RefPCobj); continue; end

        Tab = RefPCobj.SourceData.Table;
        if ~ismember('RA', Tab.Properties.VariableNames); continue; end

        CalibRA  = deg2rad(Tab.RA);
        CalibDec = deg2rad(Tab.Dec);
        Ncalib = numel(CalibRA);

        % Single cone search covering all calibrators in this crop
        MedRA  = nanmedian(CalibRA);
        MedDec = nanmedian(CalibDec);
        MaxDist = max(celestial.coo.sphere_dist_fast(MedRA, MedDec, CalibRA, CalibDec));
        SearchRadius = convert.angular('rad', 'arcsec', MaxDist) + Args.MatchRadius + 10;

        [CatH, ColCellH] = catsHTM.cone_search(Args.GaiaCat, MedRA, MedDec, SearchRadius, ...
            'RadiusUnits', 'arcsec');

        if isempty(CatH)
            if Args.Verbose
                fprintf('  Crop %02d: no GAIA sources found\n', Ic);
            end
            continue;
        end

        % Find color column index
        ColorIdx = find(strcmp(ColCellH, Args.ColorCol));
        if isempty(ColorIdx)
            warning('plotPhotResidualsColor:NoColorCol', ...
                'Column %s not found in %s', Args.ColorCol, Args.GaiaCat);
            return;
        end
        RAidx  = find(strcmp(ColCellH, 'RA'));
        DecIdx = find(strcmp(ColCellH, 'Dec'));

        % Match each calibrator to nearest GAIA source
        CalibColor = nan(Ncalib, 1);
        for Is = 1:Ncalib
            Dist = celestial.coo.sphere_dist_fast(CalibRA(Is), CalibDec(Is), ...
                CatH(:, RAidx), CatH(:, DecIdx));
            [MinDist, MinIdx] = min(Dist);
            if MinDist <= MatchRadiusRad
                CalibColor(Is) = CatH(MinIdx, ColorIdx);
            end
        end

        Nmatched = sum(isfinite(CalibColor));
        if Args.Verbose
            fprintf('  Crop %02d: %d/%d calibrators matched to %s\n', ...
                Ic, Nmatched, Ncalib, Args.GaiaCat);
        end

        % --- Collect residuals from all epochs for this crop ---
        for Iv = 1:Nvisits
            if isempty(PC.percrop{Iv}); continue; end
            if Ic > numel(PC.percrop{Iv}); continue; end
            PCobj = PC.percrop{Iv}(Ic);
            if ~PCobj.Success || isempty(PCobj.SourceData); continue; end

            TabEp = PCobj.SourceData.Table;
            ColNames = TabEp.Properties.VariableNames;
            if ~ismember('Residuals', ColNames); continue; end

            Nep = height(TabEp);
            if Nep ~= Ncalib; continue; end  % skip if calibrator count differs

            if ismember('Used', ColNames)
                UsedMask = logical(TabEp.Used);
            else
                UsedMask = true(Nep, 1);
            end

            Residuals = TabEp.Residuals(UsedMask);
            Color = CalibColor(UsedMask);

            % Get magnitude for mag-color plot
            if Args.PlotMagColor
                if strcmp(Args.MagField, 'instrumental')
                    if ismember('Flux', ColNames)
                        Mag = -2.5 * log10(TabEp.Flux(UsedMask));
                    else
                        Mag = nan(size(Residuals));
                    end
                elseif ismember(Args.MagField, ColNames)
                    Mag = TabEp.(Args.MagField)(UsedMask);
                else
                    Mag = nan(size(Residuals));
                end
            else
                Mag = nan(size(Residuals));
            end

            if Args.Normalize
                if ~ismember('MagErr', ColNames); continue; end
                MagErr = TabEp.MagErr(UsedMask);
                Residuals = Residuals ./ MagErr;
            end

            ValidMask = isfinite(Residuals) & isfinite(Color);
            allColor = [allColor; Color(ValidMask)];
            allRes   = [allRes; Residuals(ValidMask)];
            allMag   = [allMag; Mag(ValidMask)];
        end
    end

    if isempty(allColor)
        warning('plotPhotResidualsColor:NoData', 'No valid color-residual pairs found.');
        return;
    end

    % --- Plot ---
    Npanels = 1 + Args.PlotMagColor;
    figure('Name', 'Calibrator Residuals vs Color', ...
           'Position', [50, 50, 550*Npanels, 500]);

    % Panel 1: Residual vs Color
    subplot(1, Npanels, 1);
    hold on;

    plot(allColor, allRes, '.', 'MarkerSize', 2);
    plot(xlim, [0 0], 'k--');

    if ~strcmp(Args.OverlayTrend, 'none')
        ColorRange = [floor(min(allColor)*5)/5, ceil(max(allColor)*5)/5];
        TrendFun = str2func(['nan' Args.OverlayTrend]);
        R = timeSeries.bin.binningFast([allColor(:), allRes(:)], ...
            Args.TrendBinWidth, ColorRange, {'MidBin', @numel, TrendFun});
        ValidBins = R(:,2) >= 5;
        plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);
    end

    box on; grid on;
    xlabel(sprintf('GAIA %s', strrep(Args.ColorCol, '_', '\_')));
    if Args.Normalize
        ylabel('Residual / MagErr');
    else
        ylabel('Residual [mag]');
    end
    if ~isempty(Args.YLim)
        ylim(Args.YLim);
    end
    title(sprintf('Residual vs color (%d points)', numel(allColor)));

    % Panel 2: Color vs Magnitude (CMD)
    if Args.PlotMagColor
        subplot(1, Npanels, 2);
        hold on;

        ValidMC = isfinite(allMag) & isfinite(allColor);
        plot(allMag(ValidMC), allColor(ValidMC), '.', 'MarkerSize', 2);

        if ~strcmp(Args.OverlayTrend, 'none')
            MagRange = [floor(min(allMag(ValidMC))), ceil(max(allMag(ValidMC)))];
            TrendFun = str2func(['nan' Args.OverlayTrend]);
            R = timeSeries.bin.binningFast([allMag(ValidMC), allColor(ValidMC)], ...
                Args.TrendBinWidth, MagRange, {'MidBin', @numel, TrendFun});
            ValidBins = R(:,2) >= 5;
            plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);
        end

        box on; grid on;
        if strcmp(Args.MagField, 'instrumental')
            xlabel('-2.5 log_{10}(Flux)');
        else
            xlabel(strrep(Args.MagField, '_', '\_'));
        end
        ylabel(sprintf('GAIA %s', strrep(Args.ColorCol, '_', '\_')));
        title(sprintf('Color vs magnitude (%d points)', sum(ValidMC)));
    end

    sgtitle(sprintf('Calibrators × GAIA (%d epochs)', Nvisits));
end
