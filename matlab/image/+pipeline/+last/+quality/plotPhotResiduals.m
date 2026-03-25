function plotPhotResiduals(PC, Args)
    % Plot calibration residuals vs magnitude across all epochs and crops
    % Description: For each mode, collects calibrator residuals from all
    %              PhotCalibTrans objects (epochs x crops) and plots residual
    %              vs magnitude. Overlays a binned trend line.
    %              Shows magnitude-dependent systematics in the calibration fit.
    %
    % Input  : - PC struct with PC.percrop{Iv}(Ic) PhotCalibTrans arrays
    %            (from calibratePhotModes .PC output or Result.PC).
    %            Calibration residuals are identical across modes (same
    %            underlying percrop fit), so only percrop is plotted.
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Normalize' - If true, plot Residual/MagErr (normalized).
    %                          Default is false.
    %            'MagField'    - Magnitude column for x-axis:
    %                            'MAG_AB' - calibrated AB mag from SourceData (default).
    %                            'instrumental' - -2.5*log10(Flux) from SourceData.
    %                            Or any column name present in SourceData.
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'- Bin width [mag]. Default is 0.5.
    %            'YLim'        - Y-axis limits [min max]. Default is [] (auto).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotResiduals(R.PC);
    %          pipeline.last.quality.plotPhotResiduals(R.PC, 'MagField', 'instrumental');

    arguments
        PC struct
        Args.CropsToAnalyze = []
        Args.MagField       = 'MAG_AB'
        Args.Normalize logical = false  % Plot Residual/MagErr instead of Residual
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
        Args.YLim           = []
    end

    if ~isfield(PC, 'percrop')
        return;
    end

    figure('Name', 'Calibrator Residuals vs Magnitude', ...
           'Position', [50, 50, 600, 500]);
    hold on;

    allMag = [];
    allRes = [];

    Nvisits = numel(PC.percrop);
    for Iv = 1:Nvisits
        if isempty(PC.percrop{Iv}); continue; end

        CropsToUse = Args.CropsToAnalyze;
        if isempty(CropsToUse)
            CropsToUse = 1:numel(PC.percrop{Iv});
        end

        for Ic = CropsToUse
            if Ic > numel(PC.percrop{Iv}); continue; end
            PCobj = PC.percrop{Iv}(Ic);
            if ~PCobj.Success; continue; end
            if isempty(PCobj.SourceData); continue; end

            Tab = PCobj.SourceData.Table;
            ColNames = Tab.Properties.VariableNames;
            if ~ismember('Residuals', ColNames); continue; end

            % Filter to used calibrators
            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(height(Tab), 1);
            end

            Residuals = Tab.Residuals(UsedMask);

            % Normalize by MagErr if requested
            if Args.Normalize
                if ~ismember('MagErr', ColNames); continue; end
                MagErr = Tab.MagErr(UsedMask);
                Residuals = Residuals ./ MagErr;
            end

            % Get magnitude for x-axis
            if strcmp(Args.MagField, 'instrumental')
                if ~ismember('Flux', ColNames); continue; end
                Flux = Tab.Flux(UsedMask);
                Mag = -2.5 * log10(Flux);
            elseif ismember(Args.MagField, ColNames)
                Mag = Tab.(Args.MagField)(UsedMask);
            elseif ismember('Flux', ColNames)
                % Fallback to instrumental
                Flux = Tab.Flux(UsedMask);
                Mag = -2.5 * log10(Flux);
            else
                continue;
            end

            ValidMask = isfinite(Residuals) & isfinite(Mag);
            allMag = [allMag; Mag(ValidMask)];
            allRes = [allRes; Residuals(ValidMask)];
        end
    end

    if ~isempty(allMag)
        plot(allMag, allRes, '.', 'MarkerSize', 2);
        plot(xlim, [0 0], 'k--');

        if ~strcmp(Args.OverlayTrend, 'none')
            MagRange = [floor(min(allMag)), ceil(max(allMag))];
            TrendFun = str2func(['nan' Args.OverlayTrend]);
            R = timeSeries.bin.binningFast([allMag(:), allRes(:)], ...
                Args.TrendBinWidth, MagRange, {'MidBin', @numel, TrendFun});
            ValidBins = R(:,2) >= 5;
            plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);
        end
    end

    box on; grid on;
    if strcmp(Args.MagField, 'instrumental')
        xlabel('-2.5 log_{10}(Flux)');
    else
        xlabel(strrep(Args.MagField, '_', '\_'));
    end
    if Args.Normalize
        ylabel('Residual / MagErr');
    else
        ylabel('Residual [mag]');
    end
    if ~isempty(Args.YLim)
        ylim(Args.YLim);
    end
    title(sprintf('Calibrator fit residuals (%d calibrators, %d epochs)', ...
        numel(allMag), Nvisits));
end
