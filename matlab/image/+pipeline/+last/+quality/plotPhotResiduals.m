function plotPhotResiduals(PC, Args)
    % Plot calibration residuals vs magnitude across all epochs and crops
    % Description: For each mode, collects calibrator residuals from all
    %              PhotCalibTrans objects (epochs × crops) and plots residual
    %              vs instrumental magnitude. Overlays a binned trend line.
    %              Shows magnitude-dependent systematics in the calibration fit.
    %
    % Input  : - PC struct with PC.(mode){Iv}(Ic) PhotCalibTrans arrays
    %            (from calibratePhotModes .PC output or Result.PC).
    %          * ...,key,val,...
    %            'Modes'       - Cell array of modes. Required.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'- Bin width [mag]. Default is 0.5.
    %            'YLim'        - Y-axis limits [min max]. Default is [] (auto).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotResiduals(R.PC, 'Modes', {'percrop'});
    %          pipeline.last.quality.plotPhotResiduals(R.PC, ...
    %              'Modes', {'percrop','refzp'}, 'CropsToAnalyze', 1:12);

    arguments
        PC struct
        Args.Modes cell
        Args.CropsToAnalyze = []
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
        Args.YLim           = []
    end

    Nmodes = numel(Args.Modes);
    Colors = lines(Nmodes);

    figure('Name', 'Calibrator Residuals vs Magnitude', ...
           'Position', [50, 50, 400*Nmodes, 500]);

    for Im = 1:Nmodes
        Mode = Args.Modes{Im};
        if ~isfield(PC, Mode); continue; end

        subplot(1, Nmodes, Im);
        hold on;

        allMag = [];
        allRes = [];

        Nvisits = numel(PC.(Mode));
        for Iv = 1:Nvisits
            if isempty(PC.(Mode){Iv}); continue; end

            CropsToUse = Args.CropsToAnalyze;
            if isempty(CropsToUse)
                CropsToUse = 1:numel(PC.(Mode){Iv});
            end

            for Ic = CropsToUse
                if Ic > numel(PC.(Mode){Iv}); continue; end
                PCobj = PC.(Mode){Iv}(Ic);
                if ~PCobj.Success; continue; end
                if isempty(PCobj.SourceData); continue; end

                Tab = PCobj.SourceData.Table;
                ColNames = Tab.Properties.VariableNames;
                if ~ismember('Residuals', ColNames); continue; end
                if ~ismember('Flux', ColNames); continue; end

                % Filter to used calibrators
                if ismember('Used', ColNames)
                    UsedMask = logical(Tab.Used);
                else
                    UsedMask = true(height(Tab), 1);
                end

                Residuals = Tab.Residuals(UsedMask);
                Flux = Tab.Flux(UsedMask);

                % Skip if no valid data
                ValidMask = isfinite(Residuals) & Flux > 0;
                Residuals = Residuals(ValidMask);
                Flux = Flux(ValidMask);

                MagInst = -2.5 * log10(Flux);

                allMag = [allMag; MagInst(:)];
                allRes = [allRes; Residuals(:)];
            end
        end

        if ~isempty(allMag)
            plot(allMag, allRes, '.', 'Color', Colors(Im,:), 'MarkerSize', 2);
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
        xlabel('Instrumental Magnitude');
        ylabel('Residual [mag]');
        if ~isempty(Args.YLim)
            ylim(Args.YLim);
        end
        title(sprintf('%s (%d calibrators)', Mode, numel(allMag)));
    end
    sgtitle('Calibrator fit residuals vs magnitude');
end
