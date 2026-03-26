function plotPhotResidualsAirmass(PC, Args)
    % Plot calibrator residuals vs airmass
    % Description: Shows airmass-dependent systematics in the calibration.
    %              Left panel: all individual calibrator residuals vs airmass.
    %              Right panel: per-epoch median residual vs airmass.
    %              Uses percrop calibration only.
    %
    % Input  : - PC struct with PC.percrop{Iv}(Ic) PhotCalibTrans arrays.
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Normalize'   - Plot Residual/MagErr. Default is false.
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'NTrendBins'  - Number of bins for trend. Default is 20.
    %            'FitLine'     - Fit and overlay linear trend. Default is true.
    %            'YLim'        - Y-axis limits. Default is [] (auto).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotResidualsAirmass(R.PC);
    %          pipeline.last.quality.plotPhotResidualsAirmass(R.PC, 'Normalize', true);

    arguments
        PC struct
        Args.CropsToAnalyze = []
        Args.Normalize logical = false
        Args.OverlayTrend   = 'median'
        Args.NTrendBins     = 20
        Args.FitLine logical = true
        Args.YLim           = []
    end

    if ~isfield(PC, 'percrop')
        return;
    end

    % Collect individual residuals with airmass
    allAM  = [];
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
            if ~PCobj.Success || isempty(PCobj.SourceData); continue; end

            AM = PCobj.AirMass;
            if ~isfinite(AM); continue; end

            Tab = PCobj.SourceData.Table;
            ColNames = Tab.Properties.VariableNames;
            if ~ismember('Residuals', ColNames); continue; end

            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(height(Tab), 1);
            end

            Residuals = Tab.Residuals(UsedMask);

            if Args.Normalize
                if ~ismember('MagErr', ColNames); continue; end
                MagErr = Tab.MagErr(UsedMask);
                Residuals = Residuals ./ MagErr;
            end

            ValidMask = isfinite(Residuals);
            allAM  = [allAM; repmat(AM, sum(ValidMask), 1)];
            allRes = [allRes; Residuals(ValidMask)];

        end
    end

    if isempty(allRes)
        return;
    end

    if Args.Normalize
        ResLabel = 'Residual / MagErr';
    else
        ResLabel = 'Residual [mag]';
    end

    % --- Plot ---
    figure('Name', 'Residuals vs Airmass', 'Position', [50, 50, 600, 500]);
    hold on;

    plot(allAM, allRes, '.', 'MarkerSize', 2);
    plot(xlim, [0 0], 'k--');

    % Binned median trend
    AMRange = [min(allAM), max(allAM)];
    BinWidth = max(diff(AMRange) / Args.NTrendBins, 0.01);
    R = timeSeries.bin.binningFast([allAM(:), allRes(:)], ...
        BinWidth, AMRange, {'MidBin', @numel, @nanmedian});
    ValidBins = R(:,2) >= 5;
    plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);

    % Show median shift value
    MedShift = nanmedian(allRes);
    plot(xlim, [MedShift MedShift], '--m', 'LineWidth', 1.5);
    text(0.05, 0.95, sprintf('median shift = %.4f mag', MedShift), ...
        'Units', 'normalized', 'VerticalAlignment', 'top', ...
        'FontSize', 10, 'BackgroundColor', 'w');

    if Args.FitLine
        P = polyfit(allAM, allRes, 1);
        XFit = linspace(min(allAM), max(allAM), 100);
        plot(XFit, polyval(P, XFit), '-b', 'LineWidth', 2);
        text(0.05, 0.85, sprintf('slope = %.4f', P(1)), ...
            'Units', 'normalized', 'VerticalAlignment', 'top', ...
            'FontSize', 10, 'BackgroundColor', 'w');
    end

    box on; grid on;
    xlabel('Airmass');
    ylabel(ResLabel);
    if ~isempty(Args.YLim)
        ylim(Args.YLim);
    end
    title(sprintf('Residuals vs airmass (%d calibrators, %d epochs)', ...
        numel(allRes), Nvisits));
end
