function Fig = plotPhotResiduals(PC, Args)
    % Plot calibrator fit residuals against a chosen quantity.
    % Description: Pools the used calibrators' residuals from all epochs and
    %              crops of the supplied PhotCalibTrans objects and plots
    %              them against the quantity named by 'XAxis'. With
    %              YStat='residual' it draws a residual scatter with an
    %              optional binned trend; with YStat='rms' it draws the
    %              binned residual RMS on a logarithmic y-axis.
    %
    %              Replaces the residual-vs-X family: plotPhotResiduals,
    %              plotPhotResidualsAirmass, plotPhotResidualsBg,
    %              plotPhotResidualsVs, plotPhotResidualsXY,
    %              plotPhotResidualsRMS. (Residual-vs-GAIA-colour remains in
    %              plotPhotResidualsColor, which needs an external catalogue
    %              cross-match.)
    %
    % Input  : - PC - a PhotCalibTrans array, a cell of such arrays, or a
    %            struct with a .PC field (anything resolveInput accepts).
    %          * ...,key,val,...
    %            'XAxis'          - X quantity:
    %                                 'mag'          - MAG_AB column
    %                                 'instrumental' - -2.5*log10(Flux)
    %                                 'airmass'      - PhotCalibTrans.AirMass
    %                                 'background'   - 1/Flux
    %                                 <column>       - a SourceData column
    %                                 'A-B'          - column difference
    %                               Default 'mag'.
    %            'YStat'          - 'residual' (scatter + trend) or 'rms'
    %                               (binned RMS, log y-axis). Default
    %                               'residual'.
    %            'CropsToAnalyze' - Crop indices. Default [] (all).
    %            'Normalize'      - Plot Residual/MagErr. Default false.
    %            'OverlayTrend'   - 'median' | 'mean' | 'none' binned trend
    %                               (residual mode). Default 'median'.
    %            'TrendBinWidth'  - X bin width. Default [] (= X data range /
    %                               TrendBins).
    %            'TrendBins'      - Bin count when TrendBinWidth is empty.
    %                               Default 30.
    %            'MinCount'       - Minimum calibrators per trend/RMS bin.
    %                               Default 5.
    %            'FitLine'        - Overlay a linear fit and report its slope
    %                               (residual mode). Default false.
    %            'ShowMedianShift'- Overlay the overall median residual as a
    %                               horizontal line (residual mode).
    %                               Default false.
    %            'YLim'           - Y-axis limits. Default [] (auto).
    % Output : - Fig - the created figure handle ([] when there is no data).
    % Author : photCalib package refactor (2026-05)
    % Example: plotPhotResiduals(R.PC, 'XAxis', 'mag');
    %          plotPhotResiduals(R.PC, 'XAxis', 'airmass', 'FitLine', true);
    %          plotPhotResiduals(R.PC, 'XAxis', 'mag', 'YStat', 'rms');
    %          plotPhotResiduals(R.PC, 'XAxis', 'X-XPEAK');

    arguments
        PC
        Args.XAxis           {mustBeTextScalar} = 'mag'
        Args.YStat           {mustBeMember(Args.YStat,{'residual','rms'})} = 'residual'
        Args.CropsToAnalyze  double  = []
        Args.Normalize       logical = false
        Args.OverlayTrend    {mustBeMember(Args.OverlayTrend,{'median','mean','none'})} = 'median'
        Args.TrendBinWidth            = []
        Args.TrendBins       (1,1) double = 30
        Args.MinCount        (1,1) double = 5
        Args.FitLine         logical = false
        Args.ShowMedianShift logical = false
        Args.YLim                    = []
    end

    Fig = [];
    PCcell = resolveInput(PC);
    if isempty(PCcell); return; end

    % Map XAxis -> collectResiduals XField + axis label
    switch lower(Args.XAxis)
        case 'mag'
            XField = 'MAG_AB';        XLabel = 'MAG\_AB';
        case 'instrumental'
            XField = 'instrumental';  XLabel = '-2.5 log_{10}(Flux)';
        case 'airmass'
            XField = 'AirMass';       XLabel = 'Airmass';
        case 'background'
            XField = 'InvFlux';       XLabel = '1 / Flux';
        otherwise
            XField = Args.XAxis;      XLabel = strrep(Args.XAxis, '_', '\_');
    end

    D = collectResiduals(PCcell, 'CropsToAnalyze', Args.CropsToAnalyze, ...
        'XField', XField, 'Normalize', Args.Normalize);
    if isempty(D.Residual)
        warning('photCalib:plotPhotResiduals:NoData', ...
            'No valid residual data for XAxis=%s.', Args.XAxis);
        return;
    end

    if Args.Normalize
        YLabel = 'Residual / MagErr';
    else
        YLabel = 'Residual [mag]';
    end

    Xmin = min(D.X);
    Xmax = max(D.X);
    if ~isempty(Args.TrendBinWidth)
        BW = Args.TrendBinWidth;
    else
        BW = (Xmax - Xmin) / max(Args.TrendBins, 1);
    end
    CanBin = isfinite(BW) && BW > 0 && Xmax > Xmin;

    if strcmp(Args.YStat, 'rms')
        % --- Binned residual RMS ---------------------------------------
        Fig = figure('Name', sprintf('Residual RMS vs %s', Args.XAxis), ...
                     'Position', [50 50 620 500]);
        hold on;
        if CanBin
            T = binnedTrend(D.X, D.Residual, 'BinWidth', BW, ...
                'Range', [Xmin Xmax], 'Stat', 'median', 'MinCount', Args.MinCount);
            plot(T.X, T.Std, '-r', 'LineWidth', 2);
        end
        set(gca, 'YScale', 'log');
        YLabel = 'Residual RMS [mag]';
    else
        % --- Residual scatter + trend ----------------------------------
        Fig = figure('Name', sprintf('Residuals vs %s', Args.XAxis), ...
                     'Position', [50 50 620 500]);
        hold on;
        plot(D.X, D.Residual, '.', 'MarkerSize', 3);
        plot(xlim, [0 0], 'k--');

        if ~strcmp(Args.OverlayTrend, 'none') && CanBin
            T = binnedTrend(D.X, D.Residual, 'BinWidth', BW, ...
                'Range', [Xmin Xmax], 'Stat', Args.OverlayTrend, ...
                'MinCount', Args.MinCount);
            plot(T.X, T.Val, '-r', 'LineWidth', 2);
        end

        if Args.ShowMedianShift
            MShift = median(D.Residual, 'omitnan');
            plot(xlim, [MShift MShift], '--m', 'LineWidth', 1.5);
            text(0.05, 0.95, sprintf('median = %.4f', MShift), ...
                'Units', 'normalized', 'VerticalAlignment', 'top', ...
                'FontSize', 10, 'BackgroundColor', 'w');
        end

        if Args.FitLine
            Cf = polyfit(D.X, D.Residual, 1);
            XFit = linspace(Xmin, Xmax, 100);
            plot(XFit, polyval(Cf, XFit), '-b', 'LineWidth', 2);
            text(0.05, 0.85, sprintf('slope = %.4g', Cf(1)), ...
                'Units', 'normalized', 'VerticalAlignment', 'top', ...
                'FontSize', 10, 'BackgroundColor', 'w');
        end
    end

    box on; grid on;
    xlabel(XLabel);
    ylabel(YLabel);
    if ~isempty(Args.YLim); ylim(Args.YLim); end
    title(sprintf('Calibrator residuals vs %s (%d calibrators, %d epochs)', ...
        Args.XAxis, D.NCalib, D.NEpoch));
end
