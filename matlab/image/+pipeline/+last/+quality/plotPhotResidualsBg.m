function plotPhotResidualsBg(PC, Args)
    % Plot calibrator residuals vs 1/Flux to check for additive background
    % Description: If there is a residual background bg in the observed flux:
    %                ObsFlux = TrueFlux + bg
    %                DiffMag ~ -1.086 * bg / ObsFlux   (for small bg/Flux)
    %              This appears as a linear trend in Residual vs 1/Flux with
    %              slope = -1.086 * bg. A fitted line reveals the background
    %              estimate.
    %              Uses percrop calibration only (fit is identical across modes).
    %
    % Input  : - PC struct with PC.percrop{Iv}(Ic) PhotCalibTrans arrays
    %            (from calibratePhotModes .PC output or Result.PC).
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'FitLine'     - Fit and overlay linear trend. Default is true.
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'NTrendBins'  - Number of bins for trend line. Default is 30.
    %            'YLim'        - Y-axis limits. Default is [] (auto).
    %            'XLim'        - X-axis limits for 1/Flux. Default is [] (auto).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotResidualsBg(R.PC);
    %          pipeline.last.quality.plotPhotResidualsBg(R.PC, 'CropsToAnalyze', 10);

    arguments
        PC struct
        Args.CropsToAnalyze = []
        Args.FitLine logical = true
        Args.OverlayTrend   = 'median'
        Args.NTrendBins     = 30
        Args.YLim           = []
        Args.XLim           = []
    end

    if ~isfield(PC, 'percrop')
        return;
    end

    AllInvFlux = [];
    AllRes     = [];
    AllMagAB   = [];

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

            Tab = PCobj.SourceData.Table;
            ColNames = Tab.Properties.VariableNames;
            if ~ismember('Residuals', ColNames) || ~ismember('Flux', ColNames)
                continue;
            end

            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(height(Tab), 1);
            end

            Residuals = Tab.Residuals(UsedMask);
            Flux = Tab.Flux(UsedMask);

            % Get MAG_AB if available
            if ismember('MAG_AB', ColNames)
                MagAB = Tab.MAG_AB(UsedMask);
            else
                MagAB = nan(size(Flux));
            end

            ValidMask = isfinite(Residuals) & isfinite(Flux) & Flux > 0;
            AllInvFlux = [AllInvFlux; 1 ./ Flux(ValidMask)];
            AllRes     = [AllRes; Residuals(ValidMask)];
            AllMagAB   = [AllMagAB; MagAB(ValidMask)];
        end
    end

    if isempty(AllInvFlux)
        warning('plotPhotResidualsBg:NoData', 'No valid data found.');
        return;
    end

    % --- Plot ---
    figure('Name', 'Residuals vs 1/Flux (background check)', ...
           'Position', [50, 50, 600, 500]);
    hold on;

    plot(AllInvFlux, AllRes, '.', 'MarkerSize', 2);
    plot(xlim, [0 0], 'k--');

    % Binned trend
    if ~strcmp(Args.OverlayTrend, 'none')
        XRange = [min(AllInvFlux), max(AllInvFlux)];
        BinWidth = diff(XRange) / Args.NTrendBins;
        TrendFun = str2func(['nan' Args.OverlayTrend]);
        R = timeSeries.bin.binningFast([AllInvFlux(:), AllRes(:)], ...
            BinWidth, XRange, {'MidBin', @numel, TrendFun});
        ValidBins = R(:,2) >= 5;
        plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);
    end

    % Linear fit: Residual = slope * (1/Flux) + intercept
    % slope = -1.086 * bg
    if Args.FitLine
        P = polyfit(AllInvFlux, AllRes, 1);
        XFit = linspace(min(AllInvFlux), max(AllInvFlux), 100);
        plot(XFit, polyval(P, XFit), '-b', 'LineWidth', 2);

        BgEstimate = -P(1) / 1.086;
        text(0.05, 0.95, sprintf('slope = %.4f\nbg \\approx %.1f [flux units]', ...
            P(1), BgEstimate), ...
            'Units', 'normalized', 'VerticalAlignment', 'top', ...
            'FontSize', 10, 'BackgroundColor', 'w');
    end

    box on; grid on;
    xlabel('1 / Flux');
    ylabel('Residual [mag]');
    if ~isempty(Args.XLim)
        xlim(Args.XLim);
    end
    if ~isempty(Args.YLim)
        ylim(Args.YLim);
    end
    % Second X-axis showing MAG_AB
    % Compute median MAG_AB in bins of 1/Flux for tick placement
    ax1 = gca;
    ax2 = axes('Position', ax1.Position, 'XAxisLocation', 'top', ...
               'YAxisLocation', 'right', 'Color', 'none', 'YTick', []);
    ax2.XLim = ax1.XLim;

    ValidAB = isfinite(AllMagAB);
    if any(ValidAB)
        % Place ticks at integer AB magnitudes
        MagTicks = ceil(min(AllMagAB(ValidAB))):floor(max(AllMagAB(ValidAB)));
        % For each mag tick, find median 1/Flux of sources near that mag
        InvFluxTicks = nan(size(MagTicks));
        for It = 1:numel(MagTicks)
            Near = abs(AllMagAB - MagTicks(It)) < 0.5;
            if sum(Near) >= 3
                InvFluxTicks(It) = nanmedian(AllInvFlux(Near));
            end
        end
        KeepTicks = isfinite(InvFluxTicks) & ...
                    InvFluxTicks >= ax1.XLim(1) & InvFluxTicks <= ax1.XLim(2);
        ax2.XTick = InvFluxTicks(KeepTicks);
        ax2.XTickLabel = arrayfun(@(m) sprintf('%d', m), MagTicks(KeepTicks), 'UniformOutput', false);
    end
    xlabel(ax2, 'MAG\_AB');
end
