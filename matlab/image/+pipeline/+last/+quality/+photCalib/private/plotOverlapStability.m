function plotOverlapStability(R, MagCol, StdMethod, TitleTag)
    % Shared 3-panel plot for overlapPairStability / overlapPoolStability.
    % Panels:
    %   1. STD-vs-mag scatter for A-only, B-only, random-mix, each with a
    %      binned-median trend line in the same colour.
    %   2. Scatter RMS_mix vs RMS_base_theory + 1:1 line, on equal, square
    %      axes so drift shows as vertical departure from the diagonal.
    %   3. Excess (= RMS_mix - RMS_base_theory) vs median magnitude, with
    %      a binned-median trend line.
    % Input  : - R struct with fields RMS_A, RMS_B, RMS_mix, RMS_mix_sigma,
    %                                 RMS_base_theory, Excess, MedMag, NPairs.
    %          - MagCol   char, magnitude column name (for axis labels).
    %          - StdMethod char, 'robust'|'plain' (for y-axis label).
    %          - TitleTag char, extra title tag (crop pair or pool label).

    ColA   = [0.20 0.45 0.85];
    ColB   = [0.85 0.10 0.10];
    ColMix = [0.20 0.65 0.20];

    figure('WindowStyle','docked','Color',[1 1 1]);
    tiledlayout(1, 3, 'Padding','compact', 'TileSpacing','compact');

    % -------- Panel 1: STD vs mag, three series overlaid ----------------
    nexttile; hold on; grid on; box on;
    plot(R.MedMag, R.RMS_A,   '.', 'MarkerSize', 5, 'Color', [ColA   0.35]);
    plot(R.MedMag, R.RMS_B,   '.', 'MarkerSize', 5, 'Color', [ColB   0.35]);
    plot(R.MedMag, R.RMS_mix, '.', 'MarkerSize', 5, 'Color', [ColMix 0.35]);

    % Binned trend lines share bin edges so trends read at the same X.
    if numel(R.MedMag) >= 20
        Mlo = min(R.MedMag, [], 'omitnan');
        Mhi = max(R.MedMag, [], 'omitnan');
        Bw  = max(0.25, (Mhi - Mlo) / 20);
        TA = binnedTrend(R.MedMag(:), R.RMS_A(:),   'BinWidth', Bw, 'MinCount', 5, 'Stat', 'median');
        TB = binnedTrend(R.MedMag(:), R.RMS_B(:),   'BinWidth', Bw, 'MinCount', 5, 'Stat', 'median');
        TM = binnedTrend(R.MedMag(:), R.RMS_mix(:), 'BinWidth', Bw, 'MinCount', 5, 'Stat', 'median');
        if ~isempty(TA.X); plot(TA.X, TA.Val, '-', 'LineWidth', 2, 'Color', ColA);   end
        if ~isempty(TB.X); plot(TB.X, TB.Val, '-', 'LineWidth', 2, 'Color', ColB);   end
        if ~isempty(TM.X); plot(TM.X, TM.Val, '-', 'LineWidth', 2, 'Color', ColMix); end
    end
    set(gca, 'YScale', 'log');
    xlabel(sprintf('median %s [mag]', MagCol), 'Interpreter','none');
    ylabel(sprintf('%s STD(%s) [mag]', StdMethod, MagCol), 'Interpreter','none');
    legend({'A only', 'B only', 'random mix'}, 'Location','best');
    title(sprintf('STD vs magnitude  (%s)', TitleTag), 'Interpreter','none');

    % -------- Panel 2: mix vs baseline, EQUAL AXES ----------------------
    nexttile; hold on; grid on; box on;
    Combined = [R.RMS_base_theory(:); R.RMS_mix(:)];
    Combined = Combined(isfinite(Combined) & Combined > 0);
    if isempty(Combined)
        Lo = 0; Hi = 1;
    else
        Q  = quantile(Combined, [0.01 0.99]);
        Lo = min(Q(1), 0);
        Hi = Q(2);
        if ~(Hi > Lo); Hi = Lo + eps; end
    end
    plot([Lo Hi], [Lo Hi], 'k-', 'LineWidth', 1);
    errorbar(R.RMS_base_theory, R.RMS_mix, R.RMS_mix_sigma, ...
        'LineStyle','none', 'Marker','.', 'MarkerSize', 8, 'Color', ColMix);
    xlim([Lo Hi]); ylim([Lo Hi]);
    axis square;                      % square panel + shared limits
    xlabel('RMS_{base} = sqrt((RMS_A^2 + RMS_B^2)/2)', 'Interpreter','tex');
    ylabel('RMS_{mix} (bootstrap)', 'Interpreter','tex');
    title('mix vs baseline (above 1:1 -> drift)');

    % -------- Panel 3: excess vs magnitude ------------------------------
    nexttile; hold on; grid on; box on;
    plot(R.MedMag, R.Excess, '.', 'MarkerSize', 5, 'Color', [0.5 0.5 0.5 0.35]);
    yline(0, 'k:', 'LineWidth', 1);
    if numel(R.MedMag) >= 20
        Mlo = min(R.MedMag, [], 'omitnan');
        Mhi = max(R.MedMag, [], 'omitnan');
        Bw  = max(0.25, (Mhi - Mlo) / 20);
        TE = binnedTrend(R.MedMag(:), R.Excess(:), 'BinWidth', Bw, ...
                         'MinCount', 5, 'Stat', 'median');
        if ~isempty(TE.X)
            plot(TE.X, TE.Val, 'r-o', 'LineWidth', 2, ...
                 'MarkerFaceColor', 'r', 'MarkerEdgeColor', 'k', 'MarkerSize', 6);
        end
    end
    xlabel(sprintf('median %s [mag]', MagCol), 'Interpreter','none');
    ylabel('excess = RMS_{mix} - RMS_{base}');
    title('excess vs magnitude');
end
