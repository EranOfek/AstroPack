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

    % -------- Panel 2: mix vs baseline, LOG-LOG EQUAL AXES + POWER FIT --
    nexttile; hold on; grid on; box on;
    % Log-log needs positive values only; drop non-positive samples.
    Xf   = R.RMS_base_theory(:);  Yf = R.RMS_mix(:);
    Fin  = isfinite(Xf) & isfinite(Yf) & Xf > 0 & Yf > 0;
    Xf   = Xf(Fin);   Yf = Yf(Fin);
    Sig  = R.RMS_mix_sigma(:);  Sig = Sig(Fin);
    Comb = [Xf; Yf];
    if isempty(Comb)
        Lo = 1e-3; Hi = 1;
    else
        Q  = quantile(Comb, [0.005 0.995]);
        Lo = max(Q(1), eps);
        Hi = Q(2);
        if ~(Hi > Lo); Hi = Lo * 10; end
    end
    % Order: data first (bottom), then reference lines ON TOP so 1:1 and
    % fit are never occluded by the dense green cloud.
    Hd = errorbar(Xf, Yf, Sig, ...
        'LineStyle','none', 'Marker','.', 'MarkerSize', 8, 'Color', ColMix);
    H1 = plot([Lo Hi], [Lo Hi], 'k-', 'LineWidth', 1.5);                 % 1:1
    % Power-law fit in log-log space: log10(y) = a*log10(x) + b
    %   => y = 10^b * x^a. On log-log axes this draws as a straight line
    % with slope a. Reads as "if slope~=1 or intercept~=0, drift depends
    % on baseline scatter" (slope) or "constant additive drift" (intercept
    % away from 0 in log space is a multiplicative offset).
    if numel(Xf) >= 3
        P = polyfit(log10(Xf), log10(Yf), 1);   % [slope logIntercept]
        Xline = logspace(log10(Lo), log10(Hi), 100);
        Yline = 10.^(P(1)*log10(Xline) + P(2));
        % Bold dotted dark-gray line — contrasts against the green data
        % cloud AND against the thin black 1:1 diagonal.
        Hf = plot(Xline, Yline, ':', 'LineWidth', 3, 'Color', [0.30 0.30 0.30]);
        legend([Hd H1 Hf], {'data', '1:1', ...
                sprintf('fit: y = %.3f x^{%.3f}', 10.^P(2), P(1))}, ...
               'Location', 'northwest');
    end
    set(gca, 'XScale', 'log', 'YScale', 'log');
    xlim([Lo Hi]); ylim([Lo Hi]);
    axis square;
    xlabel('RMS_{base} = sqrt((RMS_A^2 + RMS_B^2)/2)', 'Interpreter','tex');
    ylabel('RMS_{mix} (bootstrap)', 'Interpreter','tex');
    title('mix vs baseline (above 1:1 -> drift, log-log)');

    % -------- Panel 3: excess vs magnitude — trend line only, log Y ----
    nexttile; hold on; grid on; box on;
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
    set(gca, 'YScale', 'log');
    xlabel(sprintf('median %s [mag]', MagCol), 'Interpreter','none');
    ylabel('excess = RMS_{mix} - RMS_{base}');
    title('excess vs magnitude (log Y)');
end
