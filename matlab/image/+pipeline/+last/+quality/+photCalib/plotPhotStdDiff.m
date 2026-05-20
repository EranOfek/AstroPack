function [Result, Fig] = plotPhotStdDiff(MS, Args)
    % Plot per-source Std difference between two magnitude fields.
    % Description: For each crop of a MatchedSources collection, computes
    %              per source the epoch-to-epoch Std of two named magnitude
    %              fields and plots their difference Std(FieldA) - Std(FieldB)
    %              versus the median of FieldA. Positive points mean FieldB
    %              is better (smaller scatter). An optional binned trend line
    %              is overlaid. Replaces the legacy mode-difference variant;
    %              the field-comparison version is what remained useful.
    %
    % Input  : - MS - a MatchedSources array (one element per crop, e.g.
    %            from matchEpochs) or a cell of MatchedSources.
    %          * ...,key,val,...
    %            'FieldA'         - First magnitude field. Required.
    %            'FieldB'         - Second magnitude field. Required.
    %            'CropsToAnalyze' - Crop indices to include. Default [] (all).
    %            'OverlayTrend'   - 'median' | 'mean' | 'none' binned trend.
    %                               Default 'median'.
    %            'TrendBinWidth'  - Bin width [mag]. Default 0.5.
    %            'MinEpochs'      - Drop sources with fewer finite epochs in
    %                               either field. 0 = no cut. Default 0.
    %            'MagRange'       - [min max] x-axis range and trend support.
    %                               Default [9 22].
    % Output : - Result - struct with .MedMag, .DeltaStd (pooled column
    %            vectors), .Stats (overall Std-diff summary), .Args.
    %          - Fig - figure handle ([] when there is no data).
    % Author : photCalib package refactor (2026-05)
    % Example: plotPhotStdDiff(MS, 'FieldA','MAG_AB_PSF', 'FieldB','MAG_CB_PSF');

    arguments
        MS
        Args.FieldA          {mustBeTextScalar}
        Args.FieldB          {mustBeTextScalar}
        Args.CropsToAnalyze  double             = []
        Args.OverlayTrend    {mustBeMember(Args.OverlayTrend,{'median','mean','none'})} = 'median'
        Args.TrendBinWidth   (1,1) double       = 0.5
        Args.MinEpochs       (1,1) double       = 0
        Args.MagRange        (1,2) double       = [9 22]
    end

    Result = struct();
    Fig    = [];

    % --- Normalize MS to a cell of MatchedSources (one per crop) ------
    if isa(MS, 'MatchedSources')
        MSc = num2cell(MS(:).');
    elseif iscell(MS)
        MSc = MS(:).';
    else
        error('plotPhotStdDiff:BadInput', ...
            'MS must be a MatchedSources array or a cell of MatchedSources.');
    end

    Crops = Args.CropsToAnalyze;
    if isempty(Crops); Crops = 1:numel(MSc); end

    % --- Collect per-source Std(A) - Std(B) and Median(A) -------------
    AllMedMag   = [];
    AllDeltaStd = [];
    for Ic = Crops
        if Ic > numel(MSc) || isempty(MSc{Ic}); continue; end
        Msi = MSc{Ic};
        if ~isfield(Msi.Data, Args.FieldA) || ~isfield(Msi.Data, Args.FieldB)
            continue;
        end
        MagA = Msi.Data.(Args.FieldA);
        MagB = Msi.Data.(Args.FieldB);
        if Args.MinEpochs > 0
            Good = sum(~isnan(MagA), 1) >= Args.MinEpochs ...
                 & sum(~isnan(MagB), 1) >= Args.MinEpochs;
            MagA = MagA(:, Good);
            MagB = MagB(:, Good);
        end
        StdA   = std(MagA, 0, 1, 'omitnan');
        StdB   = std(MagB, 0, 1, 'omitnan');
        MedMag = median(MagA, 1, 'omitnan');
        AllMedMag   = [AllMedMag,   MedMag];      %#ok<AGROW>
        AllDeltaStd = [AllDeltaStd, StdA - StdB]; %#ok<AGROW>
    end

    if isempty(AllMedMag)
        warning('photCalib:plotPhotStdDiff:NoData', ...
            'No (%s, %s) pairs found.', Args.FieldA, Args.FieldB);
        return;
    end

    % --- Plot ---------------------------------------------------------
    Fig = figure('Name', sprintf('Std diff: %s vs %s', Args.FieldA, Args.FieldB), ...
                 'Position', [50 50 700 500]);
    hold on;
    plot(AllMedMag, AllDeltaStd, '.', 'MarkerSize', 4);
    plot(Args.MagRange, [0 0], 'k--');

    TrendVals = [];
    TrendMags = [];
    TrendCounts = [];
    if ~strcmp(Args.OverlayTrend, 'none')
        T = binnedTrend(AllMedMag, AllDeltaStd, ...
            'BinWidth', Args.TrendBinWidth, 'Range', Args.MagRange, ...
            'Stat', Args.OverlayTrend, 'MinCount', 5);
        if ~isempty(T.X)
            plot(T.X, T.Val, '-r', 'LineWidth', 2);
            TrendVals   = T.Val;
            TrendMags   = T.X;
            TrendCounts = T.Count;
        end
    end
    if ~isempty(TrendVals)
        [MaxVal, MaxIdx] = max(TrendVals);
        [MinVal, MinIdx] = min(TrendVals);
        text(0.02, 0.97, sprintf( ...
            'max: %.4f @ mag %.1f (N=%d)\nmin: %.4f @ mag %.1f (N=%d)', ...
            MaxVal, TrendMags(MaxIdx), TrendCounts(MaxIdx), ...
            MinVal, TrendMags(MinIdx), TrendCounts(MinIdx)), ...
            'Units', 'normalized', 'VerticalAlignment', 'top', ...
            'FontSize', 8, 'BackgroundColor', 'w');
    end

    box on; grid on;
    xlabel('Median Magnitude');
    ylabel(sprintf('Std(%s) - Std(%s) [mag]', ...
        strrep(Args.FieldA,'_','\_'), strrep(Args.FieldB,'_','\_')));
    xlim(Args.MagRange);
    title(sprintf('%s - %s  (%d sources)', ...
        strrep(Args.FieldA,'_','\_'), strrep(Args.FieldB,'_','\_'), ...
        numel(AllDeltaStd)));

    % --- Output -------------------------------------------------------
    Result.MedMag   = AllMedMag(:);
    Result.DeltaStd = AllDeltaStd(:);
    Result.Stats    = statStruct(AllDeltaStd);
    Result.Args     = Args;
end
