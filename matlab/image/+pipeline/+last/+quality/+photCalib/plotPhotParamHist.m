function Result = plotPhotParamHist(PC, Args)
    % Histogram of one named photometric-calibration quantity
    % Description: Collects one value per (epoch, crop) from a
    %              PhotCalibTrans collection and plots a histogram with
    %              optional summary statistics. Supports the same name
    %              registry as plotPhotParamScatter (resolved via
    %              pipeline.last.quality.photCalib.resolvePCParam).
    %              Multi-set input: pass a cell of PC sources to pool
    %              all data into one combined distribution.
    % Input  : - PC source. Either:
    %              * a single source (struct, PC array, cell-of-arrays —
    %                anything resolvePC accepts), OR
    %              * a cell of multiple such sources, e.g.
    %                {R1.PC, R2.PC, R3.PC}. Values from all sets are
    %                pooled into one combined histogram.
    %          * ...,key,val,...
    %            'Param'   - Name of the quantity. Default is 'Chi2_DOF'.
    %                        See resolvePCParam for accepted names.
    %            'Bins'    - Number of bins, or a vector of bin edges.
    %                        Default is 30.
    %            'XLim'    - Plot x-limits [min max]. Default [] (auto).
    %            'LogX'    - Use a log-transformed x axis (and bin in log
    %                        space). Default false.
    %            'CropsToAnalyze' - Crop indices to include. Default []
    %                        (all crops).
    %            'Stats'   - Overlay median + 16/84 percentiles + N as
    %                        text. Default true.
    %            'Color'   - Histogram bar color. Default [0.30 0.55 0.90].
    %            'Labels'  - Cell of names per set (multi-set input only).
    %                        Default {} (auto-generated 'set 1', 'set 2', ...).
    %            'Verbose' - Print summary to console. Default false.
    % Output : - Result struct with fields:
    %            .Values    - vector of collected values (NaNs dropped).
    %            .CropIDs   - crop ID per value.
    %            .EpochIDs  - epoch (visit) index per value.
    %            .Stats     - struct with .N, .Median, .Mean, .Std,
    %                         .P16, .P84, .Min, .Max
    %            .Param     - the name resolved.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.photCalib.plotPhotParamHist(R.PC);
    %          pipeline.last.quality.photCalib.plotPhotParamHist(R.PC, ...
    %              'Param', 'Chi2_DOF', 'Bins', 50);
    %          pipeline.last.quality.photCalib.plotPhotParamHist(R.PC, ...
    %              'Param', 'NCalib', 'LogX', false);

    arguments
        PC
        Args.Param char = 'Chi2_DOF'
        Args.Bins = 30
        Args.XLim = []
        Args.LogX logical = false
        Args.CropsToAnalyze = []
        Args.Stats logical = true
        Args.Color = [0.30 0.55 0.90]
        Args.Labels cell = {}
        Args.Verbose logical = false
    end

    % Detect multi-set input: cell where each element is itself a PC source
    [Sources, IsMulti] = localSplitSources(PC);
    if IsMulti && isempty(Args.Labels)
        Args.Labels = arrayfun(@(k) sprintf('set %d', k), 1:numel(Sources), ...
            'UniformOutput', false);
    elseif ~IsMulti
        Args.Labels = {''};
    end

    % Collect values from each source, pooled into one combined sample
    Vals     = [];
    CropIDs  = [];
    EpochIDs = [];
    SetIDs   = [];

    for Is = 1:numel(Sources)
        PCcell_s = pipeline.last.quality.photCalib.resolvePC(Sources{Is});

        for Iv = 1:numel(PCcell_s)
            if isempty(PCcell_s{Iv}); continue; end
            if isempty(Args.CropsToAnalyze)
                CropsToUse = 1:numel(PCcell_s{Iv});
            else
                CropsToUse = Args.CropsToAnalyze;
            end
            for Ic = CropsToUse
                if Ic > numel(PCcell_s{Iv}); continue; end
                PCobj = PCcell_s{Iv}(Ic);
                if ~PCobj.Success; continue; end
                V = pipeline.last.quality.photCalib.resolvePCParam(PCobj, Args.Param);
                if ~isfinite(V); continue; end
                Vals(end+1,1)     = V;          %#ok<AGROW>
                CropIDs(end+1,1)  = Ic;         %#ok<AGROW>
                EpochIDs(end+1,1) = Iv;         %#ok<AGROW>
                SetIDs(end+1,1)   = Is;         %#ok<AGROW>
            end
        end
    end

    Result.Values   = Vals;
    Result.CropIDs  = CropIDs;
    Result.EpochIDs = EpochIDs;
    Result.SetIDs   = SetIDs;
    Result.Labels   = Args.Labels;
    Result.Param    = Args.Param;

    if isempty(Vals)
        Result.Stats = struct('N', 0);
        warning('plotPhotParamHist:Empty', ...
            'No finite values collected for "%s".', Args.Param);
        return;
    end

    % Statistics
    S.N      = numel(Vals);
    S.Median = median(Vals);
    S.Mean   = mean(Vals);
    S.Std    = std(Vals);
    S.P16    = prctile(Vals, 16);
    S.P84    = prctile(Vals, 84);
    S.Min    = min(Vals);
    S.Max    = max(Vals);
    Result.Stats = S;

    if Args.Verbose
        fprintf('--- %s histogram ---\n', Args.Param);
        if numel(Sources) > 1
            for Is = 1:numel(Sources)
                Ns = sum(SetIDs == Is);
                fprintf('  %-12s : N = %d\n', Args.Labels{Is}, Ns);
            end
        end
        fprintf('  N    = %d\n', S.N);
        fprintf('  Med  = %.4g  (16 / 84 pct: %.4g / %.4g)\n', ...
            S.Median, S.P16, S.P84);
        fprintf('  Mean = %.4g  +/- %.4g\n', S.Mean, S.Std);
        fprintf('  Range= [%.4g, %.4g]\n', S.Min, S.Max);
    end

    % Plotting values (log or linear). All text is rendered with
    % Interpreter='none' so column names like 'Chi2_DOF' display verbatim
    % (no TeX subscript interpretation).
    if Args.LogX
        Plot = log10(Vals(Vals > 0));
        XLab = sprintf('log10(%s)', Args.Param);
    else
        Plot = Vals;
        XLab = Args.Param;
    end

    % Bin edges
    if isscalar(Args.Bins)
        if isempty(Args.XLim)
            Edges = linspace(min(Plot), max(Plot), Args.Bins + 1);
        else
            EL = Args.XLim;
            if Args.LogX; EL = log10(EL); end
            Edges = linspace(EL(1), EL(2), Args.Bins + 1);
        end
    else
        Edges = Args.Bins;
        if Args.LogX; Edges = log10(Edges); end
    end

    figure;
    histogram(Plot, Edges, 'FaceColor', Args.Color, 'EdgeColor', 'k');
    xlabel(XLab, 'Interpreter', 'none');
    ylabel('Count');
    if numel(Sources) > 1
        TStr = sprintf('%s  (N = %d, pooled over %d sets)', ...
            Args.Param, S.N, numel(Sources));
    else
        TStr = sprintf('%s  (N = %d)', Args.Param, S.N);
    end
    title(TStr, 'Interpreter', 'none');
    if ~isempty(Args.XLim); xlim(Args.XLim); end
    grid on; box on;

    % Stats overlay
    if Args.Stats
        hold on;
        YLim = ylim;
        if Args.LogX
            MedX = log10(S.Median);
            P16X = log10(S.P16);
            P84X = log10(S.P84);
        else
            MedX = S.Median; P16X = S.P16; P84X = S.P84;
        end
        plot([MedX MedX], YLim, 'k-',  'LineWidth', 1.4);
        plot([P16X P16X], YLim, 'k--', 'LineWidth', 0.8);
        plot([P84X P84X], YLim, 'k--', 'LineWidth', 0.8);

        Txt = sprintf( ...
            'N = %d\nMed = %.4g\n[P16, P84] = [%.4g, %.4g]\nMean = %.4g\nStd = %.4g', ...
            S.N, S.Median, S.P16, S.P84, S.Mean, S.Std);
        XL = xlim;
        text(XL(1) + 0.65*(XL(2)-XL(1)), YLim(1) + 0.92*(YLim(2)-YLim(1)), ...
            Txt, 'VerticalAlignment','top', 'HorizontalAlignment','left', ...
            'BackgroundColor', [1 1 1 0.85], 'EdgeColor','k', ...
            'FontSize', 9, 'Interpreter', 'none');
        hold off;
    end
end

% -------------------------------------------------------------------------
function [Sources, IsMulti] = localSplitSources(PC)
    % Decide whether PC is a single source or a cell of multiple sources.
    %   Multi-set heuristic: PC is a cell with >1 elements where each
    %   element is itself a "source" (struct with .PC.percrop, struct
    %   with .percrop, cell of arrays, or non-scalar PhotCalibTrans).
    if iscell(PC) && numel(PC) > 1 && all(cellfun(@isSourceLike, PC))
        Sources = PC(:)';
        IsMulti = true;
    else
        Sources = {PC};
        IsMulti = false;
    end
end

function tf = isSourceLike(x)
    tf = isstruct(x) || ...
         (iscell(x) && ~isempty(x) && (isa(x{1}, 'PhotCalibTrans') || isstruct(x{1}))) || ...
         (isa(x, 'PhotCalibTrans') && numel(x) > 1);
end
