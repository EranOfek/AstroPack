function Result = plotPhotParamCompare(PC_A, PC_B, Args)
    % Pairwise scatter of one named PC quantity from two PhotCalibTrans arrays
    % Description: For two PhotCalibTrans arrays of equal crop count
    %              (typically 1x24 each — one per crop of two coadds /
    %              versions / pipelines), resolves a single named quantity
    %              per crop in each array and plots them against each
    %              other on a scatter diagram with a 1:1 reference line.
    %              Comparison statistics are reported in RELATIVE units.
    %              By default the denominator is the parameter's
    %              ALLOWED-VARIATION RANGE (Max - Min) read from the
    %              PC's TransModel.getAllFunPar registry, which is
    %              populated from imUtil.calib.predefSeqCompositeFun
    %              (e.g. Norm: [0.001, 1.0]; TauAod500: [0.01, 1.0];
    %              PWV_cm: [0.1, 10]; Center_Ang: [5000, 6000]).
    %              Statistics:
    %                * median (B - A) / Range  (or / A when no Range)
    %                * std    (B - A) / Range
    %                * Pearson R between A and B
    %              with separate central/peripheral subsets when
    %              CentralEdge=true. If the parameter has no registered
    %              Min/Max (e.g. RMS, Chi2, NCalib, header scalars), the
    %              denominator falls back to A and Result.RelMode is
    %              tagged 'A' instead of 'Range'. Crops with A == 0 in
    %              that fallback path are excluded from the reduction.
    %              The quantity name is resolved via
    %              resolvePCParam (private helper), so
    %              all the usual names work: 'ARMS', 'RMS', 'Chi2',
    %              'Chi2_DOF', 'NCalib', 'NCalibRetention', 'MedResidual',
    %              fitted transmission params (TauAod500, PWV_cm, ...),
    %              header scalars (AirMass, Temp, ExpTime, ...).
    %
    % Input  : - PC_A  PhotCalibTrans array (1xNcrop) - x-axis source.
    %          - PC_B  PhotCalibTrans array (1xNcrop) - y-axis source.
    %          * ...,key,val,...
    %            'Param'           - Quantity name (char). Default 'ARMS'.
    %            'CropsToAnalyze'  - Crop indices to include. Default []
    %                                (all crops 1..min(numel(PC_A),numel(PC_B))).
    %            'LabelA','LabelB' - Axis labels for the two sources.
    %                                Defaults 'A' and 'B'.
    %            'LogX','LogY'     - Log scale on x / y axis. Default false.
    %            'ShowOneToOne'    - Draw the 1:1 dashed reference line.
    %                                Default true.
    %            'CropLabels'      - Annotate each dot with its crop index.
    %                                Default true.
    %            'CentralEdge'     - Central crops as filled dots,
    %                                peripheral as open circles, plus
    %                                separate median+std of (B-A)/A and
    %                                Pearson R for each subset.
    %                                Default false.
    %            'TileOrder'       - For CentralEdge: 'colmajor'|'rowmajor'.
    %                                Default 'rowmajor'.
    %            'Mode'            - 'scatter' (default): Y vs X with 1:1
    %                                line, med(B-A)/A%, std(B-A)/A% and
    %                                Pearson R in the title.
    %                                'diff'             : (Y-X) vs X with
    %                                a zero line and median-offset line.
    %                                'hist'             : histogram of (Y-X)
    %                                with median + zero markers.
    %            'Bins'            - Number of bins for Mode='hist'. Default 12.
    %            'MarkerSize'      - Marker size. Default 8.
    %            'Color'           - Marker color. Default [0.30 0.55 0.90].
    %            'ShowTitle'       - Show auto title. Default true.
    %            'Title'           - Override the auto title. Default ''.
    %            'Verbose'         - Print summary. Default false.
    % Output : - Result struct with fields:
    %            .X, .Y      - per-crop quantity values used for the plot
    %                          (after dropping non-finite / non-positive).
    %            .Diff       - Y - X (absolute, in the quantity's units).
    %            .RelDiff    - (Y - X) / Range (or / X when no Range).
    %            .RelMode    - 'Range' (default for fitted transmission
    %                          parameters) or 'A' (fallback when no
    %                          Min/Max are registered for Param).
    %            .Range      - allowed-variation range used as denominator,
    %                          NaN when RelMode == 'A'.
    %            .DiffStats  - struct with absolute fields .Mean, .Median,
    %                          .Std of Diff and relative fields
    %                          .MedianRel, .StdRel (and central /
    %                          peripheral variants when CentralEdge=true).
    %            .Pearson    - struct with .R overall and .RCentral /
    %                          .RPeripheral when CentralEdge=true.
    %            .CropIDs    - crop ID per kept point.
    %            .Param, .LabelA, .LabelB, .Mode - echoed.
    % Author : D. Kovaleva (May 2026)
    % Example: % Compare ARMS per crop between two pipeline versions:
    %          [~, PC_A] = imProc.calib.fitPhotCalibTrans(Coadd_v0);
    %          [~, PC_B] = imProc.calib.fitPhotCalibTrans(Coadd_v1);
    %          R = pipeline.last.quality.photCalib.plotPhotParamCompare( ...
    %                  PC_A, PC_B, 'Param', 'ARMS', ...
    %                  'LabelA','v0','LabelB','v1');

    arguments
        PC_A   PhotCalibTrans
        PC_B   PhotCalibTrans
        Args.Param           (1,:) char         = 'ARMS'
        Args.CropsToAnalyze                     = []
        Args.LabelA          (1,:) char         = 'A'
        Args.LabelB          (1,:) char         = 'B'
        Args.LogX            logical            = false
        Args.LogY            logical            = false
        Args.ShowOneToOne    logical            = true
        Args.CropLabels      logical            = false
        Args.CentralEdge     logical            = false
        Args.TileOrder       (1,:) char         = 'rowmajor'
        Args.Mode            (1,:) char ...
            {mustBeMember(Args.Mode, {'scatter','diff','hist'})} = 'scatter'
        Args.Bins                                = 12
        Args.MarkerSize                          = 8
        Args.Color                               = [0.30 0.55 0.90]
        Args.ShowTitle       logical            = true
        Args.Title           (1,:) char         = ''
        Args.Verbose         logical            = false
    end

    % --- Crop selection -------------------------------------------------
    Ncrop = min(numel(PC_A), numel(PC_B));
    if isempty(Args.CropsToAnalyze)
        CropIdx = 1:Ncrop;
    else
        CropIdx = Args.CropsToAnalyze(:).';
        CropIdx = CropIdx(CropIdx >= 1 & CropIdx <= Ncrop);
    end

    % --- Resolve param per crop in each set -----------------------------
    Xv = nan(numel(CropIdx), 1);
    Yv = nan(numel(CropIdx), 1);
    for K = 1:numel(CropIdx)
        Ic = CropIdx(K);
        Xv(K) = resolvePCParam(PC_A(Ic), Args.Param);
        Yv(K) = resolvePCParam(PC_B(Ic), Args.Param);
    end
    Cid = CropIdx(:);

    Keep = isfinite(Xv) & isfinite(Yv);
    if Args.LogX; Keep = Keep & Xv > 0; end
    if Args.LogY; Keep = Keep & Yv > 0; end
    if ~any(Keep)
        error('plotPhotParamCompare:Empty', ...
            'No finite (%s) values to compare.', Args.Param);
    end
    Xv  = Xv(Keep);
    Yv  = Yv(Keep);
    Cid = Cid(Keep);

    % --- Central / peripheral classification ----------------------------
    CentralCrops = centralCrops(Args.TileOrder);
    IsCentral = ismember(Cid, CentralCrops);

    % --- Comparison statistics ------------------------------------------
    % Per-crop relative difference (B - A) normalised by the allowed
    % variation range of the parameter (Max - Min from the PC's
    % getAllFunPar registry, which is populated by
    % imUtil.calib.predefSeqCompositeFun). If the parameter has no
    % registered Min/Max (e.g. RMS, Chi2, NCalib, header scalars), fall
    % back to dividing by A (and tag .RelMode = 'A' instead of 'Range').
    Diff = Yv - Xv;
    [Range, RelMode] = paramRange(PC_A, Args.Param, Args.Verbose);
    if strcmp(RelMode, 'Range')
        RelDiff = Diff ./ Range;
    else
        RelDiff = Diff ./ Xv;
        RelDiff(~isfinite(RelDiff)) = NaN;   % guard against Xv == 0
    end
    Mfin = isfinite(RelDiff);

    DiffStats.N           = numel(Diff);
    DiffStats.Mean        = mean(Diff);
    DiffStats.Median      = median(Diff);
    DiffStats.Std         = std(Diff);
    DiffStats.MedianRel   = median(RelDiff(Mfin));
    DiffStats.StdRel      = std(RelDiff(Mfin));
    if Args.CentralEdge && any(IsCentral) && any(~IsCentral)
        DiffStats.MedianRelCentral    = median(RelDiff(IsCentral & Mfin));
        DiffStats.StdRelCentral       = std(RelDiff(IsCentral & Mfin));
        DiffStats.MedianRelPeripheral = median(RelDiff(~IsCentral & Mfin));
        DiffStats.StdRelPeripheral    = std(RelDiff(~IsCentral & Mfin));
    else
        DiffStats.MedianRelCentral    = NaN;
        DiffStats.StdRelCentral       = NaN;
        DiffStats.MedianRelPeripheral = NaN;
        DiffStats.StdRelPeripheral    = NaN;
    end

    Pearson.R = corr(Xv, Yv);
    if Args.CentralEdge && any(IsCentral) && any(~IsCentral)
        Pearson.RCentral    = corr(Xv(IsCentral),  Yv(IsCentral));
        Pearson.RPeripheral = corr(Xv(~IsCentral), Yv(~IsCentral));
    else
        Pearson.RCentral    = NaN;
        Pearson.RPeripheral = NaN;
    end

    if Args.Verbose
        fprintf('plotPhotParamCompare: Param=%s, N=%d crops\n', Args.Param, numel(Xv));
        fprintf('  median %s_%s = %.4g\n', Args.Param, Args.LabelA, median(Xv));
        fprintf('  median %s_%s = %.4g\n', Args.Param, Args.LabelB, median(Yv));
        if strcmp(RelMode, 'Range')
            fprintf('  Range (Max-Min) = %.4g\n', Range);
            fprintf('  median (B-A)/Range = %+.3g %%   std = %.3g %%   Pearson R = %+.3f\n', ...
                100*DiffStats.MedianRel, 100*DiffStats.StdRel, Pearson.R);
        else
            fprintf('  median (B-A)/A = %+.3g %%   std = %.3g %%   Pearson R = %+.3f\n', ...
                100*DiffStats.MedianRel, 100*DiffStats.StdRel, Pearson.R);
        end
        if Args.CentralEdge && ~isnan(DiffStats.MedianRelCentral)
            fprintf('  central:    %+.3g %% +- %.3g %%   R = %+.3f\n', ...
                100*DiffStats.MedianRelCentral, 100*DiffStats.StdRelCentral, ...
                Pearson.RCentral);
            fprintf('  peripheral: %+.3g %% +- %.3g %%   R = %+.3f\n', ...
                100*DiffStats.MedianRelPeripheral, 100*DiffStats.StdRelPeripheral, ...
                Pearson.RPeripheral);
        end
    end

    % --- Plot -----------------------------------------------------------

    figure; hold on;
    switch lower(Args.Mode)
        case 'scatter'
            renderScatter(Xv, Yv, Cid, IsCentral, Args, DiffStats, Pearson, RelMode);

        case 'diff'
            % X-axis = value in set A; Y-axis = (B - A)
            if Args.CentralEdge
                plot(Xv(IsCentral),  Diff(IsCentral),  'o', ...
                    'MarkerFaceColor', Args.Color, 'MarkerEdgeColor', Args.Color, ...
                    'MarkerSize', Args.MarkerSize, 'DisplayName', 'central');
                plot(Xv(~IsCentral), Diff(~IsCentral), 'o', ...
                    'MarkerFaceColor', 'none', 'MarkerEdgeColor', Args.Color, ...
                    'LineWidth', 1.0, 'MarkerSize', Args.MarkerSize, ...
                    'DisplayName', 'peripheral');
                legend('Location','best','Interpreter','none');
            else
                plot(Xv, Diff, 'o', ...
                    'MarkerFaceColor', Args.Color, 'MarkerEdgeColor', Args.Color, ...
                    'MarkerSize', Args.MarkerSize);
            end
            if Args.CropLabels
                for K = 1:numel(Xv)
                    text(Xv(K), Diff(K), sprintf(' %d', Cid(K)), ...
                        'FontSize', 8, 'Interpreter', 'none');
                end
            end
            if Args.LogX; set(gca, 'XScale', 'log'); end
            % Reference: zero line + median offset
            XL = xlim;
            plot(XL, [0 0], 'k-', 'HandleVisibility','off');
            plot(XL, [DiffStats.Median DiffStats.Median], 'r--', ...
                'LineWidth', 1.2, 'HandleVisibility','off');
            grid on; box on;
            xlabel(sprintf('%s  [%s]', Args.Param, Args.LabelA), 'Interpreter','none');
            ylabel(sprintf('%s  [%s - %s]', Args.Param, Args.LabelB, Args.LabelA), ...
                'Interpreter','none');
            if Args.ShowTitle
                if ~isempty(Args.Title)
                    TStr = Args.Title;
                else
                    TStr = sprintf('%s diff (%s - %s)   N=%d  med=%+.4g  std=%.4g', ...
                        Args.Param, Args.LabelB, Args.LabelA, ...
                        DiffStats.N, DiffStats.Median, DiffStats.Std);
                end
                title(TStr, 'Interpreter', 'none');
            end

        case 'hist'
            histogram(Diff, Args.Bins, ...
                'FaceColor', Args.Color, 'EdgeColor', 'k');
            grid on; box on;
            YL = ylim;
            plot([DiffStats.Median DiffStats.Median], YL, 'k-', 'LineWidth', 1.4);
            plot([0 0], YL, 'k--', 'LineWidth', 0.8);
            xlabel(sprintf('%s  [%s - %s]', Args.Param, Args.LabelB, Args.LabelA), ...
                'Interpreter','none');
            ylabel('Count');
            if Args.ShowTitle
                if ~isempty(Args.Title)
                    TStr = Args.Title;
                else
                    TStr = sprintf('%s diff distribution   N=%d  med=%+.4g  std=%.4g', ...
                        Args.Param, DiffStats.N, DiffStats.Median, DiffStats.Std);
                end
                title(TStr, 'Interpreter', 'none');
            end
    end
    hold off;

    % --- Output ---------------------------------------------------------
    Result.X         = Xv;
    Result.Y         = Yv;
    Result.Diff      = Diff;
    Result.RelDiff   = RelDiff;
    Result.RelMode   = RelMode;        % 'Range' or 'A'
    Result.Range     = Range;          % NaN when RelMode == 'A'
    Result.DiffStats = DiffStats;
    Result.Pearson   = Pearson;
    Result.CropIDs   = Cid;
    Result.Param     = Args.Param;
    Result.LabelA    = Args.LabelA;
    Result.LabelB    = Args.LabelB;
    Result.Mode      = Args.Mode;
end

% =========================================================================
function renderScatter(Xv, Yv, Cid, IsCentral, Args, DiffStats, Pearson, RelMode)
    if Args.CentralEdge
        plot(Xv(IsCentral),  Yv(IsCentral), 'o', ...
            'MarkerFaceColor', Args.Color, 'MarkerEdgeColor', Args.Color, ...
            'MarkerSize', Args.MarkerSize, 'DisplayName', 'central');
        plot(Xv(~IsCentral), Yv(~IsCentral), 'o', ...
            'MarkerFaceColor', 'none', 'MarkerEdgeColor', Args.Color, ...
            'LineWidth', 1.0, 'MarkerSize', Args.MarkerSize, ...
            'DisplayName', 'peripheral');
        legend('Location','best','Interpreter','none');
    else
        plot(Xv, Yv, 'o', ...
            'MarkerFaceColor', Args.Color, 'MarkerEdgeColor', Args.Color, ...
            'MarkerSize', Args.MarkerSize);
    end

    if Args.CropLabels
        for K = 1:numel(Xv)
            text(Xv(K), Yv(K), sprintf(' %d', Cid(K)), ...
                'FontSize', 8, 'Interpreter', 'none');
        end
    end

    if Args.LogX; set(gca, 'XScale', 'log'); end
    if Args.LogY; set(gca, 'YScale', 'log'); end

    if Args.ShowOneToOne
        XL = xlim; YL = ylim;
        Lo = min(XL(1), YL(1)); Hi = max(XL(2), YL(2));
        plot([Lo Hi], [Lo Hi], 'k--', 'HandleVisibility', 'off');
        xlim([Lo Hi]); ylim([Lo Hi]);
    end
    grid on; box on;
    xlabel(sprintf('%s  [%s]', Args.Param, Args.LabelA), 'Interpreter', 'none');
    ylabel(sprintf('%s  [%s]', Args.Param, Args.LabelB), 'Interpreter', 'none');

    if Args.ShowTitle
        if strcmp(RelMode, 'Range')
            DenLab = 'Range';
        else
            DenLab = 'A';
        end
        if ~isempty(Args.Title)
            TStr = Args.Title;
        elseif Args.CentralEdge && ~isnan(DiffStats.MedianRelCentral)
            TStr = sprintf(['%s: %s vs %s   N=%d   ', ...
                'med (B-A)/%s = %+.2g%% +- %.2g%%   R=%+.2f   ', ...
                '(cen %+.2g/%.2g, per %+.2g/%.2g)'], ...
                Args.Param, Args.LabelB, Args.LabelA, numel(Xv), DenLab, ...
                100*DiffStats.MedianRel, 100*DiffStats.StdRel, Pearson.R, ...
                100*DiffStats.MedianRelCentral, 100*DiffStats.StdRelCentral, ...
                100*DiffStats.MedianRelPeripheral, 100*DiffStats.StdRelPeripheral);
        else
            TStr = sprintf(['%s: %s vs %s   N=%d   ', ...
                'med (B-A)/%s = %+.2g%% +- %.2g%%   R=%+.2f'], ...
                Args.Param, Args.LabelB, Args.LabelA, numel(Xv), DenLab, ...
                100*DiffStats.MedianRel, 100*DiffStats.StdRel, Pearson.R);
        end
        title(TStr, 'Interpreter', 'none');
    end
end

% =========================================================================
function [Range, Mode] = paramRange(PCarray, ParamName, Verbose)
    % Look up the allowed-variation range (Max - Min) of a fitted
    % transmission parameter from any non-empty PCobj's getAllFunPar
    % registry, which is populated from
    % imUtil.calib.predefSeqCompositeFun bounds (Norm:0.001..1,
    % TauAod500:0.01..1, PWV_cm:0.1..10, Center_Ang:5000..6000, ...).
    % Returns Mode='Range' when found and finite, otherwise 'A' (caller
    % should fall back to dividing by A's value).
    Range = NaN;  Mode = 'A';
    for K = 1:numel(PCarray)
        if isempty(PCarray(K).TransModel); continue; end
        try
            P = PCarray(K).TransModel.getAllFunPar();
            Idx = find(strcmp(P.Name, ParamName), 1);
            if isempty(Idx); continue; end
            PMin = P.Min(Idx); PMax = P.Max(Idx);
            if isfinite(PMin) && isfinite(PMax) && PMax > PMin
                Range = PMax - PMin;
                Mode  = 'Range';
                return;
            end
        catch
        end
    end
    if Verbose
        fprintf(['  paramRange: %s has no finite Min/Max in TransModel; ', ...
            'falling back to (B-A)/A.\n'], ParamName);
    end
end
