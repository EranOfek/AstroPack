function Fig = plotMaskBitsComparison(Report, Args)
    % Scatter the aggregate per-bit pixel fraction, old pipeline vs new
    % Description: Plots one point per mask bit: x = aggregate fraction of
    %              pixels with that bit set in the OLD pipeline, y = the
    %              same for the NEW pipeline. A 1:1 dashed reference line is
    %              drawn; bits above it fire more often in the new pipeline,
    %              bits below it fire less.
    %
    %              The fraction used is OverallFraction (total pixels with
    %              the bit set divided by total pixels, summed over all
    %              files) -- a single aggregate number per bit, NOT a
    %              per-image average.
    %
    %              Consumes the report from
    %              pipeline.last.quality.masks.compareMaskBitsPipelines,
    %              whose Comparison table already holds OverallFrac_Old and
    %              OverallFrac_New per bit.
    % Input  : - Report: struct returned by
    %            pipeline.last.quality.masks.compareMaskBitsPipelines.
    %          * ...,key,val,...
    %            'BitNames'   - Cell array of bit names to plot. If empty,
    %                           every bit in Report.Comparison. Default {}.
    %            'LogScale'   - Log-log axes. Bit fractions span many orders
    %                           of magnitude, so log is usually wanted.
    %                           Default true.
    %            'DropZeroZero'- Drop bits whose fraction is 0 in BOTH
    %                           pipelines (no information). Default true.
    %            'LabelMode'  - How to identify bits on the figure:
    %                             'legend' - standard side legend (default)
    %                             'dots'   - write the bit name next to each
    %                                        marker, no legend
    %                             'none'   - neither
    %            'LabelFontSize' - Font size for in-plot labels when
    %                           LabelMode='dots'. Default 8.
    %            'MarkerSize' - Scatter marker area. Default 64.
    %            'Title'      - Optional title prefix. Default ''.
    %            'SaveFile'   - If non-empty, save the figure to this path
    %                           (extension sets the format). Default ''.
    %            'Visible'    - 'on'|'off'. Default 'on'.
    % Output : - Fig: handle of the created figure.
    % Author : Dana Kovaleva (May 2026)
    % Example: Rep = pipeline.last.quality.masks.compareMaskBitsPipelines( ...
    %                    OldPath, NewPath, 'FileType', 'sci_coadd');
    %          pipeline.last.quality.masks.plotMaskBitsComparison(Rep);

    arguments
        Report                struct
        Args.BitNames         cell          = {}
        Args.LogScale         (1,1) logical = true
        Args.DropZeroZero     (1,1) logical = true
        Args.LabelMode        (1,:) char {mustBeMember(Args.LabelMode, {'legend','dots','none'})} = 'legend'
        Args.LabelFontSize    (1,1) double  = 8
        Args.MarkerSize       (1,1) double  = 64
        Args.Title            (1,:) char    = ''
        Args.SaveFile         (1,:) char    = ''
        Args.Visible          (1,:) char    = 'on'
    end

    if ~isfield(Report, 'Comparison') || isempty(Report.Comparison)
        error(['Report has no Comparison table. Pass the struct returned ' ...
               'by pipeline.last.quality.masks.compareMaskBitsPipelines.']);
    end

    C = Report.Comparison;

    % --- Optional bit-name subset --------------------------------------
    if ~isempty(Args.BitNames)
        Req     = string(Args.BitNames(:));
        Keep    = ismember(string(C.BitName), Req);
        Missing = setdiff(Req, string(C.BitName));
        if ~isempty(Missing)
            warning('Bit name(s) not in Comparison: %s', ...
                    strjoin(cellstr(Missing), ', '));
        end
        C = C(Keep, :);
    end

    Bits = string(C.BitName);
    Old  = C.OverallFrac_Old;
    New  = C.OverallFrac_New;

    % --- Drop uninformative bits ---------------------------------------
    Valid = ~(isnan(Old) & isnan(New));
    if Args.DropZeroZero
        Valid = Valid & ~((Old == 0 | isnan(Old)) & (New == 0 | isnan(New)));
    end
    Bits = Bits(Valid);
    Old  = Old(Valid);
    New  = New(Valid);

    if isempty(Bits)
        error('No bits left to plot after filtering.');
    end

    % --- Log-scale floor: zeros cannot sit on a log axis ----------------
    % Replace exact 0 (a bit that never fires in one pipeline) with a
    % floor one decade below the smallest nonzero fraction, so the point
    % is still visible at the plot edge.
    PlotOld = Old;
    PlotNew = New;
    if Args.LogScale
        AllPos = [Old(Old > 0); New(New > 0)];
        if isempty(AllPos)
            error('All fractions are zero -- nothing to plot on a log axis.');
        end
        Floor = min(AllPos) / 10;
        PlotOld(PlotOld <= 0 | isnan(PlotOld)) = Floor;
        PlotNew(PlotNew <= 0 | isnan(PlotNew)) = Floor;
    end

    % --- Plot ----------------------------------------------------------
    % The 'lines' colormap cycles after Ncol colours. To keep bits visually
    % distinct when there are more bits than colours, the marker style is
    % varied per colour-cycle lap:
    %   lap 0 (bits 1..7)   - filled circle
    %   lap 1 (bits 8..14)  - open circle
    %   lap 2 (bits 15..21) - cross
    % giving Ncol*3 = 21 distinct styles; the cycle repeats beyond that.
    Fig  = figure('Visible', Args.Visible);
    hold on; grid on; box on;
    Ncol = 7;                         % distinct colours in the 'lines' cycle
    Cmap = lines(Ncol);
    for I = 1:numel(Bits)
        ColorIdx = mod(I-1, Ncol) + 1;
        Lap      = floor((I-1) / Ncol);
        Color    = Cmap(ColorIdx,:);
        switch mod(Lap, 3)
            case 0   % filled circle
                scatter(PlotOld(I), PlotNew(I), Args.MarkerSize, Color, ...
                        'o', 'filled', 'DisplayName', char(Bits(I)));
            case 1   % open circle
                scatter(PlotOld(I), PlotNew(I), Args.MarkerSize, Color, ...
                        'o', 'LineWidth', 1.2, 'DisplayName', char(Bits(I)));
            case 2   % cross
                scatter(PlotOld(I), PlotNew(I), Args.MarkerSize, Color, ...
                        'x', 'LineWidth', 1.2, 'DisplayName', char(Bits(I)));
        end
    end

    if Args.LogScale
        set(gca, 'XScale', 'log', 'YScale', 'log');
    end
    axis equal;

    % 1:1 reference line spanning the common data range
    Lims = [min([xlim, ylim]), max([xlim, ylim])];
    plot(Lims, Lims, 'k--', 'HandleVisibility', 'off');
    xlim(Lims); ylim(Lims);

    xlabel('Old pipeline — fraction of pixels with bit set');
    ylabel('New pipeline — fraction of pixels with bit set');

    TitleStr = sprintf('Aggregate mask-bit fraction: old vs new (%d bits)', ...
                       numel(Bits));
    if ~isempty(Args.Title)
        TitleStr = sprintf('%s | %s', Args.Title, TitleStr);
    end
    title(TitleStr, 'Interpreter', 'none');

    switch Args.LabelMode
        case 'legend'
            legend('Location', 'eastoutside', 'Interpreter', 'none');
        case 'dots'
            % Annotate each marker with the bit name. Offset is multiplicative
            % on log axes (so it scales with the value) and additive on linear.
            if Args.LogScale
                Offs = @(v) v * 1.08;
            else
                XR  = diff(xlim);
                Offs = @(v) v + 0.01 * XR;
            end
            for I = 1:numel(Bits)
                text(Offs(PlotOld(I)), PlotNew(I), char(Bits(I)), ...
                     'VerticalAlignment', 'middle', ...
                     'HorizontalAlignment', 'left', ...
                     'Interpreter', 'none', ...
                     'FontSize', Args.LabelFontSize);
            end
        case 'none'
            % no legend, no labels
    end

    if ~isempty(Args.SaveFile)
        exportgraphics(Fig, Args.SaveFile, 'Resolution', 150);
    end
end
