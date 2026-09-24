function H = overlayHist(Ax, Series, Args)
    % Draw one or more overlaid histograms on a given axes.
    % Description: Pools the supplied value series to derive a common edge
    %              grid (unless 'Edges' is given), then draws each series as
    %              a histogram on Ax. A single series is drawn opaque; two or
    %              more are drawn semi-transparent for overlay comparison.
    %              Shared multi-series histogram primitive.
    % Input  : - Ax     - target axes handle.
    %          - Series - a numeric vector, or a cell of numeric vectors
    %                     (one histogram per cell).
    %          * ...,key,val,...
    %            'Edges'         - Explicit bin edges. Default [] (derived
    %                              via binEdges from the pooled data).
    %            'Bins'          - Bin count when edges are derived.
    %                              Default 30.
    %            'XLim'          - [lo hi] limits for derived edges.
    %                              Default [].
    %            'LogX'          - Logarithmic x axis / edges. Default false.
    %            'Labels'        - Cell of per-series legend labels.
    %                              Default {}.
    %            'Colors'        - Nx3 RGB rows; default lines() colormap.
    %            'Normalization' - histogram Normalization. Default 'count'.
    % Output : - H - column of histogram handles (one per series; invalid
    %            handle for an empty series).
    % Author : photCalib package refactor (2026-05)
    % Example: H = overlayHist(gca, {V1,V2}, 'Labels', {'A','B'}, 'LogX', true);

    arguments
        Ax
        Series
        Args.Edges                            = []
        Args.Bins                             = 30
        Args.XLim                             = []
        Args.LogX          logical            = false
        Args.Labels        cell               = {}
        Args.Colors                           = []
        Args.Normalization {mustBeTextScalar} = 'count'
    end

    if ~iscell(Series)
        Series = {Series};
    end
    N = numel(Series);

    % Common edge grid
    Edges = Args.Edges;
    if isempty(Edges)
        AllV = [];
        for I = 1:N
            AllV = [AllV; Series{I}(:)]; %#ok<AGROW>
        end
        Edges = binEdges(AllV, 'Bins', Args.Bins, 'XLim', Args.XLim, ...
                         'LogX', Args.LogX);
    end

    if isempty(Args.Colors)
        Colors = lines(max(N, 1));
    else
        Colors = Args.Colors;
    end

    hold(Ax, 'on');
    H = gobjects(N, 1);
    for I = 1:N
        V = Series{I}(:);
        V = V(isfinite(V));
        if Args.LogX
            V = V(V > 0);
        end
        if isempty(V)
            continue;
        end

        if N == 1
            Face = Colors(1,:); Alpha = 1.0; Edge = 'k';
        else
            Face  = Colors(mod(I-1, size(Colors,1)) + 1, :);
            Alpha = 0.45;
            Edge  = Face * 0.6;
        end

        if I <= numel(Args.Labels) && ~isempty(Args.Labels{I})
            DName = sprintf('%s (N=%d)', char(Args.Labels{I}), numel(V));
        else
            DName = sprintf('series %d (N=%d)', I, numel(V));
        end

        H(I) = histogram(Ax, V, Edges, 'FaceColor', Face, ...
            'FaceAlpha', Alpha, 'EdgeColor', Edge, ...
            'Normalization', Args.Normalization, 'DisplayName', DName);
    end

    if Args.LogX
        set(Ax, 'XScale', 'log');
    end
    box(Ax, 'on');
    grid(Ax, 'on');
end
