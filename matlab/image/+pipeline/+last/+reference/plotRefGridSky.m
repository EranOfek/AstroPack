function H = plotRefGridSky(RefGrid, RowInd, Args)
    % Plot LAST reference-grid rectangles on a sky projection.
    %     For each selected row of a RefGrid table (as produced by
    %     pipeline.last.reference.buildRefGrid), draws the spherical
    %     rectangle defined by its four corners (RA1..RA4, Dec1..Dec4)
    %     with great-circle edges, and prints the row index at the
    %     image centre (RA, Dec).
    % Input  : - RefGrid: table with columns RA, Dec, RA1..RA4, Dec1..Dec4
    %            (all in degrees), as returned by buildRefGrid.
    %          - RowInd : numeric row indices, a logical mask of length
    %            height(RefGrid), or [] meaning "all rows".
    %            Default is [].
    %          * ...,key,val,...
    %            'Projection' - axesm projection name accepted by the
    %               Mapping Toolbox (e.g. 'aitoff','mollweid','hammer',
    %               'robinson','eqdcylin','sinusoid'), or 'flat' for a
    %               plain RA/Dec Cartesian plot. Default 'aitoff'.
    %            'Axes'       - target axes handle to draw into. If empty
    %               a new figure and axes are created. For map projections
    %               the supplied axes must already be a map axes (created
    %               with axesm) using the same projection.
    %               Default is [].
    %            'NPerEdge'   - number of points (incl. endpoints) used to
    %               densify each great-circle edge. Default 30.
    %            'EdgeColor'  - rectangle outline colour. Default 'b'.
    %            'LineWidth'  - rectangle outline width. Default 1.
    %            'LineStyle'  - rectangle outline style. Default '-'.
    %            'TextColor'  - row-label colour. Default 'k'.
    %            'FontSize'   - row-label font size. Default 8.
    %            'Frame'      - 'on'/'off' frame on map axes. Default 'on'.
    %            'Grid'       - 'on'/'off' grid on map axes. Default 'on'.
    %            'MLabel'     - 'on'/'off' meridian labels. Default 'off'.
    %            'PLabel'     - 'on'/'off' parallel labels. Default 'off'.
    % Output : - Struct with fields:
    %             .Axes  - axes handle used for drawing.
    %             .Line  - handle of the (single) line object holding all
    %                      rectangle outlines (separated internally by NaN).
    %             .Texts - vector of text handles, one per drawn row.
    % Author : A.M. Krassilchtchikov + ClaudeCode (Apr 2026)
    % Example: G = load('LAST_refGrid.mat');
    %          % all rows that survived the use-mask, on an Aitoff map:
    %          pipeline.last.reference.plotRefGridSky(G.LAST_RefIm_Grid, LAST_RefIm_Grid.use, 'Projection','aitoff');
    %          % a few specific rows on a flat plot:
    %          pipeline.last.reference.plotRefGridSky(G.LAST_RefIm_Grid, [10 25 137], 'Projection','flat');
    arguments
        RefGrid           table
        RowInd                          = [];
        Args.Projection                 = 'aitoff';
        Args.Axes                       = [];
        Args.NPerEdge   (1,1) double    = 30;
        Args.EdgeColor                  = 'b';
        Args.LineWidth  (1,1) double    = 1;
        Args.LineStyle                  = '-';
        Args.TextColor                  = 'k';
        Args.FontSize   (1,1) double    = 8;
        Args.Frame                      = 'on';
        Args.Grid                       = 'on';
        Args.MLabel                     = 'off';
        Args.PLabel                     = 'off';
    end

    % --- check required columns ---------------------------------------
    Required = {'RA','Dec','RA1','Dec1','RA2','Dec2','RA3','Dec3','RA4','Dec4'};
    Missing  = setdiff(Required, RefGrid.Properties.VariableNames);
    if ~isempty(Missing)
        error('plotRefGridSky:missingColumns', ...
              'RefGrid is missing required column(s): %s', strjoin(Missing,', '));
    end

    % --- resolve row selection ----------------------------------------
    if isempty(RowInd)
        Idx = (1:height(RefGrid)).';
    elseif islogical(RowInd)
        if numel(RowInd) ~= height(RefGrid)
            error('plotRefGridSky:badMask', ...
                  'Logical RowInd must have length height(RefGrid)=%d.', height(RefGrid));
        end
        Idx = find(RowInd(:));
    else
        Idx = RowInd(:);
    end
    NRows = numel(Idx);

    IsFlat = strcmpi(Args.Projection, 'flat');

    % --- prepare axes -------------------------------------------------
    if isempty(Args.Axes)
        figure('Color',[1 1 1]);
        Ax = axes();
        if IsFlat
            hold(Ax,'on'); box(Ax,'on');
            xlabel(Ax,'RA [deg]'); ylabel(Ax,'Dec [deg]');
        else
            axesm('MapProjection', Args.Projection, ...
                  'Frame',  Args.Frame, ...
                  'Grid',   Args.Grid,  ...
                  'MeridianLabel', Args.MLabel, ...
                  'ParallelLabel', Args.PLabel);
            hold(Ax,'on');
        end
    else
        Ax = Args.Axes;
        axes(Ax);              %#ok<LAXES>  needed: plotm/textm act on gca
        hold(Ax,'on');
    end

    % --- build one big polyline with NaN breaks between rows ----------
    % preallocate generously: each row contributes (4*(N-1)+1) points
    % plus a separating NaN, plus extra slots for wrap-induced NaNs.
    Pts0    = 4*(Args.NPerEdge - 1) + 1;
    Buf     = NaN(1, NRows*(Pts0 + 8));
    PolyLon = Buf;
    PolyLat = Buf;
    Pos     = 0;

    for jj = 1:NRows
        I = Idx(jj);
        CornerLon = [RefGrid.RA1(I)  RefGrid.RA2(I)  RefGrid.RA3(I)  RefGrid.RA4(I)  RefGrid.RA1(I)];
        CornerLat = [RefGrid.Dec1(I) RefGrid.Dec2(I) RefGrid.Dec3(I) RefGrid.Dec4(I) RefGrid.Dec1(I)];

        [RowLon, RowLat] = densifyPolygon(CornerLon, CornerLat, Args.NPerEdge);
        [RowLon, RowLat] = breakRAwrap(RowLon, RowLat);

        N = numel(RowLon);
        if Pos + N + 1 > numel(PolyLon)
            % grow if estimate was too small
            PolyLon(end + N + 1) = NaN; %#ok<AGROW>
            PolyLat(end + N + 1) = NaN; %#ok<AGROW>
        end
        PolyLon(Pos+1 : Pos+N) = RowLon;
        PolyLat(Pos+1 : Pos+N) = RowLat;
        Pos = Pos + N + 1;     % leave a NaN separator
    end
    PolyLon = PolyLon(1:Pos-1);
    PolyLat = PolyLat(1:Pos-1);

    % --- draw outlines (single line handle) ---------------------------
    if IsFlat
        LineH = plot(Ax, PolyLon, PolyLat, ...
                     'LineStyle', Args.LineStyle, ...
                     'Color',     Args.EdgeColor, ...
                     'LineWidth', Args.LineWidth);
    else
        LineH = plotm(PolyLat, PolyLon, ...
                      'LineStyle', Args.LineStyle, ...
                      'Color',     Args.EdgeColor, ...
                      'LineWidth', Args.LineWidth);
    end

    % --- row-index labels at image centres ----------------------------
    TextH = gobjects(NRows,1);
    for jj = 1:NRows
        I = Idx(jj);
        Lbl = num2str(I);
        if IsFlat
            TextH(jj) = text(Ax, RefGrid.RA(I), RefGrid.Dec(I), Lbl, ...
                'Color', Args.TextColor, 'FontSize', Args.FontSize, ...
                'HorizontalAlignment','center', 'VerticalAlignment','middle');
        else
            TextH(jj) = textm(RefGrid.Dec(I), RefGrid.RA(I), Lbl, ...
                'Color', Args.TextColor, 'FontSize', Args.FontSize, ...
                'HorizontalAlignment','center', 'VerticalAlignment','middle');
        end
    end

    H.Axes  = Ax;
    H.Line  = LineH;
    H.Texts = TextH;
end


function [Lon, Lat] = densifyPolygon(CornerLon, CornerLat, N)
    % Densify a closed polygon (CornerLon(end)==CornerLon(1)) by sampling
    % N points (including endpoints) along each great-circle edge.
    NEdge = numel(CornerLon) - 1;
    Lon   = zeros(1, NEdge*(N-1) + 1);
    Lat   = zeros(1, NEdge*(N-1) + 1);
    P     = 0;
    for K = 1:NEdge
        [La, Lo] = greatCircleArc(CornerLat(K), CornerLon(K), ...
                                  CornerLat(K+1), CornerLon(K+1), N);
        if K == 1
            Lon(1:N) = Lo(:).';
            Lat(1:N) = La(:).';
            P = N;
        else
            Lon(P+1 : P+N-1) = Lo(2:end).';
            Lat(P+1 : P+N-1) = La(2:end).';
            P = P + N - 1;
        end
    end
end


function [Lat, Lon] = greatCircleArc(Lat1, Lon1, Lat2, Lon2, N)
    % N evenly-spaced points (incl. endpoints) along the great circle
    % from (Lat1,Lon1) to (Lat2,Lon2). Inputs/outputs in degrees.
    [ArcLen, Az] = distance('gc', Lat1, Lon1, Lat2, Lon2, 'degrees');
    F = linspace(0, 1, N).';
    [Lat, Lon] = reckon(Lat1, Lon1, ArcLen*F, Az*ones(N,1), 'degrees');
end


function [LonOut, LatOut] = breakRAwrap(Lon, Lat)
    % Insert NaN between any two consecutive points whose RA differ by
    % more than 180 deg, so the polyline does not draw a long stripe
    % across the 0/360 (or -180/180) seam of the projection.
    Lon = Lon(:).';
    Lat = Lat(:).';
    Brk = find(abs(diff(Lon)) > 180);
    if isempty(Brk)
        LonOut = Lon;  LatOut = Lat;
        return;
    end
    LonOut = Lon;  LatOut = Lat;
    for K = numel(Brk):-1:1
        B = Brk(K);
        LonOut = [LonOut(1:B), NaN, LonOut(B+1:end)];
        LatOut = [LatOut(1:B), NaN, LatOut(B+1:end)];
    end
end
