function Fig = plotResidualsEvolutionFromReport(Rep, Args)
    % Stage-evolution plot for quality metrics from a batchPhotCalibTrans Report.
    % Description: Walks the CalibTrajectory of every selected row, picks
    %              the post-stage snapshot (highest IterIndex per unique
    %              (StageIndex, OuterIter) pair) by default, and overlays
    %              one curve per requested quantity vs stage-progression
    %              index. When more than one row survives filtering, one
    %              curve family is drawn per row (colour codes row, marker
    %              style codes quantity), so cross-visit / cross-recipe
    %              comparisons come out of a single call.
    %
    %              Quantities read directly from snapshot fields:
    %                'ARMS'         - snapshot's ARMS (bright-end
    %                                 sliding-window RMS).
    %                'RMS'          - snapshot's UnweightedResiduals RMS.
    %                'RobustStd'    - 1.4826 * MAD of Residuals.
    %                'Scatter'      - std(Residuals) at that snap.
    %                'NRemaining'   - surviving calibrator count.
    %              Quantity computed on demand from the snapshot's SourceData:
    %                'MedianRMS'    - sqrt(median(R^2)) over Used-flag rows.
    %
    % Input  : - Rep - struct array (batchPhotCalibTrans) or its table view.
    %          * ...,key,val,...
    %            'Quantities'   - Cell array of quantity names to overlay.
    %                             Default {'ARMS','MedianRMS'}.
    %            'Which'        - 'post' (default): one point per unique
    %                             (StageIndex, OuterIter), taken at the
    %                             highest IterIndex (post-clip refit).
    %                             'pre' : IterIndex==0 snapshots (state on
    %                             entry to each stage). Stage-1 pre is the
    %                             true initial state; stage-N pre for N>1
    %                             is (up to intra-stage clipping) the same
    %                             as stage-(N-1) post.
    %                             'prepost' : pre AND post for each
    %                             (StageIndex, OuterIter), interleaved in
    %                             order. Shows the initial pre-fit state
    %                             before stage 1, then the fit's evolution
    %                             stage by stage. Best for "how much did
    %                             this stage improve residuals" reads.
    %                             'all' : every snapshot, in trajectory
    %                             order.
    %            'RowFilter'    - Logical vector length numel(Rep) OR a
    %                             function handle @(row) -> logical scalar.
    %                             Default: keep rows with non-empty
    %                             CalibTrajectory.
    %            'Aggregate'    - When multiple rows survive filtering,
    %                             collapse the row family into a single
    %                             summary line per quantity:
    %                               'none'   - one line per row (default;
    %                                          preserves earlier
    %                                          behaviour).
    %                               'median' - median across rows at each
    %                                          stage-progression index.
    %                               'mean'   - mean across rows.
    %                             Requires every included row to have the
    %                             same number of selected snapshots (same
    %                             OptSeqName / Recipe). Filter rows first
    %                             if that is not the case.
    %            'AggregateBand'- Shaded spread band on the aggregated line:
    %                               'none'
    %                               'quantile' - Q1..Q3 (default when
    %                                          Aggregate is 'median').
    %                               'std'      - median +/- 1.4826*MAD
    %                                          (aggregated line at median)
    %                                          or mean +/- std (mean).
    %                             Ignored when Aggregate='none'. Default
    %                             'quantile'.
    %            'LogY'         - Log y-axis. Default true — the plotted
    %                             quantities (ARMS/RMS/MedianRMS/RobustStd/
    %                             Scatter) are strictly positive and often
    %                             span an order of magnitude across stages,
    %                             so semilogy is the readable default.
    %                             Pass false for a linear axis.
    %            'Marker'       - Marker char shared by all curves. Default 'o'.
    %            'LineWidth'    - Line width. Default 1.4.
    %            'MarkerSize'   - Marker size. Default 6.
    %            'ShowLegend'   - Draw legend. Default true.
    %            'MaxLegendRows'- Cap legend size (single-row calls always
    %                             show one entry per quantity regardless).
    %                             Default 16.
    %            'Title'        - Figure title override. Default auto.
    % Output : - Fig - the created figure handle ([] when no data).
    % Author : D. Kovaleva (Jul 2026)
    % See also: plotResidualsFromReport, plotFitQuality (PhotCalibTrans method).
    % Example:
    %   plotResidualsEvolutionFromReport(Rep, ...
    %       'Quantities', {'ARMS','MedianRMS','RMS'});
    %
    %   mask = strcmp(T.RunMode,'joint') & strcmp(T.OptSeqName, ...
    %                 'LAST_Joint_1Iter_AtmosFirst_Split3');
    %   plotResidualsEvolutionFromReport(T, 'RowFilter', mask, ...
    %       'Quantities', {'ARMS'});
    %
    %   plotResidualsEvolutionFromReport(Rep, 'Which', 'all', 'LogY', true);

    arguments
        Rep
        Args.Quantities    cell           = {'ARMS','MedianRMS'}
        Args.Which         (1,:) char     {mustBeMember(Args.Which, {'post','pre','prepost','all'})} = 'post'
        Args.RowFilter                    = []
        Args.Aggregate     (1,:) char     {mustBeMember(Args.Aggregate, {'none','median','mean'})} = 'none'
        Args.AggregateBand (1,:) char     {mustBeMember(Args.AggregateBand, {'none','quantile','std'})} = 'quantile'
        Args.LogY          logical        = true
        Args.Marker        (1,:) char     = 'o'
        Args.LineWidth     (1,1) double   = 1.4
        Args.MarkerSize    (1,1) double   = 6
        Args.ShowLegend    logical        = true
        Args.MaxLegendRows (1,1) double   = 16
        Args.Title         (1,:) char     = ''
    end

    Fig = [];

    % ---- Normalise input to a struct array ------------------------------
    if istable(Rep)
        RepStruct = table2struct(Rep);
    elseif isstruct(Rep)
        RepStruct = Rep;
    else
        error('plotResidualsEvolutionFromReport:BadRep', ...
              'Rep must be a struct array or a table view of one.');
    end
    Nrow = numel(RepStruct);
    if Nrow == 0
        warning('plotResidualsEvolutionFromReport:Empty', 'Rep is empty.');
        return;
    end

    % ---- Row filter ------------------------------------------------------
    if isempty(Args.RowFilter)
        Keep = true(Nrow, 1);
    elseif islogical(Args.RowFilter) || isnumeric(Args.RowFilter)
        Keep = logical(Args.RowFilter);
        if numel(Keep) ~= Nrow
            error('plotResidualsEvolutionFromReport:BadFilter', ...
                  'RowFilter vector length %d ~= numel(Rep) %d.', numel(Keep), Nrow);
        end
    elseif isa(Args.RowFilter, 'function_handle')
        Keep = arrayfun(@(K) logical(Args.RowFilter(RepStruct(K))), 1:Nrow);
        Keep = Keep(:);
    else
        error('plotResidualsEvolutionFromReport:BadFilterType', ...
              'RowFilter must be a logical vector or a function handle.');
    end

    ValidQuantities = {'ARMS','MedianRMS','RMS','RobustStd','Scatter','NRemaining'};
    for Iq = 1:numel(Args.Quantities)
        Q = char(Args.Quantities{Iq});
        if ~ismember(Q, ValidQuantities)
            error('plotResidualsEvolutionFromReport:BadQuantity', ...
                'Unknown Quantity "%s". Allowed: %s.', Q, strjoin(ValidQuantities, ', '));
        end
    end

    % ---- Walk rows, extract per-row curves ------------------------------
    RowIdx  = find(Keep);
    Curves  = repmat(struct('Row',[],'X',[],'Y',[],'Q','','StageNames',{{}}, ...
        'Label','', 'IsAggregate',false, 'Lo',[], 'Hi',[]), 0, 1);
    for J = 1:numel(RowIdx)
        K   = RowIdx(J);
        Row = RepStruct(K);
        Traj = i_getTrajectory(Row);
        if isempty(Traj); continue; end

        SelIdx = i_selectSnapshots(Traj, Args.Which);
        if isempty(SelIdx); continue; end
        Sub    = Traj(SelIdx);
        StageNames = arrayfun(@(S) i_snapLabel(S), Sub, 'UniformOutput', false);

        for Iq = 1:numel(Args.Quantities)
            Q     = char(Args.Quantities{Iq});
            Yvec  = arrayfun(@(S) i_quantityValue(S, Q), Sub);
            Entry = struct('Row', K, 'X', 1:numel(Sub), 'Y', Yvec(:).', ...
                'Q', Q, 'StageNames', {StageNames}, ...
                'Label', i_rowLabel(Row), ...
                'IsAggregate', false, 'Lo', [], 'Hi', []);
            Curves(end+1) = Entry; %#ok<AGROW>
        end
    end

    if isempty(Curves)
        warning('plotResidualsEvolutionFromReport:NoData', ...
                'No row survived filtering / had a non-empty CalibTrajectory.');
        return;
    end

    % ---- Aggregate across rows if requested -----------------------------
    % Collapse the per-row curve family into one summary curve per quantity.
    % Requires every row's curve for that quantity to have identical X
    % length — enforced with a specific error so the caller filters rows
    % to a single Recipe before aggregating.
    NAggRowsPerQ = struct();
    if ~strcmpi(Args.Aggregate, 'none')
        [Curves, NAggRowsPerQ] = i_aggregateCurves(Curves, Args.Aggregate, Args.AggregateBand);
    end

    % ---- Style plan -----------------------------------------------------
    NrowSel  = numel(unique([Curves.Row]));
    NquantSel = numel(unique({Curves.Q}));
    IsSingleRow = NrowSel == 1;

    UniqRows = unique([Curves.Row], 'stable');
    UniqQ    = unique({Curves.Q}, 'stable');
    RowColors = i_colorCycle(numel(UniqRows));
    QMarkers  = i_markerCycle(numel(UniqQ), Args.Marker);
    QLines    = i_lineStyleCycle(numel(UniqQ));

    % ---- Plot ------------------------------------------------------------
    Fig = figure('Color', [1 1 1], 'Position', [80 80 720 480]);
    ax = axes(Fig); hold(ax, 'on'); grid(ax, 'on'); box(ax, 'on');

    for Ic = 1:numel(Curves)
        C = Curves(Ic);
        Cr = find(UniqRows == C.Row, 1);
        Cq = find(strcmp(UniqQ, C.Q), 1);
        if IsSingleRow
            % Single row: colour by quantity so the multi-metric overlay
            % reads cleanly.
            QColors  = i_colorCycle(numel(UniqQ));
            Color    = QColors(Cq, :);
            if isfield(C, 'IsAggregate') && C.IsAggregate
                NRowsThis = NAggRowsPerQ.(matlab.lang.makeValidName(C.Q));
                DispName = sprintf('%s %s (N=%d)', C.Q, Args.Aggregate, NRowsThis);
            else
                DispName = C.Q;
            end
        else
            Color    = RowColors(Cr, :);
            DispName = sprintf('%s | %s', C.Label, C.Q);
        end

        % Shaded band when this is an aggregated curve with Lo/Hi vectors.
        if isfield(C, 'IsAggregate') && C.IsAggregate ...
                && ~strcmpi(Args.AggregateBand, 'none') ...
                && isfield(C, 'Lo') && ~isempty(C.Lo)
            patch(ax, [C.X, fliplr(C.X)], [C.Hi, fliplr(C.Lo)], Color, ...
                'FaceAlpha', 0.18, 'EdgeColor', 'none', ...
                'HandleVisibility', 'off');
        end

        plot(ax, C.X, C.Y, ...
            'Color',      Color, ...
            'Marker',     QMarkers(Cq), ...
            'LineStyle',  QLines{Cq}, ...
            'LineWidth',  Args.LineWidth, ...
            'MarkerSize', Args.MarkerSize, ...
            'MarkerFaceColor', Color, ...
            'DisplayName', DispName);
    end

    % X-axis ticks
    Xmax = max(arrayfun(@(C) max(C.X), Curves));
    xticks(ax, 1:Xmax);
    if IsSingleRow
        xticklabels(ax, Curves(1).StageNames);
        xtickangle(ax, 45);
    end

    switch Args.Which
        case 'all';     xlabel(ax, 'Snapshot index');
        case 'prepost'; xlabel(ax, 'Stage progression (pre \rightarrow post per stage)');
        otherwise;      xlabel(ax, 'Stage progression');
    end
    ylabel(ax, i_yLabel(Args.Quantities));
    if Args.LogY
        set(ax, 'YScale', 'log');
    end

    if isempty(Args.Title)
        IsAgg = ~strcmpi(Args.Aggregate, 'none') && any([Curves.IsAggregate]);
        if IsAgg
            % Aggregated view. Row identity is meaningless — describe the
            % aggregation instead. Use the per-quantity N from the map.
            NsPerQ = struct2array(NAggRowsPerQ);
            NsUniq = unique(NsPerQ);
            if isscalar(NsUniq); NStr = sprintf('N=%d', NsUniq);
            else;                NStr = sprintf('N=%d..%d', min(NsUniq), max(NsUniq));
            end
            title(ax, sprintf('Residuals evolution — %s across rows (%s, Which=%s)', ...
                Args.Aggregate, NStr, Args.Which), 'Interpreter', 'none');
        elseif IsSingleRow
            RowRef = RepStruct(UniqRows);
            title(ax, sprintf('%s | %s | %s   (Which=%s)', ...
                i_short(RowRef.VisitStem), RowRef.RunMode, RowRef.OptSeqName, ...
                Args.Which), 'Interpreter', 'none');
        else
            title(ax, sprintf('Residuals evolution — %d rows, %d quantit%s (Which=%s)', ...
                NrowSel, NquantSel, i_plural(NquantSel), Args.Which), ...
                'Interpreter', 'none');
        end
    else
        title(ax, Args.Title, 'Interpreter', 'none');
    end

    if Args.ShowLegend && numel(Curves) <= Args.MaxLegendRows
        legend(ax, 'Location', 'best', 'Interpreter', 'none', 'Box', 'off');
    end
    hold(ax, 'off');
end


% ==== Local helpers ====================================================

function [Out, NAggRowsPerQ] = i_aggregateCurves(Curves, How, BandKind)
    % Collapse a per-row curve family (one struct per row per quantity)
    % into one summary struct per quantity: median/mean central tendency
    % + a Q1..Q3 / MAD / std spread band.
    Out = repmat(Curves(1), 0, 1);          % preserve schema
    NAggRowsPerQ = struct();
    UniqQ = unique({Curves.Q}, 'stable');
    for Iq = 1:numel(UniqQ)
        Q = UniqQ{Iq};
        Sel = Curves(strcmp({Curves.Q}, Q));
        if isempty(Sel); continue; end

        % Enforce identical X across rows in this quantity group.
        Xref = Sel(1).X;
        for K = 2:numel(Sel)
            if ~isequal(size(Sel(K).X), size(Xref)) || any(Sel(K).X ~= Xref)
                error('plotResidualsEvolutionFromReport:AggregateShape', ...
                    ['Cannot aggregate: row curves for quantity ''%s'' ' ...
                     'have different snapshot counts. Filter rows to a ' ...
                     'single Recipe (same OptSeqName / same Which) first.'], Q);
            end
        end

        % Stack Ys row-by-row, then reduce down columns.
        Ymat = cat(1, Sel.Y);                             % [Nrows x Npts]
        switch lower(How)
            case 'median'
                Ycentral = median(Ymat, 1, 'omitnan');
            case 'mean'
                Ycentral = mean(Ymat, 1, 'omitnan');
        end

        Lo = []; Hi = [];
        switch lower(BandKind)
            case 'quantile'
                Q13 = quantile(Ymat, [0.25, 0.75], 1);   % [2 x Npts]
                Lo = Q13(1, :); Hi = Q13(2, :);
            case 'std'
                if strcmpi(How, 'median')
                    Sm = 1.4826 * median(abs(Ymat - Ycentral), 1, 'omitnan');
                    Lo = Ycentral - Sm; Hi = Ycentral + Sm;
                else                                     % mean +/- std
                    Sm = std(Ymat, 0, 1, 'omitnan');
                    Lo = Ycentral - Sm; Hi = Ycentral + Sm;
                end
        end

        % Emit one aggregated curve — Row is unused for aggregates (all
        % share Row=0) so downstream style logic falls through cleanly.
        Ent = Sel(1);                                    % start from schema
        Ent.Row         = 0;
        Ent.X           = Xref;
        Ent.Y           = Ycentral;
        Ent.Label       = sprintf('%s (N=%d)', How, numel(Sel));
        Ent.IsAggregate = true;
        Ent.Lo          = Lo;
        Ent.Hi          = Hi;
        Out(end+1) = Ent;                                %#ok<AGROW>

        NAggRowsPerQ.(matlab.lang.makeValidName(Q)) = numel(Sel);
    end
end


function Traj = i_getTrajectory(Row)
    Traj = [];
    if ~isfield(Row, 'CalibTrajectory'); return; end
    T = Row.CalibTrajectory;
    if iscell(T)
        if isempty(T); return; end
        T = T{1};
    end
    if isempty(T) || ~isstruct(T); return; end
    Traj = T;
end


function Idx = i_selectSnapshots(Traj, Which)
    % Return indices into Traj for the requested snapshot set.
    N = numel(Traj);
    if N == 0; Idx = []; return; end

    switch lower(Which)
        case 'all'
            Idx = 1:N;
        case 'pre'
            II  = [Traj.IterIndex];
            Idx = find(II == 0);
        case 'post'
            % One snapshot per unique (StageIndex, OuterIter), the entry
            % with the highest IterIndex.
            SI = [Traj.StageIndex];
            OI = [Traj.OuterIter];
            II = [Traj.IterIndex];
            Key = OI(:)*1e4 + SI(:);
            [~, ord]   = sort(-II);       % descending by IterIndex
            [~, first] = unique(Key(ord), 'stable');
            Idx = sort(ord(first));
            Idx = Idx(:).';
        case 'prepost'
            % Two snapshots per unique (StageIndex, OuterIter): the
            % lowest-IterIndex one (pre-fit) and the highest (post-fit).
            % Collapsed to one entry when a group has only a single snap.
            % Emitted in the natural stage-progression order.
            SI = [Traj.StageIndex];
            OI = [Traj.OuterIter];
            II = [Traj.IterIndex];
            Key = OI(:)*1e4 + SI(:);
            [UKey, ~, GroupId] = unique(Key, 'stable');
            Buf = zeros(1, 2*numel(UKey));
            Nb = 0;
            for K = 1:numel(UKey)
                Members = find(GroupId == K);
                [~, MinI] = min(II(Members));
                [~, MaxI] = max(II(Members));
                PreIdx  = Members(MinI);
                PostIdx = Members(MaxI);
                Nb = Nb + 1; Buf(Nb) = PreIdx;
                if PostIdx ~= PreIdx
                    Nb = Nb + 1; Buf(Nb) = PostIdx;
                end
            end
            Idx = Buf(1:Nb);
        otherwise
            error('plotResidualsEvolutionFromReport:BadWhich', ...
                'Unknown Which "%s".', Which);
    end
end


function V = i_quantityValue(Snap, Q)
    switch Q
        case {'ARMS','RMS','RobustStd','Scatter','NRemaining'}
            if isfield(Snap, Q) && isscalar(Snap.(Q))
                V = double(Snap.(Q));
            else
                V = NaN;
            end
        case 'MedianRMS'
            V = NaN;
            if isempty(Snap.SourceData); return; end
            Tab = Snap.SourceData.Table;
            if isempty(Tab) || ~ismember('Residuals', Tab.Properties.VariableNames)
                return;
            end
            if ismember('Used', Tab.Properties.VariableNames)
                Mask = logical(Tab.Used);
            else
                Mask = true(height(Tab), 1);
            end
            R = double(Tab.Residuals(Mask));
            R = R(isfinite(R));
            if isempty(R); return; end
            V = sqrt(median(R.^2));
        otherwise
            V = NaN;
    end
end


function L = i_snapLabel(S)
    Name = '';
    if isfield(S, 'StageName') && ~isempty(S.StageName); Name = char(S.StageName); end
    L = sprintf('%s (S%d.O%d.I%d)', Name, S.StageIndex, S.OuterIter, S.IterIndex);
end


function L = i_rowLabel(Row)
    Stem = i_getStr(Row, 'VisitStem', '?');
    Mode = i_getStr(Row, 'RunMode',    '?');
    Opt  = i_getStr(Row, 'OptSeqName', '?');
    L = sprintf('%s | %s | %s', i_short(Stem), Mode, Opt);
end


function S = i_getStr(Row, F, Default)
    S = Default;
    if isfield(Row, F)
        V = Row.(F);
        if iscell(V) && ~isempty(V); V = V{1}; end
        if ischar(V) || isstring(V); S = char(V); end
    end
end


function S = i_short(S)
    % Strip the long LAST timestamp / counter part when the stem looks like
    % '...LAST.XX.YY.ZZ_YYYYMMDD.HHMMSS.mmm_clear_<field>_...'.
    Tok = regexp(S, '_(clear|blue|red)_([^_]+)_', 'tokens', 'once');
    if ~isempty(Tok) && numel(Tok) == 2
        S = sprintf('%s.%s', Tok{2}, Tok{1});
    end
end


function P = i_plural(N)
    if N == 1; P = 'y'; else; P = 'ies'; end
end


function Y = i_yLabel(Qs)
    if numel(Qs) == 1
        Y = char(Qs{1});
    else
        Y = strjoin(cellfun(@char, Qs, 'UniformOutput', false), ' / ');
    end
end


function C = i_colorCycle(N)
    Base = lines(max(N, 7));
    C = Base(mod(0:N-1, size(Base,1)) + 1, :);
end


function M = i_markerCycle(N, DefaultMarker)
    Pool = {DefaultMarker, 's', 'd', '^', 'v', 'x', '+', 'p', 'h'};
    Pool = unique(Pool, 'stable');
    Idx  = mod(0:N-1, numel(Pool)) + 1;
    M    = char(cellfun(@(c) c, Pool(Idx)));
end


function LS = i_lineStyleCycle(N)
    Pool = {'-', '--', ':', '-.'};
    Idx  = mod(0:N-1, numel(Pool)) + 1;
    LS   = Pool(Idx);
end
