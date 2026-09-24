function Fig = plotResidualsFromReport(Rep, Args)
    % Residuals-vs-X plot working directly on batchPhotCalibTrans Report rows.
    % Description: Sister of plotPhotResiduals for the pipeline.last.quality.
    %              photCalib.batchPhotCalibTrans output. Pools every surviving
    %              calibrator's residual from every row's CalibTrajectory
    %              snapshot (the final one by default), then plots the pool
    %              against the requested XAxis quantity. Optional binned
    %              trend / RMS overlay, per-row filtering, and normalisation
    %              by MagErr — same conventions as plotPhotResiduals.
    %
    %              Two input shapes accepted:
    %                (a) Rep - the raw struct array returned by
    %                          batchPhotCalibTrans.
    %                (b) T   - the table view: T = struct2table(Rep, ...
    %                          'AsArray', true). Both are handled
    %                          identically after an internal normalisation.
    %
    % XAxis resolution order:
    %   'mag'          - MAG_AB column from SourceData.Table (falls back to
    %                    MAG_APER_3 if absent, then MAG_BP).
    %   'instrumental' - -2.5*log10(Flux) from SourceData.Table.
    %   'airmass'      - broadcast row scalar Rep.AIRMASS across the row's
    %                    calibrators.
    %   'background'   - 1./Flux from SourceData.Table.
    %   <SourceData col name> - that per-calibrator column
    %                    (e.g. 'BP_RP', 'MAG_BP', 'X', 'Y', 'RA', 'Dec',
    %                    'MatchDistance', 'MagErr').
    %   <Report col name>     - broadcast that per-row scalar
    %                    (e.g. 'FWHM', 'PWV_cm', 'TauAOD500',
    %                    'Center_Ang', 'Norm', 'NCalib', 'RMS').
    %   'A-B'          - difference of two SourceData columns.
    %
    % Input  : - Rep - struct array (batchPhotCalibTrans) or table view.
    %          * ...,key,val,...
    %            'XAxis'          - X quantity, see resolution order above.
    %                               Default 'mag'.
    %            'YStat'          - 'residual' (scatter + trend) or 'rms'
    %                               (binned residual RMS on log y).
    %                               Default 'residual'.
    %            'SnapshotIndex'  - Which entry of each row's CalibTrajectory
    %                               to read. Integer, 'first', or 'last'.
    %                               Default 'last' (post-fit).
    %            'UseOnly'        - 'used' (Used=true rows only, default) or
    %                               'all'.
    %            'RowFilter'      - Logical vector length numel(Rep) OR a
    %                               function handle @(row) -> logical scalar.
    %                               Default: keep every row that has a
    %                               non-empty CalibTrajectory.
    %            'Normalize'      - Plot Residual / MagErr. Default false.
    %            'OverlayTrend'   - 'median' | 'mean' | 'none' binned trend
    %                               (residual mode). Default 'median'.
    %            'TrendBinWidth'  - Explicit bin width in X units. Default []
    %                               (= X range / TrendBins).
    %            'TrendBins'      - Bin count when TrendBinWidth is empty.
    %                               Default 30.
    %            'MinCount'       - Skip trend/RMS bins below this count.
    %                               Default 5.
    %            'FitLine'        - Overlay a linear fit and report slope
    %                               (residual mode). Default false.
    %            'ShowMedianShift'- Overlay the pooled median residual.
    %                               Default false.
    %            'YLim'           - Y-axis limits. Default [] (auto).
    %            'MarkerSize'     - Scatter marker size. Default 3.
    % Output : - Fig - the created figure handle. [] when no data.
    % Author : D. Kovaleva (Jul 2026)
    % See also: plotPhotResiduals (PC-array input); plotCalParams
    %           (single-snap scatter with binned-median overlay).
    % Example:
    %   plotResidualsFromReport(Rep, 'XAxis', 'BP_RP');
    %   plotResidualsFromReport(T,   'XAxis', 'airmass', 'YStat', 'rms');
    %   maska = strcmp(T.OptSeqName, 'LAST_Joint_1Iter_AtmosFirst_Split3') & ...
    %           strcmp(T.RunMode,    'joint');
    %   plotResidualsFromReport(T,   'XAxis', 'MAG_BP', 'RowFilter', maska);
    %   plotResidualsFromReport(Rep, 'XAxis', 'X', 'YStat', 'rms', ...
    %                                'SnapshotIndex', 'first');

    arguments
        Rep
        Args.XAxis           {mustBeTextScalar} = 'mag'
        Args.YStat           {mustBeMember(Args.YStat, {'residual','rms'})} = 'residual'
        Args.SnapshotIndex                     = 'last'
        Args.UseOnly         {mustBeMember(Args.UseOnly, {'used','all'})} = 'used'
        Args.RowFilter                         = []
        Args.Normalize       logical           = false
        Args.OverlayTrend    {mustBeMember(Args.OverlayTrend, {'median','mean','none'})} = 'median'
        Args.TrendBinWidth                     = []
        Args.TrendBins       (1,1) double      = 30
        Args.MinCount        (1,1) double      = 5
        Args.FitLine         logical           = false
        Args.ShowMedianShift logical           = false
        Args.YLim                              = []
        Args.MarkerSize      (1,1) double      = 3
    end

    Fig = [];

    % ---- Normalise input shape ------------------------------------------
    if istable(Rep)
        RepStruct = table2struct(Rep);        % Nx1 struct array
    elseif isstruct(Rep)
        RepStruct = Rep;
    else
        error('plotResidualsFromReport:BadRep', ...
              'Rep must be a struct array or a table view of one.');
    end
    Nrow = numel(RepStruct);
    if Nrow == 0
        warning('plotResidualsFromReport:Empty', 'Rep is empty.');
        return;
    end

    % ---- Row-level filter ------------------------------------------------
    if isempty(Args.RowFilter)
        Keep = true(Nrow, 1);
    elseif islogical(Args.RowFilter) || isnumeric(Args.RowFilter)
        Keep = logical(Args.RowFilter);
        if numel(Keep) ~= Nrow
            error('plotResidualsFromReport:BadFilter', ...
                  'RowFilter vector length %d ~= numel(Rep) %d.', numel(Keep), Nrow);
        end
    elseif isa(Args.RowFilter, 'function_handle')
        Keep = arrayfun(@(k) logical(Args.RowFilter(RepStruct(k))), 1:Nrow);
        Keep = Keep(:);
    else
        error('plotResidualsFromReport:BadFilterType', ...
              'RowFilter must be a logical vector or a function handle.');
    end

    % ---- Walk rows and pool residuals -----------------------------------
    XPool = []; ResPool = []; RowIdxPool = []; NRowContrib = 0; NCalibPool = 0;
    XLabelStr = i_xlabelFor(Args.XAxis);
    for K = 1:Nrow
        if ~Keep(K); continue; end
        Row = RepStruct(K);

        % CalibTrajectory: struct array; may be wrapped in a cell when it
        % came in via table2struct.
        Traj = i_getTrajectory(Row);
        if isempty(Traj); continue; end
        S = i_pickSnap(Traj, Args.SnapshotIndex);
        if isempty(S) || isempty(S.SourceData); continue; end
        Tab = S.SourceData.Table;
        if isempty(Tab) || ~ismember('Residuals', Tab.Properties.VariableNames)
            continue;
        end

        % Used / all
        if ismember('Used', Tab.Properties.VariableNames)
            UsedMask = logical(Tab.Used);
        else
            UsedMask = true(height(Tab), 1);
        end
        switch Args.UseOnly
            case 'used'; RowMask = UsedMask;
            case 'all';  RowMask = true(height(Tab), 1);
        end

        R = double(Tab.Residuals(RowMask));
        if Args.Normalize
            if ~ismember('MagErr', Tab.Properties.VariableNames); continue; end
            R = R ./ double(Tab.MagErr(RowMask));
        end

        Xv = i_resolveX(Args.XAxis, Tab, RowMask, Row);
        if isempty(Xv); continue; end

        Good = isfinite(R) & isfinite(Xv);
        XPool      = [XPool;      Xv(Good)];        %#ok<AGROW>
        ResPool    = [ResPool;    R(Good)];         %#ok<AGROW>
        RowIdxPool = [RowIdxPool; repmat(K, nnz(Good), 1)]; %#ok<AGROW>
        NCalibPool  = NCalibPool + nnz(Good);
        NRowContrib = NRowContrib + 1;
    end

    if isempty(ResPool)
        warning('plotResidualsFromReport:NoData', ...
                'No valid residual data for XAxis=%s.', Args.XAxis);
        return;
    end

    % ---- Bin planning ----------------------------------------------------
    Xmin = min(XPool);
    Xmax = max(XPool);
    if ~isempty(Args.TrendBinWidth)
        BW = Args.TrendBinWidth;
    else
        BW = (Xmax - Xmin) / max(Args.TrendBins, 1);
    end
    CanBin = isfinite(BW) && BW > 0 && Xmax > Xmin;

    % ---- Plot ------------------------------------------------------------
    if strcmp(Args.YStat, 'rms')
        Fig = figure('Name', sprintf('Residual RMS vs %s', Args.XAxis), ...
                     'Position', [50 50 620 500]); hold on;
        if CanBin
            T = binnedTrend(XPool, ResPool, 'BinWidth', BW, ...
                'Range', [Xmin Xmax], 'Stat', 'median', 'MinCount', Args.MinCount);
            plot(T.X, T.Std, '-r', 'LineWidth', 2);
        end
        set(gca, 'YScale', 'log');
        YLabelStr = 'Residual RMS [mag]';
    else
        Fig = figure('Name', sprintf('Residuals vs %s', Args.XAxis), ...
                     'Position', [50 50 620 500]); hold on;
        plot(XPool, ResPool, '.', 'MarkerSize', Args.MarkerSize);
        plot(xlim, [0 0], 'k--');

        if ~strcmp(Args.OverlayTrend, 'none') && CanBin
            T = binnedTrend(XPool, ResPool, 'BinWidth', BW, ...
                'Range', [Xmin Xmax], 'Stat', Args.OverlayTrend, ...
                'MinCount', Args.MinCount);
            plot(T.X, T.Val, '-r', 'LineWidth', 2);
        end
        if Args.ShowMedianShift
            MShift = median(ResPool, 'omitnan');
            plot(xlim, [MShift MShift], '--m', 'LineWidth', 1.5);
            text(0.05, 0.95, sprintf('median = %.4f', MShift), ...
                'Units', 'normalized', 'VerticalAlignment', 'top', ...
                'FontSize', 10, 'BackgroundColor', 'w');
        end
        if Args.FitLine
            Cf = polyfit(XPool, ResPool, 1);
            XFit = linspace(Xmin, Xmax, 100);
            plot(XFit, polyval(Cf, XFit), '-b', 'LineWidth', 2);
            text(0.05, 0.85, sprintf('slope = %.4g', Cf(1)), ...
                'Units', 'normalized', 'VerticalAlignment', 'top', ...
                'FontSize', 10, 'BackgroundColor', 'w');
        end
        if Args.Normalize
            YLabelStr = 'Residual / MagErr';
        else
            YLabelStr = 'Residual [mag]';
        end
    end

    box on; grid on;
    xlabel(XLabelStr, 'Interpreter', 'tex');
    ylabel(YLabelStr, 'Interpreter', 'none');
    if ~isempty(Args.YLim); ylim(Args.YLim); end
    title(sprintf('Report residuals vs %s   (%d calibrators, %d rows)', ...
        Args.XAxis, NCalibPool, NRowContrib), 'Interpreter', 'none');
end


% ==== Local helpers ====================================================

function Traj = i_getTrajectory(Row)
    % CalibTrajectory may arrive as struct array (from a raw Rep row) or
    % wrapped in a cell (when row came via table2struct of a Rep-derived
    % table). Handle both.
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


function S = i_pickSnap(Traj, Which)
    % Select a snapshot (last / first / integer index).
    N = numel(Traj);
    if ischar(Which) || isstring(Which)
        switch lower(char(Which))
            case 'first'; K = 1;
            case 'last';  K = N;
            otherwise
                error('plotResidualsFromReport:BadSnap', ...
                    'SnapshotIndex must be an integer, ''first'', or ''last''.');
        end
    else
        K = double(Which);
        if K < 1 || K > N || K ~= round(K)
            error('plotResidualsFromReport:BadSnap', ...
                'SnapshotIndex %g out of range [1, %d].', K, N);
        end
    end
    S = Traj(K);
end


function Lab = i_xlabelFor(X)
    switch lower(X)
        case 'mag';          Lab = 'MAG\_AB';
        case 'instrumental'; Lab = '-2.5 log_{10}(Flux)';
        case 'airmass';      Lab = 'AIRMASS';
        case 'background';   Lab = '1 / Flux';
        otherwise;           Lab = strrep(X, '_', '\_');
    end
end


function Xv = i_resolveX(XAxis, Tab, Mask, Row)
    % Resolve the X quantity for one row's calibrator subset.
    Cols = Tab.Properties.VariableNames;
    Xv = [];

    % 1) Specials
    switch lower(XAxis)
        case 'mag'
            % LAST convention: MAG_AB, then MAG_APER_3, then MAG_BP fallback
            for Nm = {'MAG_AB','MAG_APER_3','MAG_BP'}
                if ismember(Nm{1}, Cols)
                    Xv = double(Tab.(Nm{1})(Mask));
                    return;
                end
            end
            return;                                 % nothing usable
        case 'instrumental'
            if ismember('Flux', Cols)
                Xv = -2.5 * log10(double(Tab.Flux(Mask)));
            end
            return;
        case 'airmass'
            % Row-scalar broadcast: prefer top-level AIRMASS column, fall
            % back to ObsMetadata.AirMass, then Report row AirMass alias.
            V = i_getRowScalar(Row, 'AIRMASS');
            if ~isfinite(V); V = i_getObsMetaField(Row, 'AirMass'); end
            if ~isfinite(V); return; end
            Xv = repmat(V, nnz(Mask), 1);
            return;
        case 'background'
            if ismember('Flux', Cols)
                Xv = 1 ./ double(Tab.Flux(Mask));
            end
            return;
    end

    % 2) 'A-B' column-difference form
    if contains(XAxis, '-')
        Parts = strsplit(XAxis, '-');
        if numel(Parts) == 2 && all(ismember(strtrim(Parts), Cols))
            A = double(Tab.(strtrim(Parts{1}))(Mask));
            B = double(Tab.(strtrim(Parts{2}))(Mask));
            Xv = A - B;
            return;
        end
    end

    % 3) Direct SourceData column
    if ismember(XAxis, Cols)
        Xv = double(Tab.(XAxis)(Mask));
        return;
    end

    % 4) Per-row scalar (broadcast across the row's calibrators)
    V = i_getRowScalar(Row, XAxis);
    if isfinite(V)
        Xv = repmat(V, nnz(Mask), 1);
        return;
    end

    % 5) ObsMetadata subfield fallback (row.ObsMetadata.<XAxis>)
    V = i_getObsMetaField(Row, XAxis);
    if isfinite(V)
        Xv = repmat(V, nnz(Mask), 1);
        return;
    end

    % 6) Nothing matched
end


function V = i_getRowScalar(Row, Name)
    V = NaN;
    if isfield(Row, Name)
        Cand = Row.(Name);
        if isnumeric(Cand) && isscalar(Cand)
            V = double(Cand);
        elseif iscell(Cand) && ~isempty(Cand) && isnumeric(Cand{1}) && isscalar(Cand{1})
            % Table columns land as cells when heterogeneous; unwrap.
            V = double(Cand{1});
        end
    end
end


function V = i_getObsMetaField(Row, Name)
    V = NaN;
    if ~isfield(Row, 'ObsMetadata'); return; end
    M = Row.ObsMetadata;
    if iscell(M) && ~isempty(M); M = M{1}; end
    if isstruct(M) && isfield(M, Name) && isnumeric(M.(Name)) && isscalar(M.(Name))
        V = double(M.(Name));
    end
end
