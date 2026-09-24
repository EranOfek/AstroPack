function [X, Y, Xb, Ym] = plotCalParams(Src, XCol, YCol, Args)
    % Scatter one column against another, overlaid with a binned median
    % (+ optional Q1-Q3 or MAD band).
    %
    % Src can be any of:
    %   (a) A fitPhotCalibTrans CalibTrajectory - a struct array with
    %       .SourceData (AstroCatalog) per entry. Extract the snapshot
    %       selected by SnapshotIndex and plot from its Table.
    %   (b) A MATLAB table.
    %   (c) An AstroCatalog (its .Table is used).
    %
    % For (a) the entries carry per-source columns:
    %   Used, Residuals, PredictedFlux, MagErr,
    %   Flux, FluxErr, X, Y, RA, Dec, MatchDistance, NumMatches,
    %   and optionally AIRMASS, BP_RP, MAG_BP, MAG_RP.
    % For (b)/(c) any two columns present in the table can be plotted; the
    % SnapshotIndex / UseOnly args are silently ignored.
    %
    % Usage:
    %   plotCalParams(Cal, 'X', 'Residuals')                       % last snap
    %   plotCalParams(Cal, 'MAG_BP', 'Residuals', ...
    %                 'SnapshotIndex', 4, 'UseOnly', 'all');
    %   plotCalParams(Cal, 'MatchDistance', 'Residuals', ...
    %                 'NBins', 30, 'OverlayBand', 'std');
    %   plotCalParams(Cal, 'Flux', 'Residuals', 'LogX', true);
    %   % Fixed 0.5-mag-wide bins (overrides NBins):
    %   plotCalParams(Cal, 'MAG_BP', 'Residuals', 'BinWidth', 0.5);
    %   % One horizontal line at the global median with a Q1-Q3 band:
    %   plotCalParams(Cal, 'X', 'Residuals', 'BinMode', 'global');
    %   % Same, via the NBins=1 shortcut:
    %   plotCalParams(Cal, 'X', 'Residuals', 'NBins', 1);
    %   % Semilogy of |Residuals| vs pixel X:
    %   plotCalParams(Cal, 'X', 'Residuals', 'AbsY', true, 'LogY', true);
    %   % Same but robust MAD band (matches the log-Y scale better):
    %   plotCalParams(Cal, 'X', 'Residuals', ...
    %                 'AbsY', true, 'LogY', true, 'OverlayBand', 'std');
    %
    %   % Generic tables:
    %   T = readtable('/home/dana/tmp/N3/parameters_py_m.csv');
    %   plotCalParams(T, 'csv_norm', 'pc_norm');                     % 1:1 scatter
    %   plotCalParams(T, 'AIRMASS',  'pc_RMS', 'OverlayBand', 'std');
    %
    %   % AstroCatalog:
    %   Cat = AI.CatData;
    %   plotCalParams(Cat, 'MAG_APER_3', 'MAGERR_APER_3', 'LogY', true);
    %
    % Inputs:
    %   Cal  - CalibTrajectory struct array (see fitPhotCalibTrans doc).
    %   XCol - name of the column plotted on X (from Cal(k).SourceData.Table).
    %   YCol - name of the column plotted on Y.
    %
    % Optional name-value pairs:
    %   'SnapshotIndex' - Which entry of Cal to plot. Default = numel(Cal)
    %                     (final calibrator list). Accepts an integer
    %                     1..numel(Cal), 'first', or 'last'.
    %   'UseOnly'       - 'used' (default: only rows with SourceData.Used
    %                     true) or 'all' (every calibrator that entered
    %                     this snap; discarded ones are drawn faded).
    %   'NBins'         - Bin count for the median overlay. Default 20.
    %                     Ignored when BinWidth is set.
    %   'BinWidth'      - Explicit bin width in X units (e.g. 0.5 mag).
    %                     Empty (default) means use NBins+BinMode. When
    %                     set, overrides NBins/BinMode: bins run over
    %                     floor(min(X)/BW)*BW : BW : ceil(max(X)/BW)*BW.
    %                     Applies to linear X only; on LogX the log-spaced
    %                     NBins path is used regardless.
    %   'MinPerBin'     - Skip bins with fewer valid points. Default 5.
    %   'BinMode'       - 'edges' (equal-width, default), 'equalCount',
    %                     or 'global'. Ignored when BinWidth is set.
    %                     'global' collapses to a single bin over all X:
    %                     Ym is the median of ALL Y (one scalar), drawn
    %                     as a horizontal line spanning min(X)..max(X),
    %                     with the band (Q1/Q3 or MAD) shown as a
    %                     transparent horizontal strip. Also triggered
    %                     by NBins=1.
    %   'OverlayMedian' - Draw the binned-median line. Default true.
    %   'OverlayBand'   - 'quantile' (Q1-Q3, default), 'std'
    %                     (median +- 1.4826 * MAD), or 'none'.
    %   'OverlayColor'  - Line + band RGB triplet. Default [1 0 0] (red).
    %   'MarkerSize'    - Scatter marker area (points^2). Default 12.
    %   'LineWidth'     - Median-line width. Default 2.5.
    %   'LogX', 'LogY'  - Log-scale axes. Default false. LogX also
    %                     switches to log-spaced bin edges. LogY forces
    %                     drop of any non-positive Y values before
    %                     binning (matplotlib-style behaviour).
    %   'AbsX', 'AbsY'  - Take abs() of the column before plotting.
    %                     Default false. Useful for |Residuals| on
    %                     semilogy — combine with 'LogY', true.
    %   'Title'         - Figure title override. Default auto (Cal index,
    %                     StageName, StageIndex/IterIndex/OuterIter, N).
    %   'XLabel','YLabel' - Axis label override. Default: column name.
    %
    % Outputs (all optional):
    %   X, Y - per-source vectors actually plotted (after UseOnly + finite).
    %   Xb   - bin centres of the median overlay.
    %   Ym   - per-bin median values.

    arguments
        Src
        XCol       (1,:) char
        YCol       (1,:) char
        Args.SnapshotIndex               = []
        Args.UseOnly       (1,:) char    = 'used'
        Args.NBins         (1,1) double  = 20
        Args.BinWidth              double = []
        Args.MinPerBin     (1,1) double  = 5
        Args.BinMode       (1,:) char    = 'edges'
        Args.OverlayMedian (1,1) logical = true
        Args.OverlayBand   (1,:) char    = 'quantile'
        Args.OverlayColor  (1,3) double  = [1 0 0]
        Args.MarkerSize    (1,1) double  = 12
        Args.LineWidth     (1,1) double  = 2.5
        Args.LogX          (1,1) logical = false
        Args.LogY          (1,1) logical = false
        Args.AbsX          (1,1) logical = false
        Args.AbsY          (1,1) logical = false
        Args.Title         (1,:) char    = ''
        Args.XLabel        (1,:) char    = ''
        Args.YLabel        (1,:) char    = ''
    end

    % ---- Resolve Src -> Tab (MATLAB table) --------------------------------
    % Three input modes:
    %   (a) struct array with .SourceData (CalibTrajectory)
    %   (b) MATLAB table
    %   (c) AstroCatalog (or anything else with a .Table property)
    S = [];   % snapshot struct, used only for the auto-title (mode (a))
    K = [];   % snapshot index for the auto-title
    if istable(Src)
        Tab = Src;
    elseif isstruct(Src) && ~isempty(Src) && isfield(Src, 'SourceData')
        N = numel(Src);
        if isempty(Args.SnapshotIndex)
            K = N;
        elseif ischar(Args.SnapshotIndex) || isstring(Args.SnapshotIndex)
            switch lower(char(Args.SnapshotIndex))
                case 'first'; K = 1;
                case 'last';  K = N;
                otherwise
                    error('plotCalParams:BadSnap', ...
                        'SnapshotIndex must be an integer, ''first'' or ''last''.');
            end
        else
            K = double(Args.SnapshotIndex);
            if K < 1 || K > N || K ~= round(K)
                error('plotCalParams:BadSnap', ...
                    'SnapshotIndex %g out of range [1, %d].', K, N);
            end
        end
        S = Src(K);
        if isempty(S.SourceData)
            error('plotCalParams:NoSourceData', ...
                'Src(%d) has no SourceData.', K);
        end
        Tab = S.SourceData.Table;
    elseif isa(Src, 'AstroCatalog') || isprop(Src, 'Table')
        Tab = Src.Table;
    else
        error('plotCalParams:BadSrc', ...
            ['First argument must be a CalibTrajectory struct array, ', ...
             'a MATLAB table, or an AstroCatalog.']);
    end
    if ~istable(Tab)
        error('plotCalParams:BadTable', ...
            'Resolved data source has no MATLAB table to read from.');
    end
    VN = Tab.Properties.VariableNames;

    if ~ismember(XCol, VN)
        error('plotCalParams:NoX', ...
            'Table has no column "%s". Available: %s', ...
            XCol, strjoin(VN, ', '));
    end
    if ~ismember(YCol, VN)
        error('plotCalParams:NoY', ...
            'Table has no column "%s". Available: %s', ...
            YCol, strjoin(VN, ', '));
    end

    X = double(Tab.(XCol));
    Y = double(Tab.(YCol));
    if Args.AbsX; X = abs(X); end
    if Args.AbsY; Y = abs(Y); end

    Used = true(numel(X), 1);
    if ismember('Used', VN); Used = logical(Tab.Used); end

    switch lower(Args.UseOnly)
        case 'used'
            KeepMask = Used;
        case 'all'
            KeepMask = true(numel(X), 1);
        otherwise
            error('plotCalParams:BadUseOnly', ...
                'UseOnly must be ''used'' or ''all''.');
    end

    Fin = isfinite(X) & isfinite(Y);
    if Args.LogY; Fin = Fin & Y > 0; end   % drop non-positive Y for semilogy
    if Args.LogX; Fin = Fin & X > 0; end   % ditto for semilogx
    Keep = KeepMask & Fin;
    X = X(Keep);
    Y = Y(Keep);
    Usel = Used(Keep);

    if isempty(X)
        error('plotCalParams:NoData', 'No finite data to plot.');
    end

    % ---- Plot dots -------------------------------------------------------
    figure('Color', [1 1 1]); hold on; grid on; box on;
    DotLabel = 'data';
    if ~isempty(S); DotLabel = 'calibrators'; end
    if strcmpi(Args.UseOnly, 'all')
        scatter(X(~Usel), Y(~Usel), Args.MarkerSize, ...
                [0.6 0.6 0.6], 'filled', 'MarkerFaceAlpha', 0.4, ...
                'DisplayName', 'discarded');
        scatter(X(Usel),  Y(Usel),  Args.MarkerSize, ...
                [0.1 0.1 0.6], 'filled', 'MarkerFaceAlpha', 0.7, ...
                'DisplayName', 'used');
    else
        scatter(X, Y, Args.MarkerSize, [0.1 0.1 0.6], 'filled', ...
                'MarkerFaceAlpha', 0.6, 'DisplayName', DotLabel);
    end

    % ---- Binned median + band overlay ------------------------------------
    Xb = []; Ym = [];
    IsGlobal = strcmpi(Args.BinMode, 'global') || Args.NBins == 1;
    if Args.OverlayMedian && numel(X) >= max(Args.MinPerBin, 2) && IsGlobal
        % Single global bin: horizontal line at median(Y), spanning
        % [min(X), max(X)] (log-safe end-points via Xpos when LogX).
        if Args.LogX
            Xpos = X(X > 0);
            if isempty(Xpos)
                error('plotCalParams:LogXempty', ...
                    'LogX requested but X has no positive values.');
            end
            Xlo = min(Xpos); Xhi = max(Xpos);
        else
            Xlo = min(X);    Xhi = max(X);
        end
        Med = median(Y);
        switch lower(Args.OverlayBand)
            case 'quantile'
                Q = quantile(Y, [0.25, 0.75]);
                Lo = Q(1); Hi = Q(2);
            case 'std'
                Sm = 1.4826 * median(abs(Y - Med));
                Lo = Med - Sm; Hi = Med + Sm;
            case 'none'
                Lo = NaN; Hi = NaN;
            otherwise
                error('plotCalParams:BadBand', ...
                    'OverlayBand must be ''quantile'', ''std'', or ''none''.');
        end
        if ~strcmpi(Args.OverlayBand, 'none')
            patch([Xlo Xhi Xhi Xlo], [Lo Lo Hi Hi], Args.OverlayColor, ...
                  'FaceAlpha', 0.2, 'EdgeColor', 'none', ...
                  'HandleVisibility', 'off');
        end
        plot([Xlo, Xhi], [Med, Med], '-', 'Color', Args.OverlayColor, ...
             'LineWidth', Args.LineWidth, ...
             'DisplayName', sprintf('global median = %.4g', Med));
        Xb = [Xlo, Xhi];
        Ym = [Med, Med];
    elseif Args.OverlayMedian && numel(X) >= max(Args.MinPerBin, 2)
        if Args.LogX
            Xpos = X(X > 0);
            if isempty(Xpos)
                error('plotCalParams:LogXempty', ...
                    'LogX requested but X has no positive values.');
            end
            Edges = logspace(log10(min(Xpos)), log10(max(Xpos)), Args.NBins + 1);
        elseif ~isempty(Args.BinWidth)
            BW = Args.BinWidth;
            if ~isscalar(BW) || ~isfinite(BW) || BW <= 0
                error('plotCalParams:BadBinWidth', ...
                    'BinWidth must be a positive finite scalar.');
            end
            Lo = floor(min(X)/BW) * BW;
            Hi = ceil (max(X)/BW) * BW;
            Edges = Lo:BW:Hi;
            if numel(Edges) < 2
                Edges = [Lo, Lo + BW];
            end
        else
            switch lower(Args.BinMode)
                case 'edges'
                    Edges = linspace(min(X), max(X), Args.NBins + 1);
                case 'equalcount'
                    Xs = sort(X);
                    Ix = round(linspace(1, numel(Xs), Args.NBins + 1));
                    Edges = Xs(Ix).';
                    Edges = unique(Edges);
                otherwise
                    error('plotCalParams:BadBinMode', ...
                        'BinMode must be ''edges'', ''equalCount'', or ''global''.');
            end
        end
        if Args.LogX
            Ctr = sqrt(Edges(1:end-1) .* Edges(2:end));
        else
            Ctr = 0.5 * (Edges(1:end-1) + Edges(2:end));
        end
        BinId = discretize(X, Edges);
        BinId(isnan(BinId)) = numel(Ctr);   % include right-edge value

        Nb  = numel(Ctr);
        Med = nan(1, Nb); Lo = nan(1, Nb); Hi = nan(1, Nb);
        for b = 1:Nb
            m = BinId == b;
            if nnz(m) < Args.MinPerBin; continue; end
            Yv = Y(m);
            Med(b) = median(Yv);
            switch lower(Args.OverlayBand)
                case 'quantile'
                    Q = quantile(Yv, [0.25, 0.75]);
                    Lo(b) = Q(1); Hi(b) = Q(2);
                case 'std'
                    Sm = 1.4826 * median(abs(Yv - Med(b)));
                    Lo(b) = Med(b) - Sm; Hi(b) = Med(b) + Sm;
                case 'none'
                    % no band
                otherwise
                    error('plotCalParams:BadBand', ...
                        'OverlayBand must be ''quantile'', ''std'', or ''none''.');
            end
        end
        Good = ~isnan(Med);
        Xb = Ctr(Good);
        Ym = Med(Good);
        if ~strcmpi(Args.OverlayBand, 'none')
            LoG = Lo(Good); HiG = Hi(Good);
            patch([Xb, fliplr(Xb)], [HiG, fliplr(LoG)], ...
                  Args.OverlayColor, 'FaceAlpha', 0.2, 'EdgeColor', 'none', ...
                  'HandleVisibility', 'off');
        end
        plot(Xb, Ym, '-', 'Color', Args.OverlayColor, ...
             'LineWidth', Args.LineWidth, 'DisplayName', 'binned median');
    end

    if Args.LogX; set(gca, 'XScale', 'log'); end
    if Args.LogY; set(gca, 'YScale', 'log'); end

    if isempty(Args.XLabel)
        Lab = XCol; if Args.AbsX; Lab = ['|' Lab '|']; end
        xlabel(Lab, 'Interpreter', 'none');
    else
        xlabel(Args.XLabel);
    end
    if isempty(Args.YLabel)
        Lab = YCol; if Args.AbsY; Lab = ['|' Lab '|']; end
        ylabel(Lab, 'Interpreter', 'none');
    else
        ylabel(Args.YLabel);
    end

    if isempty(Args.Title)
        if ~isempty(S)
            StageName = '';
            Tags = {};
            if isfield(S, 'StageName');  StageName = char(S.StageName); end
            if isfield(S, 'StageIndex'); Tags{end+1} = sprintf('S%d', S.StageIndex); end
            if isfield(S, 'IterIndex');  Tags{end+1} = sprintf('I%d', S.IterIndex);  end
            if isfield(S, 'OuterIter');  Tags{end+1} = sprintf('O%d', S.OuterIter);  end
            TagStr = strjoin(Tags, '.');
            title(sprintf('Cal(%d) - %s %s   N=%d', K, StageName, TagStr, numel(X)), ...
                  'Interpreter', 'none');
        else
            title(sprintf('%s vs %s   N=%d', YCol, XCol, numel(X)), ...
                  'Interpreter', 'none');
        end
    else
        title(Args.Title, 'Interpreter', 'none');
    end
    legend('Location', 'best', 'Box', 'off');
    hold off;
end
