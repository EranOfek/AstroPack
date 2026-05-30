function plotARMSHistograms(Data, Args)
    % Histograms of per-crop ARMS values produced by batchARMSHistograms
    % Description: Renders one or more figures of ARMS distributions.
    %              Accepts a batchARMSHistograms Result struct, a bare
    %              ARMS table, or a cell of such items. A cell input
    %              overlays one histogram per element (e.g. compare two
    %              multi-night sets {Tall0, Tall1}); a single struct/table
    %              is optionally split into overlaid groups by a column
    %              ('Cohort' by default). Quantities listed in
    %              'LogXQuantities' are drawn with log-spaced bin edges
    %              and a log X axis.
    %
    % Input  : - Data: one of
    %              * a struct with field .ARMS (a table) — a
    %                batchARMSHistograms Result;
    %              * a table whose columns are <Quantity>_ARMS;
    %              * a cell array of the above — each element is one
    %                overlaid group.
    %                    * ...,key,val,...
    %            'Quantities'     - cell of quantity names to plot.
    %                               Default {} = inferred from the
    %                               *_ARMS columns (or Result.Quantities).
    %            'PanelGroups'    - cell of cells; each inner cell is one
    %                               figure, rendered as a row of panels.
    %                               Default {} = one figure, one panel
    %                               per quantity.
    %            'GroupColumn'    - for a single struct/table input, the
    %                               column whose distinct values become
    %                               overlaid groups. Default 'Cohort'.
    %                               Ignored for cell input. '' disables
    %                               splitting (one histogram).
    %            'Labels'         - legend labels. For cell input, one per
    %                               element. Default {} = 'set 1','set 2'…
    %                               (cell) or the GroupColumn values.
    %            'AngularQuantities' - names whose ARMS is in arcsec.
    %                               Default {'RA','Dec'}.
    %            'FluxAsMag'      - names whose ARMS is in mag. Default
    %                               {'FLUX_PSF','FLUX_APER_3'}.
    %            'LogXQuantities' - names drawn with a log X axis
    %                               (log-spaced edges, non-positive
    %                               values dropped). Default {}.
    %            'LogYQuantities' - names drawn with a log Y axis (the
    %                               counts / probability axis). Useful
    %                               when the bulk of sources sit in one
    %                               tall bin and the tail bins vanish on
    %                               a linear scale. Pass 'all' (or the
    %                               sentinel '__all__') to apply log Y to
    %                               every panel regardless of quantity.
    %                               Default {}.
    %            'Bins'           - bin count or explicit edges.
    %                               Default 30.
    %            'Normalization'  - histogram Normalization. Default
    %                               'probability'.
    %            'SaveFig'        - save .fig/.png per figure. Default
    %                               false.
    %            'OutDir'         - directory for saved figures. Default
    %                               '~/results_ARMS'.
    % Output : - none (figures only).
    % Author : D. Kovaleva (May 2026)
    % Example: % Re-plot a single batchARMSHistograms result:
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms(BaseDir);
    %          pipeline.last.quality.photCalib.plotARMSHistograms(R);
    %
    %          % Overlay two multi-night sets, RA/Dec on a log X axis:
    %          pipeline.last.quality.photCalib.plotARMSHistograms( ...
    %              {Tall0, Tall1}, 'Labels', {'v0','v2'}, ...
    %              'PanelGroups', {{'RA','Dec'},{'FLUX_APER_3'}}, ...
    %              'LogXQuantities', {'RA','Dec'});
    %
    %          % Same overlay but with a log Y (counts) axis on every
    %          % panel -- expose the tail bins:
    %          pipeline.last.quality.photCalib.plotARMSHistograms( ...
    %              {Tall0, Tall1}, 'Labels', {'v0','v2'}, ...
    %              'LogYQuantities', 'all');
    %
    %          % One combined table, split by its Cohort column:
    %          Tall = vertcat(R1.ARMS, R2.ARMS, R3.ARMS);
    %          pipeline.last.quality.photCalib.plotARMSHistograms(Tall);
    %
    %          % Compare V0 vs V1 pipelines on only the visits they share.
    %          % Visit names differ by a v0/v1 suffix, so a plain
    %          % intersect/setdiff on ARMS.Visit treats them as disjoint;
    %          % strip the trailing v\d+ first, then filter both tables:
    %          B0 = regexprep(string(R0.ARMS.Visit), 'v\d+$', '');
    %          B1 = regexprep(string(R1.ARMS.Visit), 'v\d+$', '');
    %          Common = intersect(B0, B1);
    %          R0ov   = R0.ARMS(ismember(B0, Common), :);
    %          R1ov   = R1.ARMS(ismember(B1, Common), :);
    %          pipeline.last.quality.photCalib.plotARMSHistograms( ...
    %              {R0ov, R1ov}, 'Labels', {'v0','v1'});
    %
    %          % If V1 visit IDs were renumbered with an integer offset
    %          % relative to V0 (V0_id == V1_id + Offset, with Offset 10
    %          % or 15 in some LAST reprocessings), cast the stripped
    %          % bases to numeric and align before intersecting:
    %          Offset = 10;                        % or 15; 0 = no shift
    %          N0 = double(string(B0));
    %          N1 = double(string(B1)) + Offset;   % shift V1 onto V0 numbering
    %          Common = intersect(N0, N1);
    %          R0ov   = R0.ARMS(ismember(N0, Common), :);
    %          R1ov   = R1.ARMS(ismember(N1, Common), :);
    %          pipeline.last.quality.photCalib.plotARMSHistograms( ...
    %              {R0ov, R1ov}, 'Labels', {'v0','v1'});
    %          % setdiff(B0,B1) / setdiff(B1,B0) (or setdiff(N0,N1)) for
    %          % the asymmetric tails.

    arguments
        Data
        Args.Quantities        cell         = {}
        Args.PanelGroups       cell         = {}
        Args.GroupColumn       (1,:) char   = 'Cohort'
        Args.Labels            cell         = {}
        Args.AngularQuantities cell         = {'RA','Dec'}
        Args.FluxAsMag         cell         = {'FLUX_PSF','FLUX_APER_3'}
        Args.LogXQuantities    cell         = {}
        Args.LogYQuantities    cell         = {}     % names rendered with a log Y axis (counts axis). Use 'all' or {'__all__'} to apply to every panel.
        Args.Bins                           = 30
        Args.Normalization     (1,:) char   = 'probability'
        Args.SaveFig           logical      = false
        Args.OutDir            {mustBeText} = '~/results_ARMS'
    end

    % --- Normalise input into a cell of {table} groups + labels -------
    [GroupTabs, GroupLabels, Quantities] = i_normalise(Data, Args);
    if isempty(GroupTabs)
        error('plotARMSHistograms:NoData', 'No ARMS data to plot.');
    end

    PanelGroups = Args.PanelGroups;
    if isempty(PanelGroups)
        PanelGroups = {Quantities};
    end

    Cmap   = lines(max(numel(GroupTabs), 1));
    OutDir = char(Args.OutDir);
    if Args.SaveFig && ~exist(OutDir, 'dir'); mkdir(OutDir); end

    for Ig = 1:numel(PanelGroups)
        QGroup = PanelGroups{Ig};
        Np     = numel(QGroup);
        F = figure('Name', sprintf('ARMS: %s', strjoin(QGroup, ' / ')), ...
            'Position', [60 60 420*Np 380]);
        for Ip = 1:Np
            Ax    = subplot(1, Np, Ip); hold(Ax, 'on');
            Q     = QGroup{Ip};
            Col   = i_varName(Q);
            IsLog = ismember(Q, Args.LogXQuantities);

            % pooled finite (and >0 if log) values for common edges
            All = [];
            for Ks = 1:numel(GroupTabs)
                T = GroupTabs{Ks};
                if ismember(Col, T.Properties.VariableNames)
                    v = T.(Col);
                    v = v(isfinite(v));
                    if IsLog; v = v(v > 0); end
                    All = [All; v(:)]; %#ok<AGROW>
                end
            end
            if isempty(All)
                title(Ax, sprintf('%s (no data)', Q), 'Interpreter','none');
                continue;
            end
            if isscalar(Args.Bins)
                if IsLog
                    Edges = logspace(log10(min(All)), log10(max(All)), Args.Bins+1);
                else
                    Edges = linspace(min(All), max(All), Args.Bins+1);
                end
            else
                Edges = Args.Bins;
            end

            for Ks = 1:numel(GroupTabs)
                T = GroupTabs{Ks};
                if ~ismember(Col, T.Properties.VariableNames); continue; end
                v = T.(Col);
                v = v(isfinite(v));
                if IsLog; v = v(v > 0); end
                if isempty(v); continue; end
                histogram(Ax, v, Edges, ...
                    'FaceColor', Cmap(Ks,:), 'FaceAlpha', 0.55, ...
                    'EdgeColor', Cmap(Ks,:)*0.6, 'LineWidth', 0.8, ...
                    'Normalization', Args.Normalization, ...
                    'DisplayName', sprintf('%s (N=%d, med=%.3g)', ...
                        GroupLabels{Ks}, numel(v), median(v)));
            end
            if IsLog; set(Ax, 'XScale', 'log'); end
            % Y axis (counts/probability): log when the quantity is in
            % LogYQuantities, or when the user passed 'all' / '__all__'.
            ApplyLogY = ismember(Q, Args.LogYQuantities) || ...
                        any(strcmpi(Args.LogYQuantities, 'all')) || ...
                        any(strcmp(Args.LogYQuantities, '__all__'));
            if ApplyLogY; set(Ax, 'YScale', 'log'); end
            grid(Ax, 'on'); box(Ax, 'on');
            U = i_armsUnit(Q, Args);
            if isempty(U)
                xlabel(Ax, sprintf('ARMS(%s)', Q), 'Interpreter', 'none');
            else
                xlabel(Ax, sprintf('ARMS(%s) [%s]', Q, U), 'Interpreter', 'none');
            end
            ylabel(Ax, i_normLabel(Args.Normalization));
            title(Ax, Q, 'Interpreter', 'none');
            if numel(GroupTabs) > 1
                legend(Ax, 'Location', 'best', 'Interpreter', 'none');
            end
        end
        sgtitle(sprintf('ARMS distribution: %s', strjoin(QGroup, ' / ')), ...
            'Interpreter', 'none');
        if Args.SaveFig
            Base = fullfile(OutDir, sprintf('arms_%s', ...
                matlab.lang.makeValidName(strjoin(QGroup, '_'))));
            savefig(F, [Base '.fig']);
            saveas(F,  [Base '.png']);
        end
    end
end

% =========================================================================
function [Tabs, Labels, Quantities] = i_normalise(Data, Args)
    % Resolve Data into a cell of group tables, their labels, and the
    % quantity name list.
    Tabs = {}; Labels = {}; Quantities = Args.Quantities;

    if iscell(Data)
        for I = 1:numel(Data)
            [T, Q] = i_toTable(Data{I});
            Tabs{end+1} = T; %#ok<AGROW>
            if isempty(Quantities); Quantities = Q; end
        end
        if ~isempty(Args.Labels)
            Labels = cellfun(@char, Args.Labels(:).', 'Uni', 0);
        else
            Labels = arrayfun(@(k) sprintf('set %d', k), ...
                1:numel(Tabs), 'Uni', 0);
        end
        return;
    end

    % single struct or table
    [T, Q] = i_toTable(Data);
    if isempty(Quantities); Quantities = Q; end
    GC = Args.GroupColumn;
    if ~isempty(GC) && istable(T) && ismember(GC, T.Properties.VariableNames)
        Vals = string(T.(GC));
        U    = unique(Vals, 'stable');
        for I = 1:numel(U)
            Tabs{end+1}   = T(Vals == U(I), :);   %#ok<AGROW>
            Labels{end+1} = char(U(I));           %#ok<AGROW>
        end
    else
        Tabs   = {T};
        Labels = {'all'};
    end
end

% =========================================================================
function [T, Q] = i_toTable(D)
    % Coerce one input element to a table; report its quantity list.
    Q = {};
    if istable(D)
        T = D;
    elseif isstruct(D) && isfield(D, 'ARMS') && istable(D.ARMS)
        T = D.ARMS;
        if isfield(D, 'Quantities'); Q = D.Quantities; end
    else
        error('plotARMSHistograms:BadInput', ...
            ['Each input must be a table or a batchARMSHistograms ', ...
             'Result struct (with field .ARMS).']);
    end
    if isempty(Q)
        Vn   = T.Properties.VariableNames;
        Mask = endsWith(Vn, '_ARMS');
        Q    = cellfun(@(s) s(1:end-5), Vn(Mask), 'Uni', 0);
    end
end

% =========================================================================
function V = i_varName(Q)
    % Struct-field / table-column-safe ARMS column name for quantity Q.
    V = matlab.lang.makeValidName([char(Q) '_ARMS']);
end

% =========================================================================
function U = i_armsUnit(Q, Args)
    % Unit string for the ARMS of quantity Q:
    %   arcsec - angular quantities (std rescaled x3600 x cos Dec),
    %   mag    - FluxAsMag columns (-2.5*log10) and MAG_* columns,
    %   ''     - otherwise (raw native units).
    Qc = char(Q);
    if ismember(Q, Args.AngularQuantities)
        U = 'arcsec';
    elseif ismember(Q, Args.FluxAsMag)
        U = 'mag';
    elseif startsWith(upper(Qc), 'MAG')
        U = 'mag';
    else
        U = '';
    end
end

% =========================================================================
function L = i_normLabel(N)
    % Y-axis label matching the histogram Normalization.
    switch lower(N)
        case 'probability'; L = 'Fraction';
        case 'count';       L = 'Count';
        case 'countdensity';L = 'Count density';
        case 'pdf';         L = 'PDF';
        case 'cdf';         L = 'CDF';
        otherwise;          L = N;
    end
end
