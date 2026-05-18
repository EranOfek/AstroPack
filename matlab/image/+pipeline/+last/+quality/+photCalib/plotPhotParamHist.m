function [Result, Fig] = plotPhotParamHist(Input, Args)
    % Histogram of one named photometric-calibration quantity.
    % Description: Collects one value per (epoch, crop) of a PhotCalibTrans
    %              collection and plots its distribution. Three usage modes,
    %              all in this one function (it replaces the former
    %              plotPhotParamHist + plotPhotHistogram +
    %              plotPhotParamCentralEdgeHist):
    %                - PC input  : pass PhotCalibTrans objects directly.
    %                - path input: pass a directory; visits are discovered
    %                              and calibrated via calibrateVisits.
    %                - multi-set : pass a cell of PC sources, or a cell of
    %                              FileType names with a path, to overlay
    %                              several pooled distributions.
    %              With 'CentralEdge' the per-crop values are split into
    %              central and peripheral groups and overlaid; 'Center' then
    %              optionally removes the per-visit median/mean first.
    %
    % Input  : - Input - one of:
    %              * a PhotCalibTrans array / cell of arrays / struct with a
    %                .PC field (anything resolveInput accepts);
    %              * a cell of several such sources (multi-set);
    %              * a char/string directory path (visits are calibrated).
    %          * ...,key,val,...
    %            'Param'          - Quantity name (see resolvePCParam).
    %                               Default 'Chi2_DOF'.
    %            'CentralEdge'    - Split into central vs peripheral crops
    %                               and overlay. Default false.
    %            'Center'         - Per-visit centering before pooling:
    %                               'none' | 'median' | 'mean'. Default
    %                               'none'.
    %            'TileOrder'      - 'rowmajor' | 'colmajor' central-crop
    %                               classification. Default 'rowmajor'.
    %            'CropsToAnalyze' - Crop indices to include. Default [] (all).
    %            'Bins'           - Bin count or explicit edge vector.
    %                               Default 30.
    %            'XLim'           - X-axis limits. Default [] (auto).
    %            'LogX'           - Logarithmic x axis. Default false.
    %            'Stats'          - Overlay median/P16/P84 + a stats box
    %                               (single-series only). Default true.
    %            'Color'          - Bar colour (single series).
    %                               Default [0.30 0.55 0.90].
    %            'HistColors'     - 2x3 [central; peripheral] colours for
    %                               CentralEdge mode. Default
    %                               [0.40 0.40 0.40; 0.85 0.85 0.85].
    %            'Labels'         - Per-set legend labels (multi-set).
    %                               Default {} (auto).
    %            'FileType'       - 'coadd' | 'proc', or a cell of those, for
    %                               path input (a cell overlays one set per
    %                               type). Default 'coadd'.
    %            'VisitGlob'      - Visit glob for path input.
    %                               Default '*v[0-9]*'.
    %            'Recursive'      - Recurse for visit dirs (path input).
    %                               Default true.
    %            'FieldId'        - Field-id filter (path input). Default ''.
    %            'CropID'         - Single-crop filter (path input).
    %                               Default [].
    %            'CalibArgs'      - NV pairs forwarded to fitPhotCalibTrans
    %                               (path input). Default {}.
    %            'OutFile'        - calibrateVisits cache .mat (path input;
    %                               suffixed per set when several). Default ''.
    %            'ForceRecalc'    - Recompute the calibration cache.
    %                               Default false.
    %            'Verbose'        - Print a short summary. Default false.
    % Output : - Result - struct with .Values, .CropID, .EpochID, .SetID,
    %            .IsCentral, .Stats, .Param, .Labels, .Args (and
    %            .StatsCentral/.StatsPeripheral in CentralEdge mode).
    %          - Fig - the created figure handle ([] when there is no data).
    % Author : photCalib package refactor (2026-05)
    % Example: plotPhotParamHist(R.PC, 'Param', 'Chi2_DOF', 'LogX', true);
    %          plotPhotParamHist('/data/2025/04/26', 'Param', 'NCalib');
    %          plotPhotParamHist(R.PC, 'Param', 'Center_Ang', 'CentralEdge', true);

    arguments
        Input
        Args.Param          {mustBeTextScalar} = 'Chi2_DOF'
        Args.CentralEdge    logical = false
        Args.Center         {mustBeMember(Args.Center,{'none','median','mean'})} = 'none'
        Args.TileOrder      {mustBeTextScalar} = 'rowmajor'
        Args.CropsToAnalyze double  = []
        Args.Bins                   = 30
        Args.XLim                   = []
        Args.LogX           logical = false
        Args.Stats          logical = true
        Args.Color                  = [0.30 0.55 0.90]
        Args.HistColors             = [0.40 0.40 0.40; 0.85 0.85 0.85]
        Args.Labels         cell    = {}
        Args.FileType               = 'coadd'
        Args.VisitGlob      {mustBeText} = '*v[0-9]*'
        Args.Recursive      logical = true
        Args.FieldId                = ''
        Args.CropID         double {mustBeInteger,mustBeNonnegative} = []
        Args.CalibArgs      cell    = {}
        Args.OutFile        {mustBeText} = ''
        Args.ForceRecalc    logical = false
        Args.Verbose        logical = false
    end

    Fig    = [];
    Result = struct();

    % --- Resolve Input into a list of PC-cell sources + set labels -----
    [Sources, SetLabels] = i_resolveSources(Input, Args);
    if isempty(Sources)
        warning('photCalib:plotPhotParamHist:NoInput', 'No usable input.');
        Result.Stats = struct('N', 0);
        return;
    end

    % --- Collect one value per (set, visit, crop) ----------------------
    Vals = []; CropID = []; EpochID = []; SetID = [];
    for Is = 1:numel(Sources)
        PCcell = Sources{Is};
        for Iv = 1:numel(PCcell)
            if isempty(PCcell{Iv}); continue; end
            if isempty(Args.CropsToAnalyze)
                Crops = 1:numel(PCcell{Iv});
            else
                Crops = Args.CropsToAnalyze;
            end
            for Ic = Crops
                if Ic > numel(PCcell{Iv}); continue; end
                PCobj = PCcell{Iv}(Ic);
                if ~isa(PCobj,'PhotCalibTrans') || ~PCobj.Success; continue; end
                V = pipeline.last.quality.photCalib.resolvePCParam(PCobj, Args.Param);
                if ~isfinite(V); continue; end
                Vals(end+1,1)    = V;   %#ok<AGROW>
                CropID(end+1,1)  = Ic;  %#ok<AGROW>
                EpochID(end+1,1) = Iv;  %#ok<AGROW>
                SetID(end+1,1)   = Is;  %#ok<AGROW>
            end
        end
    end

    if isempty(Vals)
        Result.Stats = struct('N', 0);
        warning('photCalib:plotPhotParamHist:Empty', ...
            'No finite values collected for "%s".', Args.Param);
        return;
    end

    % --- Optional per-visit centering ----------------------------------
    if ~strcmp(Args.Center, 'none')
        CenterFun = str2func(Args.Center);
        Key = SetID*1e6 + EpochID;
        for Uk = unique(Key).'
            M = (Key == Uk);
            if nnz(M) >= 2
                Vals(M) = Vals(M) - CenterFun(Vals(M));
            end
        end
    end

    % --- Central / peripheral classification ---------------------------
    CentralSet = centralCrops(Args.TileOrder, max(CropID));
    IsCentral  = ismember(CropID, CentralSet);

    % --- Build the series to overlay -----------------------------------
    if Args.CentralEdge
        Series  = {Vals(IsCentral), Vals(~IsCentral)};
        SLabels = {'central', 'peripheral'};
        Colors  = Args.HistColors;
    elseif numel(Sources) > 1
        Series  = cell(1, numel(Sources));
        for Is = 1:numel(Sources); Series{Is} = Vals(SetID == Is); end
        SLabels = SetLabels;
        Colors  = [];
    else
        Series  = {Vals};
        SLabels = {Args.Param};
        Colors  = Args.Color;
    end

    % --- Plot ----------------------------------------------------------
    Fig = figure('Name', sprintf('%s histogram', Args.Param));
    Edges = binEdges(Vals, 'Bins', Args.Bins, 'XLim', Args.XLim, 'LogX', Args.LogX);
    H = overlayHist(gca, Series, 'Edges', Edges, 'LogX', Args.LogX, ...
        'Labels', SLabels, 'Colors', Colors);
    xlabel(Args.Param, 'Interpreter', 'none');
    ylabel('Count');
    if ~isempty(Args.XLim); xlim(Args.XLim); end

    SAll = statStruct(Vals);

    if numel(Series) > 1
        legend(H(isgraphics(H)), 'Location', 'best', 'Interpreter', 'none');
        if Args.CentralEdge
            title(sprintf('%s  central vs peripheral  (N = %d)', ...
                Args.Param, SAll.N), 'Interpreter', 'none');
        else
            title(sprintf('%s  (N = %d, %d sets)', ...
                Args.Param, SAll.N, numel(Series)), 'Interpreter', 'none');
        end
    else
        title(sprintf('%s  (N = %d)', Args.Param, SAll.N), 'Interpreter', 'none');
        if Args.Stats
            i_statsOverlay(SAll);
        end
    end

    if Args.Verbose
        fprintf('plotPhotParamHist: %s  N=%d  med=%.4g  [P16 P84]=[%.4g %.4g]\n', ...
            Args.Param, SAll.N, SAll.Median, SAll.P16, SAll.P84);
    end

    % --- Assemble Result -----------------------------------------------
    Result.Values    = Vals;
    Result.CropID    = CropID;
    Result.EpochID   = EpochID;
    Result.SetID     = SetID;
    Result.IsCentral = IsCentral;
    Result.Stats     = SAll;
    Result.Param     = Args.Param;
    Result.Labels    = SLabels;
    Result.Args      = Args;
    if Args.CentralEdge
        Result.StatsCentral    = statStruct(Vals(IsCentral));
        Result.StatsPeripheral = statStruct(Vals(~IsCentral));
    end
end

% =========================================================================
function [Sources, Labels] = i_resolveSources(Input, Args)
    % Resolve Input into a cell of PC-cell sources (+ per-set labels).
    Sources = {};
    Labels  = {};

    IsPath = (ischar(Input) || isstring(Input)) || ...
             (iscell(Input) && ~isempty(Input) && ...
              all(cellfun(@(x) ischar(x) || isstring(x), Input)));

    if IsPath
        Paths = cellstr(string(Input));
        if iscell(Args.FileType)
            FT = cellfun(@char, Args.FileType, 'UniformOutput', false);
        else
            FT = {char(Args.FileType)};
        end
        NSet = numel(Paths) * numel(FT);
        for Ip = 1:numel(Paths)
            for It = 1:numel(FT)
                OutF = '';
                if ~isempty(Args.OutFile)
                    if NSet == 1
                        OutF = char(Args.OutFile);
                    else
                        [Pd, Pn, Pe] = fileparts(char(Args.OutFile));
                        OutF = fullfile(Pd, sprintf('%s_%s%s', Pn, FT{It}, Pe));
                    end
                end
                R = pipeline.last.quality.photCalib.calibrateVisits(Paths{Ip}, ...
                    'VisitGlob',    Args.VisitGlob, ...
                    'Recursive',    Args.Recursive, ...
                    'FileType',     FT{It}, ...
                    'FieldId',      Args.FieldId, ...
                    'CropID',       Args.CropID, ...
                    'CalibArgs',    Args.CalibArgs, ...
                    'OutFile',      OutF, ...
                    'ForceRecalc',  Args.ForceRecalc, ...
                    'Verbose',      Args.Verbose);
                if ~isempty(R.PC) && ~all(cellfun(@isempty, R.PC))
                    Sources{end+1} = R.PC;  %#ok<AGROW>
                    Labels{end+1}  = FT{It};%#ok<AGROW>
                end
            end
        end
        return;
    end

    % PC objects: single source, or a cell of several sources (multi-set)
    if iscell(Input) && numel(Input) > 1 && all(cellfun(@i_sourceLike, Input))
        Srcs = Input(:).';
    else
        Srcs = {Input};
    end
    for Is = 1:numel(Srcs)
        C = resolveInput(Srcs{Is});
        if ~isempty(C); Sources{end+1} = C; end %#ok<AGROW>
    end
    if numel(Sources) > 1
        if ~isempty(Args.Labels)
            Labels = Args.Labels;
        else
            Labels = arrayfun(@(K) sprintf('set %d', K), 1:numel(Sources), ...
                'UniformOutput', false);
        end
    end
end

% =========================================================================
function tf = i_sourceLike(x)
    % True when x looks like one PC source (not a bare PC array element).
    tf = (isstruct(x) && (isfield(x,'PC') || isfield(x,'MS'))) || ...
         (iscell(x) && ~isempty(x) && isa(x{1}, 'PhotCalibTrans')) || ...
         (isa(x, 'PhotCalibTrans') && ~isscalar(x));
end

% =========================================================================
function i_statsOverlay(S)
    % Median / P16 / P84 guide lines and a stats text box on current axes.
    hold on;
    YL = ylim;
    plot([S.Median S.Median], YL, 'k-',  'LineWidth', 1.4);
    plot([S.P16 S.P16],       YL, 'k--', 'LineWidth', 0.8);
    plot([S.P84 S.P84],       YL, 'k--', 'LineWidth', 0.8);
    Txt = sprintf(['N = %d\nMed = %.4g\n[P16, P84] = [%.4g, %.4g]\n', ...
                   'Mean = %.4g\nStd = %.4g'], ...
        S.N, S.Median, S.P16, S.P84, S.Mean, S.Std);
    XL = xlim;
    text(XL(1) + 0.65*(XL(2)-XL(1)), YL(1) + 0.92*(YL(2)-YL(1)), Txt, ...
        'VerticalAlignment', 'top', 'HorizontalAlignment', 'left', ...
        'BackgroundColor', [1 1 1 0.85], 'EdgeColor', 'k', ...
        'FontSize', 9, 'Interpreter', 'none');
    hold off;
end
