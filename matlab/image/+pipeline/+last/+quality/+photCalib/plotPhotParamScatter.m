function Result = plotPhotParamScatter(PC, Args)
    % 2D scatter of any two named photometric-calibration quantities
    % Description: Collects one (X,Y) point per (epoch, crop) from a
    %              PhotCalibTrans array and plots a scatter diagram with
    %              Pearson correlation coefficients computed separately
    %              for central and peripheral crops. Central crops are
    %              shown as filled dots, peripheral as empty circles.
    %              Each of the two names is resolved, in order, against:
    %                (a) fitted transmission parameters (TransModel.getAllFunPar:
    %                    'TauAod500', 'PWV_cm', 'Center_Ang', 'Norm', ...)
    %                (b) fit-quality scalars on TransModel: 'RMS', 'Chi2',
    %                    'DOF', and derived 'Chi2_DOF' (= Chi2/DOF)
    %                (c) scalar properties of PhotCalibTrans (from header):
    %                    'AirMass', 'Temp', 'Pressure', 'Humidity', 'ExpTime',
    %                    'NCoadd', 'ARMS', 'DeltaZP_CB'
    %                (d) per-crop SourceData summaries: 'NCalib' (sum Used),
    %                    'NSources' (height of Table), 'MedResidual',
    %                    'StdResidual', 'MedMagErr'.
    %
    % Input  : - PC source. Either:
    %              * a single source (struct, PC array, cell-of-arrays —
    %                anything resolvePC accepts), OR
    %              * a cell of multiple such sources, e.g.
    %                {R1.PC, R2.PC, R3.PC}. In multi-set mode each set is
    %                drawn in a distinct color; central/peripheral marker
    %                shape (filled/empty) and per-set Pearson R are reported.
    %          * ...,key,val,...
    %            'XParam' - Name of the X quantity. Default is 'AirMass'.
    %            'YParam' - Name of the Y quantity. Default is 'Center_Ang'.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    %                          Determines which crop IDs are central.
    %            'MarkerSize' - Dot area. Default is 18.
    %            'FitLine' - Overlay linear fits (per central/periph in
    %                        single-set, per set in multi-set). Default is true.
    %            'ShowStats' - Show correlation annotation. Default is true.
    %            'Labels'    - Cell of names per set (multi-set only).
    %                          Default {} (auto-generated 'set 1', ...).
    %            'Verbose' - Print summary. Default is true.
    % Output : - Result struct with fields:
    %            .X, .Y, .CropIDs, .EpochIDs, .SetIDs - collected samples.
    %            .CentralMask - logical, which samples are in central crops.
    %            .Labels      - cell of set labels.
    %            .All.Pearson.R / .P / .N
    %            .Central.Pearson.R / .P / .N
    %            .Peripheral.Pearson.R / .P / .N
    %            .PerSet(Is).Pearson.R / .P / .N (multi-set only)
    %            .All.FitSlope / .FitIntercept (if FitLine true),
    %            .Central.FitSlope / .FitIntercept,
    %            .Peripheral.FitSlope / .FitIntercept,
    %            .PerSet(Is).FitSlope / .FitIntercept.
    %            .XParam, .YParam.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.photCalib.plotPhotParamScatter(R.PC);
    %          pipeline.last.quality.photCalib.plotPhotParamScatter(R.PC, ...
    %              'XParam', 'AirMass', 'YParam', 'Center_Ang');
    %          pipeline.last.quality.photCalib.plotPhotParamScatter({R1.PC, R2.PC}, ...
    %              'XParam', 'NCalib', 'YParam', 'Chi2_DOF', ...
    %              'Labels', {'field A','field B'});

    arguments
        PC
        Args.XParam         = 'AirMass'
        Args.YParam         = 'Center_Ang'
        Args.CropsToAnalyze = []
        Args.TileOrder      = 'rowmajor'
        Args.MarkerSize     = 18
        Args.FitLine logical    = true
        Args.ShowStats logical  = true
        Args.ShowLegend logical = true
        Args.LogX    logical    = false
        Args.LogY    logical    = false
        Args.XLim                = []      % [xmin xmax]; overrides auto-tight
        Args.YLim                = []      % [ymin ymax]; overrides auto-tight
        Args.Chi2Quantiles       = []      % e.g. [0.16 0.5 0.84]; overlay chi2inv(q, DOF)
        Args.Chi2DofMin          = 1       % min DOF used when drawing curves
        Args.SplitCentral logical = false  % true = colour central vs peripheral separately
        Args.DotColor    (1,3) double = [0.15 0.35 0.75]   % used when SplitCentral=false
        Args.Labels cell    = {}
        Args.Verbose logical    = true
    end

    EmptyStats = struct('Pearson', struct('R', NaN, 'P', NaN), ...
                        'FitSlope', NaN, 'FitIntercept', NaN, 'N', 0);
    Result = struct('X', [], 'Y', [], 'CropIDs', [], 'EpochIDs', [], 'SetIDs', [], ...
        'CentralMask', [], 'Labels', {{}}, ...
        'All', EmptyStats, 'Central', EmptyStats, 'Peripheral', EmptyStats, ...
        'PerSet', [], ...
        'XParam', Args.XParam, 'YParam', Args.YParam);

    % Detect multi-set input
    [Sources, IsMulti] = localSplitSources(PC);
    if IsMulti && isempty(Args.Labels)
        Args.Labels = arrayfun(@(k) sprintf('set %d', k), 1:numel(Sources), ...
            'UniformOutput', false);
    elseif ~IsMulti
        Args.Labels = {''};
    end
    Result.Labels = Args.Labels;
    Nsets = numel(Sources);

    % Central crops per tile convention
    switch lower(Args.TileOrder)
        case 'colmajor'
            CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise
            CentralCrops = [];
    end

    % Collect (X,Y) per (epoch, crop) — pooled across sets, with set ID
    Xs = []; Ys = []; Cs = []; Es = []; Ss = [];
    AnyXfinite = false;
    AnyYfinite = false;

    for Is = 1:Nsets
        PCcell = resolveInput(Sources{Is});
        if isempty(PCcell); continue; end
        for Iv = 1:numel(PCcell)
            if isempty(PCcell{Iv}); continue; end
            CropsToUse = Args.CropsToAnalyze;
            if isempty(CropsToUse)
                CropsToUse = 1:numel(PCcell{Iv});
            end
            for Ic = CropsToUse
                if Ic > numel(PCcell{Iv}); continue; end
                PCobj = PCcell{Iv}(Ic);
                if isempty(PCobj.TransModel); continue; end

                XV = resolvePCParam(PCobj, Args.XParam);
                YV = resolvePCParam(PCobj, Args.YParam);
                AnyXfinite = AnyXfinite || isfinite(XV);
                AnyYfinite = AnyYfinite || isfinite(YV);
                if ~isfinite(XV) || ~isfinite(YV); continue; end

                Xs(end+1,1) = XV;      %#ok<AGROW>
                Ys(end+1,1) = YV;      %#ok<AGROW>
                Cs(end+1,1) = Ic;      %#ok<AGROW>
                Es(end+1,1) = Iv;      %#ok<AGROW>
                Ss(end+1,1) = Is;      %#ok<AGROW>
            end
        end
    end

    if isempty(Xs)
        if ~AnyXfinite
            warning('plotPhotParamScatter:BadName', ...
                'No values resolved for XParam=''%s''. Check the name of property ''XParam''.', ...
                Args.XParam);
        end
        if ~AnyYfinite
            warning('plotPhotParamScatter:BadName', ...
                'No values resolved for YParam=''%s''. Check the name of property ''YParam''.', ...
                Args.YParam);
        end
        if AnyXfinite && AnyYfinite
            warning('plotPhotParamScatter:NoData', ...
                'No epoch/crop had finite values for both X=%s and Y=%s.', ...
                Args.XParam, Args.YParam);
        end
        return;
    end

    CentralMask = ismember(Cs, CentralCrops);

    Result.X = Xs;  Result.Y = Ys;
    Result.CropIDs = Cs;  Result.EpochIDs = Es;  Result.SetIDs = Ss;
    Result.CentralMask = CentralMask;

    Result.All        = localStats(Xs, Ys, Args.FitLine);
    Result.Central    = localStats(Xs(CentralMask),  Ys(CentralMask),  Args.FitLine);
    Result.Peripheral = localStats(Xs(~CentralMask), Ys(~CentralMask), Args.FitLine);

    % Per-set stats
    if Nsets > 1
        Result.PerSet = repmat(EmptyStats, 1, Nsets);
        for Is = 1:Nsets
            M = (Ss == Is);
            Result.PerSet(Is) = localStats(Xs(M), Ys(M), Args.FitLine);
        end
    end

    % --- Plot ---
    figure('Name', sprintf('%s vs %s', Args.YParam, Args.XParam), ...
           'Position', [80, 80, 720, 560]);
    hold on;

    XLab = strrep(Args.XParam, '_', '\_');
    YLab = strrep(Args.YParam, '_', '\_');

    Handles = gobjects(0);
    HandleNames = {};

    if Nsets == 1
        ColCentral = [0.15 0.35 0.75];
        ColPeriph  = [0.85 0.30 0.10];

        if Args.SplitCentral
            hC = scatter(Xs(CentralMask), Ys(CentralMask), Args.MarkerSize, ...
                ColCentral, 'filled', 'MarkerFaceAlpha', 0.75, ...
                'DisplayName', 'Central');
            hP = scatter(Xs(~CentralMask), Ys(~CentralMask), Args.MarkerSize, ...
                'MarkerEdgeColor', ColPeriph, 'LineWidth', 0.8, ...
                'DisplayName', 'Peripheral');
            Handles = [hC, hP];
        else
            hAll = scatter(Xs, Ys, Args.MarkerSize, ...
                Args.DotColor, 'filled', 'MarkerFaceAlpha', 0.75, ...
                'DisplayName', sprintf('All (N=%d)', numel(Xs)));
            Handles = hAll;
        end
    else
        % Multi-set: color per set, central=filled / peripheral=empty
        Colors = lines(Nsets);
        for Is = 1:Nsets
            MSet = (Ss == Is);
            McSet = MSet & CentralMask;
            MpSet = MSet & ~CentralMask;
            DispName = sprintf('%s (N=%d)', Args.Labels{Is}, sum(MSet));
            HasFilled = any(McSet);
            HasOpen   = any(MpSet);
            if HasFilled
                hF = scatter(Xs(McSet), Ys(McSet), Args.MarkerSize, ...
                    Colors(Is,:), 'filled', 'MarkerFaceAlpha', 0.75, ...
                    'DisplayName', DispName);
                Handles(end+1) = hF; %#ok<AGROW>
                HandleNames{end+1} = DispName; %#ok<AGROW>
            end
            if HasOpen
                if HasFilled
                    % Filled marker already in legend; hide open from legend
                    scatter(Xs(MpSet), Ys(MpSet), Args.MarkerSize, ...
                        'MarkerEdgeColor', Colors(Is,:), 'LineWidth', 0.8, ...
                        'HandleVisibility', 'off');
                else
                    hO = scatter(Xs(MpSet), Ys(MpSet), Args.MarkerSize, ...
                        'MarkerEdgeColor', Colors(Is,:), 'LineWidth', 0.8, ...
                        'DisplayName', DispName);
                    Handles(end+1) = hO; %#ok<AGROW>
                    HandleNames{end+1} = DispName; %#ok<AGROW>
                end
            end
        end
    end

    ColAll = [0.25 0.25 0.25];
    if Args.FitLine && Nsets == 1
        % Per-cohort fit lines only when the user asked for the split;
        % otherwise draw only the single 'All' line to match the
        % single-colour dot layout.
        if Args.SplitCentral
            if ~isnan(Result.Central.FitSlope) && any(CentralMask)
                XF = linspace(min(Xs(CentralMask)), max(Xs(CentralMask)), 50);
                plot(XF, Result.Central.FitSlope*XF + Result.Central.FitIntercept, ...
                    '-', 'Color', ColCentral, 'LineWidth', 1.5, ...
                    'HandleVisibility', 'off');
            end
            if ~isnan(Result.Peripheral.FitSlope) && any(~CentralMask)
                XF = linspace(min(Xs(~CentralMask)), max(Xs(~CentralMask)), 50);
                plot(XF, Result.Peripheral.FitSlope*XF + Result.Peripheral.FitIntercept, ...
                    '-', 'Color', ColPeriph, 'LineWidth', 1.5, ...
                    'HandleVisibility', 'off');
            end
        end
        if ~isnan(Result.All.FitSlope)
            XF = linspace(min(Xs), max(Xs), 50);
            plot(XF, Result.All.FitSlope*XF + Result.All.FitIntercept, ...
                '--', 'Color', ColAll, 'LineWidth', 1.5, ...
                'HandleVisibility', 'off');
        end
    elseif Args.FitLine && Nsets > 1
        % Per-set fit lines
        Colors = lines(Nsets);
        for Is = 1:Nsets
            S = Result.PerSet(Is);
            if ~isnan(S.FitSlope) && S.N >= 3
                MSet = (Ss == Is);
                XF = linspace(min(Xs(MSet)), max(Xs(MSet)), 50);
                plot(XF, S.FitSlope*XF + S.FitIntercept, ...
                    '-', 'Color', Colors(Is,:), 'LineWidth', 1.5, ...
                    'HandleVisibility', 'off');
            end
        end
    end

    % --- Optional chi^2(q, DOF) reference curves ------------------------
    %   Auto-detect: whichever of XParam/YParam contains 'DOF'
    %   (case-insens.) is the DOF axis; iterate over it and compute the
    %   chi^2 quantile on the opposite axis.
    if ~isempty(Args.Chi2Quantiles)
        Q = Args.Chi2Quantiles(:).';
        XIsDof = contains(Args.XParam, 'DOF', 'IgnoreCase', true);
        YIsDof = contains(Args.YParam, 'DOF', 'IgnoreCase', true);
        if any(Q <= 0 | Q >= 1)
            warning('plotPhotParamScatter:BadChi2Quantiles', ...
                'Chi2Quantiles must lie strictly in (0,1); ignoring.');
        elseif ~XIsDof && ~YIsDof
            warning('plotPhotParamScatter:NoDofAxis', ...
                ['Chi2Quantiles requires one axis to be a DOF (param name ', ...
                 'containing ''DOF''). XParam=%s, YParam=%s; skipping curves.'], ...
                Args.XParam, Args.YParam);
        else
            if XIsDof; Dof = Xs; else; Dof = Ys; end
            DofFloor = max(Args.Chi2DofMin, 1);
            DofKept  = Dof(Dof >= DofFloor);
            if ~isempty(DofKept) && max(DofKept) > min(DofKept)
                DofGrid = linspace(min(DofKept), max(DofKept), 400);
                for Iq = 1:numel(Q)
                    q   = Q(Iq);
                    Chi = chi2inv(q, DofGrid);
                    if abs(q - 0.5) < 1e-9
                        Style = '-'; LW = 2.0;
                    else
                        Style = '--'; LW = 1.0;
                    end
                    if XIsDof
                        plot(DofGrid, Chi, Style, 'Color', [0.85 0.20 0.20], ...
                            'LineWidth', LW, ...
                            'DisplayName', sprintf('chi2inv(%.2f, %s)', q, Args.XParam));
                    else
                        plot(Chi, DofGrid, Style, 'Color', [0.85 0.20 0.20], ...
                            'LineWidth', LW, ...
                            'DisplayName', sprintf('chi2inv(%.2f, %s)', q, Args.YParam));
                    end
                end
            end
        end
    end

    if Args.ShowStats
        Lines = {sprintf('N_{tot} = %d', numel(Xs))};
        if Nsets == 1
            if Result.Central.N >= 3
                Lines{end+1} = sprintf('Central:    r = %+.3f  (p=%.1e, N=%d)', ...
                    Result.Central.Pearson.R, Result.Central.Pearson.P, Result.Central.N);
            end
            if Result.Peripheral.N >= 3
                Lines{end+1} = sprintf('Periph.:    r = %+.3f  (p=%.1e, N=%d)', ...
                    Result.Peripheral.Pearson.R, Result.Peripheral.Pearson.P, Result.Peripheral.N);
            end
            if Result.All.N >= 3
                Lines{end+1} = sprintf('All crops:  r = %+.3f  (p=%.1e)', ...
                    Result.All.Pearson.R, Result.All.Pearson.P);
            end
        else
            for Is = 1:Nsets
                S = Result.PerSet(Is);
                if S.N >= 3
                    Lines{end+1} = sprintf('%-10s r = %+.3f  (p=%.1e, N=%d)', ...
                        Args.Labels{Is}, S.Pearson.R, S.Pearson.P, S.N); %#ok<AGROW>
                end
            end
            if Result.All.N >= 3
                Lines{end+1} = sprintf('All sets:  r = %+.3f  (p=%.1e, N=%d)', ...
                    Result.All.Pearson.R, Result.All.Pearson.P, Result.All.N);
            end
        end
        text(0.02, 0.98, strjoin(Lines, newline), ...
            'Units', 'normalized', 'VerticalAlignment', 'top', ...
            'HorizontalAlignment', 'left', 'FontSize', 9, ...
            'BackgroundColor', [1 1 1 0.85], 'EdgeColor', [0.6 0.6 0.6], ...
            'FontName', 'FixedWidth');
    end

    if Args.ShowLegend && ~isempty(Handles)
        legend(Handles, 'Location', 'best');
    end
    box on; grid on;
    xlabel(XLab);
    ylabel(YLab);
    title(sprintf('%s vs %s', YLab, XLab));

    if Args.LogX; set(gca, 'XScale', 'log'); end
    if Args.LogY; set(gca, 'YScale', 'log'); end

    % Tight axes: hug actual data extrema (no MATLAB nice-round padding).
    % On log axes, restrict to strictly-positive samples.
    XFin = isfinite(Xs);
    YFin = isfinite(Ys);
    if Args.LogX; XFin = XFin & Xs > 0; end
    if Args.LogY; YFin = YFin & Ys > 0; end
    if any(XFin)
        Xmn = min(Xs(XFin)); Xmx = max(Xs(XFin));
        if Xmx > Xmn; xlim([Xmn Xmx]); end
    end
    if any(YFin)
        Ymn = min(Ys(YFin)); Ymx = max(Ys(YFin));
        if Ymx > Ymn; ylim([Ymn Ymx]); end
    end

    % Explicit XLim/YLim override (final word, applied after scale + auto).
    if ~isempty(Args.XLim)
        set(gca, 'XLimMode', 'manual'); xlim(Args.XLim);
    end
    if ~isempty(Args.YLim)
        set(gca, 'YLimMode', 'manual'); ylim(Args.YLim);
    end

    if Args.Verbose
        fprintf('\n=== plotPhotParamScatter ===\n');
        fprintf('X = %s, Y = %s   (N_tot = %d, epochs = %d)\n', ...
            Args.XParam, Args.YParam, numel(Xs), numel(unique(Es)));
        fprintf('%-12s %8s %11s %9s %9s\n', 'group', 'N', 'Pearson r', 'p', 'slope');
        fprintf('%s\n', repmat('-', 1, 52));
        if Nsets == 1
            localPrint('Central',    Result.Central);
            localPrint('Peripheral', Result.Peripheral);
            localPrint('All',        Result.All);
        else
            for Is = 1:Nsets
                localPrint(Args.Labels{Is}, Result.PerSet(Is));
            end
            localPrint('All sets', Result.All);
        end
    end
end

% -------------------------------------------------------------------------
function [Sources, IsMulti] = localSplitSources(PC)
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

% -------------------------------------------------------------------------
function S = localStats(X, Y, DoFit)
    S = struct('Pearson', struct('R', NaN, 'P', NaN), ...
               'FitSlope', NaN, 'FitIntercept', NaN, 'N', numel(X));
    if numel(X) >= 3
        [R, P] = corr(X, Y, 'type', 'Pearson', 'rows', 'complete');
        S.Pearson.R = R;  S.Pearson.P = P;
    end
    if DoFit && numel(X) >= 2
        Pc = polyfit(X, Y, 1);
        S.FitSlope = Pc(1);  S.FitIntercept = Pc(2);
    end
end

% -------------------------------------------------------------------------
function localPrint(Label, S)
    if S.N >= 3
        fprintf('%-12s %8d %+11.3f %9.2e %+9.4g\n', ...
            Label, S.N, S.Pearson.R, S.Pearson.P, S.FitSlope);
    else
        fprintf('%-12s %8d   (too few samples)\n', Label, S.N);
    end
end

