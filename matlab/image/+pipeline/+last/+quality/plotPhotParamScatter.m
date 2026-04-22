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
    %                (b) fit-quality scalars on TransModel: 'RMS', 'Chi2', 'DOF'
    %                (c) scalar properties of PhotCalibTrans (from header):
    %                    'AirMass', 'Temp', 'Pressure', 'Humidity', 'ExpTime',
    %                    'NCoadd', 'ARMS', 'DeltaZP_CB'
    %                (d) per-crop SourceData summaries: 'NCalib' (sum Used),
    %                    'NSources' (height of Table), 'MedResidual',
    %                    'StdResidual', 'MedMagErr'.
    %
    % Input  : - PC struct, Result struct, PhotCalibTrans array, or cell array
    %            (any format accepted by resolvePC).
    %          * ...,key,val,...
    %            'XParam' - Name of the X quantity. Default is 'AirMass'.
    %            'YParam' - Name of the Y quantity. Default is 'Center_Ang'.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    %                          Determines which crop IDs are central.
    %            'MarkerSize' - Dot area. Default is 18.
    %            'FitLine' - Overlay linear fits (separate central/periph).
    %                        Default is true.
    %            'ShowStats' - Show correlation annotation. Default is true.
    %            'Verbose' - Print summary. Default is true.
    % Output : - Result struct with fields:
    %            .X, .Y, .CropIDs, .EpochIDs - collected samples.
    %            .CentralMask - logical, which samples are in central crops.
    %            .All.Pearson.R / .P / .N
    %            .Central.Pearson.R / .P / .N
    %            .Peripheral.Pearson.R / .P / .N
    %            .All.FitSlope / .FitIntercept (if FitLine true),
    %            .Central.FitSlope / .FitIntercept,
    %            .Peripheral.FitSlope / .FitIntercept.
    %            .XParam, .YParam.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.plotPhotParamScatter(R.PC);
    %          pipeline.last.quality.plotPhotParamScatter(R.PC, ...
    %              'XParam', 'AirMass', 'YParam', 'Center_Ang');
    %          pipeline.last.quality.plotPhotParamScatter(R.PC, ...
    %              'XParam', 'TauAod500', 'YParam', 'RMS');

    arguments
        PC
        Args.XParam         = 'AirMass'
        Args.YParam         = 'Center_Ang'
        Args.CropsToAnalyze = []
        Args.TileOrder      = 'rowmajor'
        Args.MarkerSize     = 18
        Args.FitLine logical    = true
        Args.ShowStats logical  = true
        Args.Verbose logical    = true
    end

    EmptyStats = struct('Pearson', struct('R', NaN, 'P', NaN), ...
                        'FitSlope', NaN, 'FitIntercept', NaN, 'N', 0);
    Result = struct('X', [], 'Y', [], 'CropIDs', [], 'EpochIDs', [], ...
        'CentralMask', [], ...
        'All', EmptyStats, 'Central', EmptyStats, 'Peripheral', EmptyStats, ...
        'XParam', Args.XParam, 'YParam', Args.YParam);

    PCcell = pipeline.last.quality.resolvePC(PC);
    if isempty(PCcell); return; end

    % Central crops per tile convention
    switch lower(Args.TileOrder)
        case 'colmajor'
            CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise
            CentralCrops = [];
    end

    Nvisits = numel(PCcell);

    % Collect (X,Y) per (epoch, crop)
    Xs = [];
    Ys = [];
    Cs = [];
    Es = [];
    AnyXfinite = false;
    AnyYfinite = false;
    for Iv = 1:Nvisits
        if isempty(PCcell{Iv}); continue; end
        CropsToUse = Args.CropsToAnalyze;
        if isempty(CropsToUse)
            CropsToUse = 1:numel(PCcell{Iv});
        end
        for Ic = CropsToUse
            if Ic > numel(PCcell{Iv}); continue; end
            PCobj = PCcell{Iv}(Ic);
            if ~PCobj.Success; continue; end

            XV = localLookup(PCobj, Args.XParam);
            YV = localLookup(PCobj, Args.YParam);
            AnyXfinite = AnyXfinite || isfinite(XV);
            AnyYfinite = AnyYfinite || isfinite(YV);
            if ~isfinite(XV) || ~isfinite(YV); continue; end

            Xs(end+1,1) = XV;      %#ok<AGROW>
            Ys(end+1,1) = YV;      %#ok<AGROW>
            Cs(end+1,1) = Ic;      %#ok<AGROW>
            Es(end+1,1) = Iv;      %#ok<AGROW>
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
    Result.CropIDs = Cs;  Result.EpochIDs = Es;
    Result.CentralMask = CentralMask;

    Result.All        = localStats(Xs, Ys, Args.FitLine);
    Result.Central    = localStats(Xs(CentralMask),  Ys(CentralMask),  Args.FitLine);
    Result.Peripheral = localStats(Xs(~CentralMask), Ys(~CentralMask), Args.FitLine);

    % --- Plot ---
    figure('Name', sprintf('%s vs %s', Args.YParam, Args.XParam), ...
           'Position', [80, 80, 720, 560]);
    hold on;

    XLab = strrep(Args.XParam, '_', '\_');
    YLab = strrep(Args.YParam, '_', '\_');

    ColCentral = [0.15 0.35 0.75];
    ColPeriph  = [0.85 0.30 0.10];

    hC = scatter(Xs(CentralMask), Ys(CentralMask), Args.MarkerSize, ...
        ColCentral, 'filled', 'MarkerFaceAlpha', 0.75, ...
        'DisplayName', 'Central');
    hP = scatter(Xs(~CentralMask), Ys(~CentralMask), Args.MarkerSize, ...
        'MarkerEdgeColor', ColPeriph, 'LineWidth', 0.8, ...
        'DisplayName', 'Peripheral');

    ColAll = [0.25 0.25 0.25];
    if Args.FitLine
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
        if ~isnan(Result.All.FitSlope)
            XF = linspace(min(Xs), max(Xs), 50);
            plot(XF, Result.All.FitSlope*XF + Result.All.FitIntercept, ...
                '--', 'Color', ColAll, 'LineWidth', 1.5, ...
                'HandleVisibility', 'off');
        end
    end

    if Args.ShowStats
        Lines = {sprintf('N_{tot} = %d', numel(Xs))};
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
        text(0.02, 0.98, strjoin(Lines, newline), ...
            'Units', 'normalized', 'VerticalAlignment', 'top', ...
            'HorizontalAlignment', 'left', 'FontSize', 9, ...
            'BackgroundColor', [1 1 1 0.85], 'EdgeColor', [0.6 0.6 0.6], ...
            'FontName', 'FixedWidth');
    end

    legend([hC, hP], 'Location', 'best');
    box on; grid on;
    xlabel(XLab);
    ylabel(YLab);
    title(sprintf('%s vs %s', YLab, XLab));

    if Args.Verbose
        fprintf('\n=== plotPhotParamScatter ===\n');
        fprintf('X = %s, Y = %s   (N_tot = %d, epochs = %d)\n', ...
            Args.XParam, Args.YParam, numel(Xs), numel(unique(Es)));
        fprintf('%-12s %8s %11s %9s %9s\n', 'group', 'N', 'Pearson r', 'p', 'slope');
        fprintf('%s\n', repmat('-', 1, 52));
        localPrint('Central',    Result.Central);
        localPrint('Peripheral', Result.Peripheral);
        localPrint('All',        Result.All);
    end
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

% -------------------------------------------------------------------------
function V = localLookup(PCobj, Name)
    % Resolve a named quantity from a single PhotCalibTrans object.
    V = NaN;

    % (a) fitted transmission parameter
    if ~isempty(PCobj.TransModel)
        try
            P = PCobj.TransModel.getAllFunPar();
            Idx = find(strcmp(P.Name, Name), 1);
            if ~isempty(Idx)
                V = P.Val(Idx);
                return;
            end
        catch
        end

        % (b) TransModel scalar fit-quality fields
        if ismember(Name, {'RMS', 'Chi2', 'DOF'}) && isprop(PCobj.TransModel, Name)
            V = PCobj.TransModel.(Name);
            return;
        end
    end

    % (c) PhotCalibTrans scalar properties (header-derived or metadata)
    ScalarProps = {'AirMass','Temp','Pressure','Humidity','ExpTime', ...
                   'NCoadd','ARMS','DeltaZP_CB','Aperture'};
    if ismember(Name, ScalarProps) && isprop(PCobj, Name)
        Tmp = PCobj.(Name);
        if isnumeric(Tmp) && isscalar(Tmp)
            V = Tmp;
        end
        return;
    end

    % (d) per-crop SourceData summaries
    if ~isempty(PCobj.SourceData) && isprop(PCobj.SourceData, 'Catalog')
        Tab = PCobj.SourceData.Table;
        if ~isempty(Tab)
            Cols = Tab.Properties.VariableNames;
            switch Name
                case 'NSources'
                    V = height(Tab);
                case 'NCalib'
                    if ismember('Used', Cols)
                        V = sum(logical(Tab.Used));
                    else
                        V = height(Tab);
                    end
                case 'MedResidual'
                    if ismember('Residuals', Cols)
                        R = Tab.Residuals;
                        if ismember('Used', Cols); R = R(logical(Tab.Used)); end
                        V = median(R, 'omitnan');
                    end
                case 'StdResidual'
                    if ismember('Residuals', Cols)
                        R = Tab.Residuals;
                        if ismember('Used', Cols); R = R(logical(Tab.Used)); end
                        V = std(R, 0, 'omitnan');
                    end
                case 'MedMagErr'
                    if ismember('MagErr', Cols)
                        V = median(Tab.MagErr, 'omitnan');
                    end
            end
        end
    end
end
