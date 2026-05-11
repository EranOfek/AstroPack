function Result = plotPhotParamCentralEdgeHist(DataPath, Args)
    % Central vs peripheral histogram of one PC quantity, optionally
    % centered per-visit, pooled over many calibrated visits.
    % Description: Walks visit subdirectories under DataPath (one level or
    %              recursively, like plotHeaderScatter / plotPhotHistogram),
    %              calibrates each visit via batchPhotCalib to obtain
    %              per-crop PhotCalibTrans objects, then for each visit:
    %                - resolves the named Param per crop (1xNcrop vector),
    %                - optionally subtracts the per-visit median (or mean)
    %                  so different visits become comparable on the same
    %                  X axis,
    %                - splits the per-crop values into central and
    %                  peripheral groups by crop ID (TileOrder).
    %              Finally pools the values across all visits and plots
    %              two overlaid histograms (central + peripheral) on the
    %              same bin grid, using the same greyscale styling as
    %              plotPhotParamMosaic's histogram row (peripheral light
    %              grey with bold black edge, central dark grey).
    %
    %              For Center_Ang specifically, note that the header
    %              keyword is PT_8_V2 — but plotPhotParamHist /
    %              resolvePCParam access it via the fitted-parameter
    %              registry, so 'Center_Ang' just works.
    %
    % Input  : - DataPath char/string. Parent directory containing visit
    %            subdirectories (or '' when loading from PCFile).
    %          * ...,key,val,...
    %            'Param'        - PC quantity name. Default 'Center_Ang'.
    %                             Resolved via resolvePCParam.
    %            'VisitPattern' - Glob for visit subdirs.
    %                             Default '*v[0-9]*'.
    %            'Recursive'    - Search at any depth. Default true.
    %            'FileType'     - 'coadd' or 'proc'. Default 'coadd'.
    %            'CalibArgs'    - Cell of NV pairs forwarded to
    %                             fitPhotCalibTrans. Default {}.
    %            'PCFile'       - .mat path with a cached batch struct
    %                             (R.PC, R.VisitName, R.AIIndex). When
    %                             non-empty, skips disk walk + calibration.
    %                             Default ''.
    %            'CacheFile'    - .mat path to save the freshly computed
    %                             batch. Ignored when PCFile is set.
    %                             Default ''.
    %            'Center'       - Per-visit centering applied before
    %                             pooling: 'median' (default), 'mean',
    %                             or 'none'. With 'median' the X axis
    %                             becomes Param - med_visit(Param).
    %            'TileOrder'    - 'rowmajor' (default) or 'colmajor';
    %                             determines which crop IDs are central.
    %            'Ncrop'        - Crops per visit. Default 24.
    %            'Bins'         - Histogram bin count or edge vector.
    %                             Default 30.
    %            'XLim'         - X limits. Default [] (auto).
    %            'HistColors'   - 2x3 [central; peripheral] face colors.
    %                             Default [0.40 0.40 0.40; 0.85 0.85 0.85].
    %            'HistEdgeWidth' - Bold edge width on peripheral bars.
    %                             Default 2.4.
    %            'ShowTitle'    - Auto title. Default true.
    %            'Verbose'      - Print progress. Default false.
    % Output : - Result struct with fields:
    %            .ValuesCentral, .ValuesPeripheral - pooled value vectors
    %                          (after per-visit centering).
    %            .StatsCentral, .StatsPeripheral - structs with .N, .Mean,
    %                          .Median, .Std, .P16, .P84.
    %            .Param, .Center, .TileOrder - echoed.
    %            .Batch        - struct returned by batchPhotCalib (or
    %                          loaded from PCFile).
    % Author : D. Kovaleva (May 2026)
    % Example: % Center_Ang offset from per-visit median, central vs
    %          % peripheral, all v1 visits across 2025:
    %          R = pipeline.last.quality.photCalib.plotPhotParamCentralEdgeHist( ...
    %                  '/archimedes/LASTunitTest/2025', ...
    %                  'VisitPattern', '*v1', 'Param', 'Center_Ang', ...
    %                  'CacheFile', '~/results/2025_v1_PC.mat', ...
    %                  'Verbose', true);
    %
    %          % Replot from cache without recomputing:
    %          R = pipeline.last.quality.photCalib.plotPhotParamCentralEdgeHist( ...
    %                  '', 'PCFile', '~/results/2025_v1_PC.mat', ...
    %                  'Param', 'Center_Ang');

    arguments
        DataPath                          = ''
        Args.Param          (1,:) char    = 'Center_Ang'
        Args.VisitPattern   {mustBeText}  = '*v[0-9]*'
        Args.Recursive      logical       = true
        Args.FileType       {mustBeMember(Args.FileType, {'coadd','proc'})} = 'coadd'
        Args.CalibArgs      cell          = {}
        Args.PCFile         {mustBeText}  = ''
        Args.CacheFile      {mustBeText}  = ''
        Args.Center         (1,:) char ...
            {mustBeMember(Args.Center, {'median','mean','none'})} = 'median'
        Args.TileOrder      (1,:) char    = 'rowmajor'
        Args.Ncrop                         = 24
        Args.Bins                          = 30
        Args.XLim                          = []
        Args.HistColors                    = [0.40 0.40 0.40; 0.85 0.85 0.85]
        Args.HistEdgeWidth                 = 2.4
        Args.ShowTitle      logical       = true
        Args.Verbose        logical       = false
    end

    % --- Obtain the batch -----------------------------------------------
    if ~isempty(Args.PCFile)
        if Args.Verbose
            fprintf('Loading cached batch from %s\n', Args.PCFile);
        end
        S = load(char(Args.PCFile), 'R');
        if ~isfield(S, 'R') || ~isfield(S.R, 'PC')
            error('plotPhotParamCentralEdgeHist:BadCache', ...
                'File %s does not contain an R struct with field PC.', Args.PCFile);
        end
        Batch = S.R;
    else
        if isempty(DataPath)
            error('plotPhotParamCentralEdgeHist:NoInput', ...
                'Provide either DataPath or PCFile.');
        end
        Batch = pipeline.last.quality.photCalib.batchPhotCalib( ...
            char(DataPath), ...
            'VisitGlob',  Args.VisitPattern, ...
            'Recursive',  Args.Recursive, ...
            'FileType',   Args.FileType, ...
            'CalibArgs',  Args.CalibArgs, ...
            'OutFile',    Args.CacheFile, ...
            'Verbose',    Args.Verbose);
    end

    if isempty(Batch.PC)
        error('plotPhotParamCentralEdgeHist:EmptyPC', ...
            'No PhotCalibTrans objects available.');
    end

    % --- Central crop set -----------------------------------------------
    switch lower(Args.TileOrder)
        case 'colmajor', CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor', CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise,        CentralCrops = [];
    end

    % --- Group by visit, resolve Param, center, split -------------------
    if isfield(Batch, 'VisitName') && ~isempty(Batch.VisitName)
        [VisitList, ~, VG] = unique(Batch.VisitName, 'stable');
    else
        VisitList = {'<single>'};
        VG = ones(1, numel(Batch.PC));
    end
    Nvisits = numel(VisitList);

    ValsC = [];
    ValsP = [];
    NSkip = 0;

    for Iv = 1:Nvisits
        Mask = (VG == Iv);
        PCv  = Batch.PC(Mask);
        if isfield(Batch, 'AIIndex') && ~isempty(Batch.AIIndex)
            CropIds = Batch.AIIndex(Mask);
        else
            CropIds = 1:numel(PCv);
        end

        % Resolve Param per crop in this visit
        V = nan(numel(PCv), 1);
        for K = 1:numel(PCv)
            if ~PCv(K).Success; continue; end
            V(K) = pipeline.last.quality.photCalib.resolvePCParam(PCv(K), Args.Param);
        end

        % Per-visit centering
        Vfin = isfinite(V);
        if nnz(Vfin) < 2
            NSkip = NSkip + 1;
            continue;
        end
        switch Args.Center
            case 'median', Cv = median(V(Vfin));
            case 'mean',   Cv = mean(V(Vfin));
            case 'none',   Cv = 0;
        end
        Vc = V - Cv;

        % Central / peripheral split by crop ID
        IsCentral = ismember(CropIds(:), CentralCrops);
        ValsC = [ValsC; Vc(IsCentral & Vfin)]; %#ok<AGROW>
        ValsP = [ValsP; Vc(~IsCentral & Vfin)]; %#ok<AGROW>
    end

    if isempty(ValsC) && isempty(ValsP)
        error('plotPhotParamCentralEdgeHist:Empty', ...
            'No finite %s values pooled across visits.', Args.Param);
    end

    % --- Stats ----------------------------------------------------------
    StatsCentral    = makeStats(ValsC);
    StatsPeripheral = makeStats(ValsP);

    if Args.Verbose
        fprintf('plotPhotParamCentralEdgeHist: Param=%s, Center=%s\n', ...
            Args.Param, Args.Center);
        fprintf('  visits used = %d  (%d skipped)\n', Nvisits - NSkip, NSkip);
        fprintf('  central     : N=%d  med=%+.4g  std=%.4g\n', ...
            StatsCentral.N, StatsCentral.Median, StatsCentral.Std);
        fprintf('  peripheral  : N=%d  med=%+.4g  std=%.4g\n', ...
            StatsPeripheral.N, StatsPeripheral.Median, StatsPeripheral.Std);
    end

    % --- Plot -----------------------------------------------------------
    figure('Name', sprintf('%s central vs peripheral (centered: %s)', ...
        Args.Param, Args.Center));
    hold on;
    All = [ValsC; ValsP];
    if isscalar(Args.Bins)
        if isempty(Args.XLim)
            Edges = linspace(min(All), max(All), Args.Bins + 1);
        else
            Edges = linspace(Args.XLim(1), Args.XLim(2), Args.Bins + 1);
        end
    else
        Edges = Args.Bins;
    end

    ColCen = Args.HistColors(1, :);
    ColPer = Args.HistColors(2, :);
    HP = histogram(ValsP, Edges, ...
        'FaceColor', ColPer, 'FaceAlpha', 1.0, ...
        'EdgeColor', 'k', 'LineWidth', Args.HistEdgeWidth, ...
        'DisplayName', sprintf('peripheral (N=%d)', StatsPeripheral.N));
    HC = histogram(ValsC, Edges, ...
        'FaceColor', ColCen, 'FaceAlpha', 0.85, ...
        'EdgeColor', ColCen * 0.6, 'LineWidth', 0.5, ...
        'DisplayName', sprintf('central (N=%d)', StatsCentral.N));
    grid on; box on;
    if ~isempty(Args.XLim); xlim(Args.XLim); end

    switch Args.Center
        case 'median', XLab = sprintf('%s - med_visit(%s)', Args.Param, Args.Param);
        case 'mean',   XLab = sprintf('%s - mean_visit(%s)', Args.Param, Args.Param);
        case 'none',   XLab = Args.Param;
    end
    xlabel(XLab, 'Interpreter', 'none');
    ylabel('Count');
    legend([HC, HP], 'Location', 'best', 'Interpreter', 'none');

    if Args.ShowTitle
        title(sprintf('%s   central med=%+.4g  peripheral med=%+.4g  (%d visits)', ...
            Args.Param, StatsCentral.Median, StatsPeripheral.Median, ...
            Nvisits - NSkip), 'Interpreter', 'none');
    end
    hold off;

    % --- Output ---------------------------------------------------------
    Result.ValuesCentral    = ValsC;
    Result.ValuesPeripheral = ValsP;
    Result.StatsCentral     = StatsCentral;
    Result.StatsPeripheral  = StatsPeripheral;
    Result.Param            = Args.Param;
    Result.Center           = Args.Center;
    Result.TileOrder        = Args.TileOrder;
    Result.Batch            = Batch;
end

% =========================================================================
function S = makeStats(V)
    if isempty(V)
        S = struct('N',0,'Mean',NaN,'Median',NaN,'Std',NaN,'P16',NaN,'P84',NaN);
    else
        S.N      = numel(V);
        S.Mean   = mean(V);
        S.Median = median(V);
        S.Std    = std(V);
        S.P16    = prctile(V, 16);
        S.P84    = prctile(V, 84);
    end
end
