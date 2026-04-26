function Result = plotPhotShapeSynchrony(PC, Args)
    % Analyze synchrony of shape-only integral transmission across crops
    % Description: For each crop/epoch, evaluates the transmission curve
    %              with Norm=1 (excluding the overall scaling) and no Tran2D,
    %              then integrates to a scalar shape-only throughput.
    %              This isolates variations due to fitted atmospheric and
    %              spectral parameters, excluding crop-to-crop Norm differences.
    %              Produces three outputs:
    %                (1) Delta plot: value(Iv,Ic) - median(value(:,Ic)) vs epoch.
    %                (2) Cross-crop correlation heatmap.
    %                (3) Console summary: variance decomposition and correlations.
    %
    % Input  : - PC struct, Result struct, PhotCalibTrans array, or cell array
    %            (any format accepted by resolvePC).
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    %            'WvlRange'  - [min max] wavelength range [Angstrom]. Default is [] (full).
    %            'PlotDelta' - Show delta plot. Default is true.
    %            'PlotHeatmap' - Show correlation heatmap. Default is true.
    %            'Verbose'   - Print numerical summary. Default is true.
    % Output : - Result struct with .ShapeT (Nvisits x Ncrop matrix),
    %            .SharedVar, .MeanCorr, .MedianCorr, .CorrMatrix.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.photCalib.plotPhotShapeSynchrony(R.PC);
    %          pipeline.last.quality.photCalib.plotPhotShapeSynchrony(R.PC, ...
    %              'WvlRange', [4000 9000]);

    arguments
        PC
        Args.CropsToAnalyze = []
        Args.TileOrder      = 'rowmajor'
        Args.WvlRange       = []
        Args.PlotDelta logical = true
        Args.PlotHeatmap logical = true
        Args.Verbose logical = true
    end

    PCcell = pipeline.last.quality.photCalib.resolvePC(PC);
    if isempty(PCcell)
        Result = struct();
        return;
    end

    Nvisits = numel(PCcell);
    FirstValid = find(~cellfun(@isempty, PCcell), 1);
    if isempty(FirstValid); Result = struct(); return; end
    Ncrop = numel(PCcell{FirstValid});
    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Ncrop;
    end
    CropsToUse = Args.CropsToAnalyze;
    NcropUse = numel(CropsToUse);

    % Find Norm index (once)
    NormIdx = [];
    for Ic = 1:Ncrop
        if PCcell{FirstValid}(Ic).Success
            AllFunPar = PCcell{FirstValid}(Ic).TransModel.getAllFunPar();
            NormIdx = find(strcmp(AllFunPar.Name, 'Norm'));
            break;
        end
    end
    if isempty(NormIdx)
        warning('plotPhotShapeSynchrony:NoNorm', 'Norm parameter not found.');
        Result = struct();
        return;
    end

    % --- Compute shape-only integral T for each crop/epoch ---
    ShapeT = nan(Nvisits, Ncrop);
    for Iv = 1:Nvisits
        if isempty(PCcell{Iv}); continue; end
        for Ic = CropsToUse
            if Ic > numel(PCcell{Iv}); continue; end
            PCobj = PCcell{Iv}(Ic);
            if ~PCobj.Success; continue; end

            Lambda = PCobj.TransWvl;
            P = PCobj.TransModel.getAllFunPar();
            Vals = P.Val(:)';
            Vals(NormIdx) = 1;  % shape-only: Norm = 1

            % Evaluate base transmission (no Tran2D — evaluateAllFunParInput
            % doesn't apply Tran2D since it has no X/Y)
            Trans = PCobj.TransModel.evaluateAllFunParInput(Lambda, Vals);
            Trans = Trans(:);
            Lam = Lambda(:);

            if ~isempty(Args.WvlRange)
                Mask = Lam >= Args.WvlRange(1) & Lam <= Args.WvlRange(2);
                Lam = Lam(Mask);
                Trans = Trans(Mask);
            end

            if numel(Lam) < 2; continue; end
            ShapeT(Iv, Ic) = trapz(Lam, Trans) / (Lam(end) - Lam(1));
        end
    end

    % --- Analysis (mirrors plotPhotParamSynchrony) ---
    SubMat = ShapeT(:, CropsToUse);

    % Per-crop temporal median
    MedPerCrop = median(SubMat, 1, 'omitnan');

    % Delta from each crop's own median
    DeltaMat = SubMat - MedPerCrop;

    % Central crops
    switch lower(Args.TileOrder)
        case 'colmajor'
            CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise
            CentralCrops = [];
    end
    CentralMask = ismember(CropsToUse, CentralCrops);
    CentralCropsUsed = CropsToUse(CentralMask);
    PeriphCrops = CropsToUse(~CentralMask);

    % Compute stats for all, central, peripheral
    [SharedVar, MeanCorr, MedianCorr, CorrMat] = syncStats(SubMat, DeltaMat);

    CentralIdx = find(CentralMask);
    PeriphIdx  = find(~CentralMask);
    if numel(CentralIdx) >= 2
        [SV_c, MC_c, MDC_c] = syncStats(SubMat(:, CentralIdx), DeltaMat(:, CentralIdx));
    else
        SV_c = NaN; MC_c = NaN; MDC_c = NaN;
    end
    if numel(PeriphIdx) >= 2
        [SV_p, MC_p, MDC_p] = syncStats(SubMat(:, PeriphIdx), DeltaMat(:, PeriphIdx));
    else
        SV_p = NaN; MC_p = NaN; MDC_p = NaN;
    end

    if Args.Verbose
        fprintf('\n=== Shape-only integral T synchrony across crops ===\n');
        fprintf('%-20s | %-36s | %-36s | %-36s\n', '', 'All crops', 'Central', 'Peripheral');
        fprintf('%-20s | %11s %11s %11s | %11s %11s %11s | %11s %11s %11s\n', ...
            'Quantity', 'ShVar(%)', 'MnCorr', 'MdCorr', ...
            'ShVar(%)', 'MnCorr', 'MdCorr', 'ShVar(%)', 'MnCorr', 'MdCorr');
        fprintf('%s\n', repmat('-', 1, 140));
        fprintf('%-20s | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f\n', ...
            'ShapeT (Norm=1)', SharedVar, MeanCorr, MedianCorr, ...
            SV_c, MC_c, MDC_c, SV_p, MC_p, MDC_p);
        fprintf('%s\n', repmat('-', 1, 140));
        fprintf('SharedVar: %% of total variance in cross-crop median (shared signal)\n');
        fprintf('MeanCorr/MdCorr: mean/median off-diagonal cross-crop correlation\n');
        fprintf('Central crops: %s\n', mat2str(CentralCropsUsed));
        fprintf('Peripheral crops: %s\n', mat2str(PeriphCrops));
    end

    Result.ShapeT    = ShapeT;
    Result.DeltaMat  = DeltaMat;
    Result.All       = struct('SharedVar', SharedVar, 'MeanCorr', MeanCorr, ...
        'MedianCorr', MedianCorr, 'CorrMatrix', CorrMat);
    Result.Central   = struct('SharedVar', SV_c, 'MeanCorr', MC_c, 'MedianCorr', MDC_c);
    Result.Peripheral = struct('SharedVar', SV_p, 'MeanCorr', MC_p, 'MedianCorr', MDC_p);

    EpochVec = 1:Nvisits;
    Cmap = lines(NcropUse);

    % --- (1) Delta plot ---
    if Args.PlotDelta
        figure('Name', 'Shape-only integral T delta vs epoch', ...
               'Position', [50, 50, 800, 450]);
        hold on;
        for Iic = 1:NcropUse
            Ic = CropsToUse(Iic);
            if ismember(Ic, CentralCrops)
                plot(EpochVec, DeltaMat(:, Iic), '-', 'LineWidth', 1.5, ...
                    'Color', [Cmap(Iic,:) 0.7]);
            else
                plot(EpochVec, DeltaMat(:, Iic), '--', 'LineWidth', 0.5, ...
                    'Color', [Cmap(Iic,:) 0.3]);
            end
        end
        plot(EpochVec, zeros(size(EpochVec)), '-k', 'LineWidth', 2);
        box on; grid on;
        xlabel('Epoch');
        ylabel('Shape-only Integral T - crop median');
        title(sprintf('Shape-only integral T (Norm=1) deviation (SharedVar=%.1f%%, MeanCorr=%.2f)', ...
            SharedVar, MeanCorr));
    end

    % --- (2) Correlation heatmap ---
    if Args.PlotHeatmap
        figure('Name', 'Shape-only integral T cross-crop correlation', ...
               'Position', [50, 50, 600, 550]);
        imagesc(CorrMat);
        colorbar;
        colormap(jet);
        caxis([-1 1]);
        axis equal tight;
        xlabel('Crop ID');
        ylabel('Crop ID');
        set(gca, 'XTick', 1:NcropUse, 'XTickLabel', CropsToUse, ...
                 'YTick', 1:NcropUse, 'YTickLabel', CropsToUse);
        title(sprintf('Shape-only integral T cross-crop correlation (mean=%.2f, median=%.2f)', ...
            MeanCorr, MedianCorr));
    end
end

function [SharedVar, MeanCorr, MedianCorr, CorrMat] = syncStats(SubMat, DeltaMat)
    MedDeltaEpoch = median(DeltaMat, 2, 'omitnan');
    TotalVar  = var(DeltaMat(:), 'omitnan');
    MedianVar = var(MedDeltaEpoch, 'omitnan');
    if TotalVar > 0
        SharedVar = 100 * MedianVar / TotalVar;
    else
        SharedVar = NaN;
    end
    Nc = size(SubMat, 2);
    CorrMat = corr(SubMat, 'rows', 'pairwise');
    OffDiag = CorrMat(~eye(Nc));
    MeanCorr   = mean(OffDiag, 'omitnan');
    MedianCorr = median(OffDiag, 'omitnan');
end
