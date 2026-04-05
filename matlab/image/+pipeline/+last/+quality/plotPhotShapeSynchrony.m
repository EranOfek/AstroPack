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
    %            .VarianceRatio, .MeanCorr, .MedianCorr, .CorrMatrix.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.plotPhotShapeSynchrony(R.PC);
    %          pipeline.last.quality.plotPhotShapeSynchrony(R.PC, ...
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

    PCcell = pipeline.last.quality.resolvePC(PC);
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
    MedPerCrop = nanmedian(SubMat, 1);

    % Delta from each crop's own median
    DeltaMat = SubMat - MedPerCrop;

    % Variance decomposition of the TEMPORAL signal (after removing
    % per-crop baseline). Isolates synchrony from baseline scatter.
    MedDeltaEpoch = nanmedian(DeltaMat, 2);
    TotalVar    = nanvar(DeltaMat(:));
    MedianVar   = nanvar(MedDeltaEpoch);
    VarianceRatio = 100 * MedianVar / TotalVar;

    % Cross-crop correlation
    CorrMat = corr(SubMat, 'rows', 'pairwise');
    OffDiag = CorrMat(~eye(NcropUse));
    MeanCorr   = nanmean(OffDiag);
    MedianCorr = nanmedian(OffDiag);

    if Args.Verbose
        fprintf('\n=== Shape-only integral T synchrony across crops ===\n');
        fprintf('%-20s %12s %12s %12s\n', 'Quantity', 'VarRatio(%)', 'MeanCorr', 'MedianCorr');
        fprintf('%s\n', repmat('-', 1, 60));
        fprintf('%-20s %12.1f %12.3f %12.3f\n', ...
            'ShapeT (Norm=1)', VarianceRatio, MeanCorr, MedianCorr);
        fprintf('%s\n', repmat('-', 1, 60));
        fprintf('VarRatio: %% of total variance in cross-crop median (shared signal)\n');
        fprintf('MeanCorr: mean off-diagonal cross-crop correlation\n');
    end

    Result.ShapeT        = ShapeT;
    Result.VarianceRatio = VarianceRatio;
    Result.MeanCorr      = MeanCorr;
    Result.MedianCorr    = MedianCorr;
    Result.CorrMatrix    = CorrMat;

    % Central crops
    switch lower(Args.TileOrder)
        case 'colmajor'
            CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise
            CentralCrops = [];
    end

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
        title(sprintf('Shape-only integral T (Norm=1) deviation (VarRatio=%.1f%%, MeanCorr=%.2f)', ...
            VarianceRatio, MeanCorr));
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
