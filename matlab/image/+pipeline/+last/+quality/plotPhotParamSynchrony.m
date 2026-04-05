function Result = plotPhotParamSynchrony(PC, Args)
    % Analyze synchrony of fitted parameters across crops
    % Description: For each fitted parameter, produces three outputs:
    %   (1) Delta plot: Param(Iv,Ic) - median(Param(Iv,:)) vs epoch, one line
    %       per crop. Reveals crops that deviate from the global trend.
    %   (2) Correlation heatmap: 24x24 matrix of pairwise correlations
    %       between crops across epochs.
    %   (3) Console summary: variance decomposition and mean cross-crop
    %       correlation per parameter.
    %
    % Input  : - PC struct, Result struct, PhotCalibTrans array, or cell array
    %            (any format accepted by resolvePC).
    %          * ...,key,val,...
    %            'ParamNames' - Parameter names. Default is {'TauAod500','PWV_cm','Norm'}.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    %            'PlotDelta' - Show delta plot. Default is true.
    %            'PlotHeatmap' - Show correlation heatmap. Default is true.
    %            'Verbose'   - Print numerical summary. Default is true.
    % Output : - Result struct with .(ParamName).VarianceRatio, .MeanCorr,
    %            .CorrMatrix, .ParMat for each parameter.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.plotPhotParamSynchrony(R.PC);
    %          pipeline.last.quality.plotPhotParamSynchrony(R.PC, ...
    %              'ParamNames', {'TauAod500','PWV_cm'});

    arguments
        PC
        Args.ParamNames     = {'TauAod500', 'PWV_cm', 'Center_Ang', 'Norm'}
        Args.CropsToAnalyze = []
        Args.TileOrder      = 'rowmajor'
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

    % Parameter names in model
    AllParNames = {};
    for Ic = 1:Ncrop
        if PCcell{FirstValid}(Ic).Success
            P = PCcell{FirstValid}(Ic).TransModel.getAllFunPar();
            AllParNames = P.Name;
            break;
        end
    end

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
    Result = struct();

    if Args.Verbose
        fprintf('\n=== Parameter synchrony across crops ===\n');
        fprintf('%-20s %12s %12s %12s\n', 'Parameter', 'VarRatio(%)', 'MeanCorr', 'MedianCorr');
        fprintf('%s\n', repmat('-', 1, 60));
    end

    for Ip = 1:numel(Args.ParamNames)
        PName = Args.ParamNames{Ip};
        PIdx = find(strcmp(AllParNames, PName));
        if isempty(PIdx)
            warning('plotPhotParamSynchrony:ParamNotFound', ...
                'Parameter %s not found.', PName);
            continue;
        end

        % Extract [Nvisits x Ncrop] matrix
        ParMat = nan(Nvisits, Ncrop);
        for Iv = 1:Nvisits
            if isempty(PCcell{Iv}); continue; end
            for Ic = CropsToUse
                if Ic > numel(PCcell{Iv}); continue; end
                if ~PCcell{Iv}(Ic).Success; continue; end
                P = PCcell{Iv}(Ic).TransModel.getAllFunPar();
                ParMat(Iv, Ic) = P.Val(PIdx);
            end
        end

        % Subset to analyzed crops
        SubMat = ParMat(:, CropsToUse);

        % Per-crop temporal median (one value per crop)
        MedPerCrop = nanmedian(SubMat, 1);

        % Delta from each crop's own median (shows temporal evolution
        % after removing crop-specific baseline)
        DeltaMat = SubMat - MedPerCrop;

        % Variance decomposition of the TEMPORAL signal (after removing
        % per-crop baseline). This isolates synchrony from baseline scatter.
        %   VarRatio = var(shared temporal signal) / var(total temporal signal)
        % High = crops move together in time; low = independent temporal noise.
        MedDeltaEpoch = nanmedian(DeltaMat, 2);   % shared temporal signal
        TotalVar  = nanvar(DeltaMat(:));
        MedianVar = nanvar(MedDeltaEpoch);
        VarianceRatio = 100 * MedianVar / TotalVar;

        % Cross-crop correlation matrix
        CorrMat = corr(SubMat, 'rows', 'pairwise');
        OffDiag = CorrMat(~eye(NcropUse));
        MeanCorr   = nanmean(OffDiag);
        MedianCorr = nanmedian(OffDiag);

        if Args.Verbose
            fprintf('%-20s %12.1f %12.3f %12.3f\n', PName, VarianceRatio, MeanCorr, MedianCorr);
        end

        Result.(matlab.lang.makeValidName(PName)).ParMat = ParMat;
        Result.(matlab.lang.makeValidName(PName)).VarianceRatio = VarianceRatio;
        Result.(matlab.lang.makeValidName(PName)).MeanCorr = MeanCorr;
        Result.(matlab.lang.makeValidName(PName)).MedianCorr = MedianCorr;
        Result.(matlab.lang.makeValidName(PName)).CorrMatrix = CorrMat;

        % --- (1) Delta plot ---
        if Args.PlotDelta
            figure('Name', sprintf('%s delta vs epoch', PName), ...
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
            ylabel(sprintf('%s - median', strrep(PName, '_', '\_')));
            title(sprintf('%s deviation from cross-crop median (VarRatio=%.1f%%, MeanCorr=%.2f)', ...
                strrep(PName, '_', '\_'), VarianceRatio, MeanCorr));
        end

        % --- (2) Correlation heatmap ---
        if Args.PlotHeatmap
            figure('Name', sprintf('%s cross-crop correlation', PName), ...
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
            title(sprintf('%s cross-crop correlation (mean=%.2f, median=%.2f)', ...
                strrep(PName, '_', '\_'), MeanCorr, MedianCorr));
        end
    end

    if Args.Verbose
        fprintf('%s\n', repmat('-', 1, 60));
        fprintf('VarRatio: %% of total variance in the cross-crop median (shared signal)\n');
        fprintf('MeanCorr: mean off-diagonal cross-crop correlation\n');
    end
end
