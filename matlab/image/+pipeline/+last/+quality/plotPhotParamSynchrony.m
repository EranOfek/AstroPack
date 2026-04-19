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
    % Output : - Result struct with per-parameter fields:
    %            .ParMat, .DeltaMat, .DiffMat
    %            .All / .Central / .Peripheral each containing:
    %              .SharedVar, .MeanCorr, .MedianCorr (state: per-epoch level)
    %              .DynSharedVar, .DynMeanCorr, .DynMedianCorr (dynamics:
    %                epoch-to-epoch change)
    %              .CorrMatrix / .DynCorrMatrix (on All only).
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

    % Split crops into central and peripheral
    CentralMask = ismember(CropsToUse, CentralCrops);
    PeriphCrops = CropsToUse(~CentralMask);
    CentralCropsUsed = CropsToUse(CentralMask);

    if Args.Verbose
        fprintf('\n=== Parameter synchrony across crops — STATE (per-epoch level) ===\n');
        fprintf('%-20s | %-36s | %-36s | %-36s\n', '', 'All crops', 'Central', 'Peripheral');
        fprintf('%-20s | %11s %11s %11s | %11s %11s %11s | %11s %11s %11s\n', ...
            'Parameter', 'ShVar(%)', 'MnCorr', 'MdCorr', ...
            'ShVar(%)', 'MnCorr', 'MdCorr', 'ShVar(%)', 'MnCorr', 'MdCorr');
        fprintf('%s\n', repmat('-', 1, 140));
    end
    DynRows = {};  % accumulate dynamics rows to print in a second table

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
        MedPerCrop = median(SubMat, 1, 'omitnan');

        % Delta from each crop's own median (shows temporal evolution
        % after removing crop-specific baseline)
        DeltaMat = SubMat - MedPerCrop;

        % Compute stats for all, central, peripheral
        [SharedVar, MeanCorr, MedianCorr, CorrMat] = syncStats(SubMat, DeltaMat);

        % Dynamics: epoch-to-epoch changes, centered by per-crop median of diffs
        DiffMat      = diff(SubMat, 1, 1);
        DiffDeltaMat = DiffMat - median(DiffMat, 1, 'omitnan');
        [DynShVar, DynMeanCorr, DynMedCorr, DynCorrMat] = syncStats(DiffMat, DiffDeltaMat);

        CentralIdx = find(CentralMask);
        PeriphIdx  = find(~CentralMask);
        if numel(CentralIdx) >= 2
            [VR_c, MC_c, MDC_c] = syncStats(SubMat(:, CentralIdx), DeltaMat(:, CentralIdx));
            [DVR_c, DMC_c, DMDC_c] = syncStats(DiffMat(:, CentralIdx), DiffDeltaMat(:, CentralIdx));
        else
            VR_c = NaN; MC_c = NaN; MDC_c = NaN;
            DVR_c = NaN; DMC_c = NaN; DMDC_c = NaN;
        end
        if numel(PeriphIdx) >= 2
            [VR_p, MC_p, MDC_p] = syncStats(SubMat(:, PeriphIdx), DeltaMat(:, PeriphIdx));
            [DVR_p, DMC_p, DMDC_p] = syncStats(DiffMat(:, PeriphIdx), DiffDeltaMat(:, PeriphIdx));
        else
            VR_p = NaN; MC_p = NaN; MDC_p = NaN;
            DVR_p = NaN; DMC_p = NaN; DMDC_p = NaN;
        end

        if Args.Verbose
            fprintf('%-20s | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f\n', ...
                PName, SharedVar, MeanCorr, MedianCorr, ...
                VR_c, MC_c, MDC_c, VR_p, MC_p, MDC_p);
            DynRows{end+1} = sprintf('%-20s | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f', ...
                PName, DynShVar, DynMeanCorr, DynMedCorr, ...
                DVR_c, DMC_c, DMDC_c, DVR_p, DMC_p, DMDC_p);
        end

        FN = matlab.lang.makeValidName(PName);
        Result.(FN).ParMat = ParMat;
        Result.(FN).DeltaMat = DeltaMat;
        Result.(FN).DiffMat  = DiffMat;
        Result.(FN).All = struct('SharedVar', SharedVar, 'MeanCorr', MeanCorr, ...
            'MedianCorr', MedianCorr, 'CorrMatrix', CorrMat, ...
            'DynSharedVar', DynShVar, 'DynMeanCorr', DynMeanCorr, ...
            'DynMedianCorr', DynMedCorr, 'DynCorrMatrix', DynCorrMat);
        Result.(FN).Central = struct('SharedVar', VR_c, 'MeanCorr', MC_c, 'MedianCorr', MDC_c, ...
            'DynSharedVar', DVR_c, 'DynMeanCorr', DMC_c, 'DynMedianCorr', DMDC_c);
        Result.(FN).Peripheral = struct('SharedVar', VR_p, 'MeanCorr', MC_p, 'MedianCorr', MDC_p, ...
            'DynSharedVar', DVR_p, 'DynMeanCorr', DMC_p, 'DynMedianCorr', DMDC_p);

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
            title(sprintf('%s deviation from cross-crop median', strrep(PName, '_', '\_')));
            text(0.02, 0.98, sprintf('ShVar: Total=%.1f%%, Central=%.1f%%, Periph=%.1f%%', ...
                    SharedVar, VR_c, VR_p), ...
                'Units', 'normalized', 'VerticalAlignment', 'top', ...
                'HorizontalAlignment', 'left', 'FontSize', 9, ...
                'BackgroundColor', [1 1 1 0.7], 'EdgeColor', [0.6 0.6 0.6]);
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
            title(sprintf('%s cross-crop correlation', strrep(PName, '_', '\_')));
        end
    end

    if Args.Verbose
        fprintf('%s\n', repmat('-', 1, 140));
        if ~isempty(DynRows)
            fprintf('\n=== Parameter synchrony across crops — DYNAMICS (epoch-to-epoch change) ===\n');
            fprintf('%-20s | %-36s | %-36s | %-36s\n', '', 'All crops', 'Central', 'Peripheral');
            fprintf('%-20s | %11s %11s %11s | %11s %11s %11s | %11s %11s %11s\n', ...
                'Parameter', 'ShVar(%)', 'MnCorr', 'MdCorr', ...
                'ShVar(%)', 'MnCorr', 'MdCorr', 'ShVar(%)', 'MnCorr', 'MdCorr');
            fprintf('%s\n', repmat('-', 1, 140));
            for Ir = 1:numel(DynRows)
                fprintf('%s\n', DynRows{Ir});
            end
            fprintf('%s\n', repmat('-', 1, 140));
        end
        fprintf('STATE: operates on deviations from each crop''s temporal median\n');
        fprintf('DYNAMICS: operates on epoch-to-epoch differences (diff along time)\n');
        fprintf('SharedVar: %% of total variance captured by the cross-crop median signal\n');
        fprintf('MeanCorr/MdCorr: mean/median off-diagonal cross-crop correlation\n');
        fprintf('Central crops: %s\n', mat2str(CentralCropsUsed));
        fprintf('Peripheral crops: %s\n', mat2str(PeriphCrops));
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
