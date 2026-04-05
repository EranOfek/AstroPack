function plotPhotParamCorr(PC, Args)
    % Plot correlation matrix of fitted transmission parameters
    % Description: Extracts fitted parameter values from PhotCalibTrans
    %              objects across all epochs and crops. Plots scatter matrix
    %              (parameter vs parameter) with histograms on the diagonal.
    %              Optionally color-codes by crop ID.
    %
    % Input  : - PC struct, Result struct, PhotCalibTrans array, or cell array.
    %            (any format accepted by resolvePC).
    %          * ...,key,val,...
    %            'ParamNames' - Cell array of parameter names to include.
    %                          Default is {'TauAod500','PWV_cm','Norm'}.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'ColorByCrop' - Color points by crop ID. Default is false.
    %            'MarkerSize'  - Dot size. Default is 4.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.plotPhotParamCorr(R.PC);
    %          pipeline.last.quality.plotPhotParamCorr(R.PC, ...
    %              'ParamNames', {'TauAod500','PWV_cm','Norm','QE_Center_Ang'});
    %          pipeline.last.quality.plotPhotParamCorr(R.PC, 'ColorByCrop', true);

    arguments
        PC
        Args.ParamNames     = {'TauAod500', 'PWV_cm', 'Center_Ang', 'Norm'}
        Args.CropsToAnalyze = []
        Args.ColorByCrop logical = false
        Args.MarkerSize     = 4
    end

    PCcell = pipeline.last.quality.resolvePC(PC);
    if isempty(PCcell)
        return;
    end

    Nvisits = numel(PCcell);
    Npar = numel(Args.ParamNames);

    % Get all parameter names from first successful PC
    FirstValid = find(~cellfun(@isempty, PCcell), 1);
    if isempty(FirstValid); return; end
    Ncrop = numel(PCcell{FirstValid});
    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Ncrop;
    end

    AllParNames = {};
    for Ic = 1:Ncrop
        if PCcell{FirstValid}(Ic).Success
            P = PCcell{FirstValid}(Ic).TransModel.getAllFunPar();
            AllParNames = P.Name;
            break;
        end
    end
    if isempty(AllParNames); return; end

    % Find indices for requested parameters
    ParIdx = zeros(Npar, 1);
    for Ip = 1:Npar
        Idx = find(strcmp(AllParNames, Args.ParamNames{Ip}));
        if isempty(Idx)
            warning('plotPhotParamCorr:ParamNotFound', ...
                'Parameter %s not found. Available: %s', ...
                Args.ParamNames{Ip}, strjoin(AllParNames, ', '));
            return;
        end
        ParIdx(Ip) = Idx;
    end

    % Extract parameter values: [Npoints x Npar] + crop IDs
    ParData  = [];
    CropIDs  = [];

    for Iv = 1:Nvisits
        if isempty(PCcell{Iv}); continue; end
        for Ic = Args.CropsToAnalyze
            if Ic > numel(PCcell{Iv}); continue; end
            if ~PCcell{Iv}(Ic).Success; continue; end
            P = PCcell{Iv}(Ic).TransModel.getAllFunPar();
            Vals = P.Val(ParIdx)';
            if all(isfinite(Vals))
                ParData = [ParData; Vals];
                CropIDs = [CropIDs; Ic];
            end
        end
    end

    if isempty(ParData)
        warning('plotPhotParamCorr:NoData', 'No valid parameter data found.');
        return;
    end

    % --- Plot ---
    figure('Name', 'Fitted Parameter Correlations', ...
           'Position', [50, 50, 200*Npar, 200*Npar]);

    % Parameter labels
    ParLabels = cellfun(@(S) strrep(S, '_', '\_'), Args.ParamNames, 'UniformOutput', false);

    if Args.ColorByCrop
        UniqueCrops = unique(CropIDs);
        Cmap = lines(numel(UniqueCrops));
        CropColorIdx = zeros(size(CropIDs));
        for Iu = 1:numel(UniqueCrops)
            CropColorIdx(CropIDs == UniqueCrops(Iu)) = Iu;
        end
        DotColors = Cmap(CropColorIdx, :);
    end

    for Irow = 1:Npar
        for Icol = 1:Npar
            subplot(Npar, Npar, (Irow-1)*Npar + Icol);

            if Irow == Icol
                % Diagonal: histogram
                histogram(ParData(:, Irow), 30, 'FaceColor', [0.5 0.5 0.5]);
                if Irow == 1
                    title(ParLabels{Icol});
                end
            else
                % Off-diagonal: scatter
                if Args.ColorByCrop
                    scatter(ParData(:, Icol), ParData(:, Irow), ...
                        Args.MarkerSize, DotColors, 'filled', 'MarkerFaceAlpha', 0.5);
                else
                    plot(ParData(:, Icol), ParData(:, Irow), '.', ...
                        'MarkerSize', Args.MarkerSize, 'Color', [0.3 0.5 0.8]);
                end

                % Correlation coefficient
                Valid = isfinite(ParData(:, Icol)) & isfinite(ParData(:, Irow));
                if sum(Valid) > 3
                    Rho = corr(ParData(Valid, Icol), ParData(Valid, Irow));
                    text(0.05, 0.95, sprintf('r=%.2f', Rho), ...
                        'Units', 'normalized', 'VerticalAlignment', 'top', ...
                        'FontSize', 7, 'BackgroundColor', 'w');
                end
            end

            % Axis labels on edges only
            if Irow == Npar
                xlabel(ParLabels{Icol}, 'FontSize', 8);
            else
                set(gca, 'XTickLabel', []);
            end
            if Icol == 1 && Irow ~= Icol
                ylabel(ParLabels{Irow}, 'FontSize', 8);
            else
                set(gca, 'YTickLabel', []);
            end

            box on;
        end
    end

    sgtitle('Fitted parameter correlations', 'FontSize', 11);
end
