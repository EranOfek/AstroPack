function plotPhotFittedParams(PC, Args)
    % Plot evolution of fitted transmission parameters vs epoch
    % Description: Extracts fitted parameter values from PhotCalibTrans
    %              objects across epochs and crops. Plots per-crop lines
    %              (bold for central, dashed for edge) with thick median.
    %              One figure per parameter.
    %
    % Input  : - PC struct, Result struct, PhotCalibTrans array, or cell array.
    %            (any format accepted by resolvePC).
    %          * ...,key,val,...
    %            'ParamNames' - Cell array of parameter names to plot.
    %                          Default is {'TauAod500','PWV_cm','Norm'}.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.plotPhotFittedParams(R.PC);
    %          pipeline.last.quality.plotPhotFittedParams(R, ...
    %              'ParamNames', {'TauAod500','PWV_cm','Norm'});
    %          pipeline.last.quality.plotPhotFittedParams(R.PC, ...
    %              'ParamNames', {'TauAod500'}, 'TileOrder', 'colmajor');

    arguments
        PC
        Args.ParamNames     = {'TauAod500', 'PWV_cm', 'Norm'}
        Args.CropsToAnalyze = []
        Args.TileOrder      = 'rowmajor'
    end

    PCcell = pipeline.last.quality.resolvePC(PC);
    if isempty(PCcell)
        return;
    end

    Nvisits = numel(PCcell);

    % Determine crops
    FirstValid = find(~cellfun(@isempty, PCcell), 1);
    if isempty(FirstValid); return; end
    Ncrop = numel(PCcell{FirstValid});
    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Ncrop;
    end
    CropsToUse = Args.CropsToAnalyze;
    NcropUse = numel(CropsToUse);

    % Get all parameter names from first successful PC
    AllParNames = {};
    for Ic = 1:Ncrop
        if PCcell{FirstValid}(Ic).Success
            P = PCcell{FirstValid}(Ic).TransModel.getAllFunPar();
            AllParNames = P.Name;
            break;
        end
    end
    if isempty(AllParNames); return; end

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

    for Ip = 1:numel(Args.ParamNames)
        PName = Args.ParamNames{Ip};
        PIdx = find(strcmp(AllParNames, PName));
        if isempty(PIdx)
            warning('plotPhotFittedParams:ParamNotFound', ...
                'Parameter %s not found in model. Available: %s', ...
                PName, strjoin(AllParNames, ', '));
            continue;
        end

        % Extract parameter values: [Nvisits x Ncrop]
        ParMat = nan(Nvisits, max(CropsToUse));
        for Iv = 1:Nvisits
            if isempty(PCcell{Iv}); continue; end
            for Ic = CropsToUse
                if Ic > numel(PCcell{Iv}); continue; end
                if ~PCcell{Iv}(Ic).Success; continue; end
                P = PCcell{Iv}(Ic).TransModel.getAllFunPar();
                ParMat(Iv, Ic) = P.Val(PIdx);
            end
        end

        % Plot
        figure('Name', sprintf('%s vs Epoch', PName), ...
               'Position', [50, 50, 800, 450]);
        hold on;

        for Iic = 1:NcropUse
            Ic = CropsToUse(Iic);
            if ismember(Ic, CentralCrops)
                plot(EpochVec, ParMat(:, Ic), '-', 'LineWidth', 1.5, ...
                    'Color', [Cmap(Iic,:) 0.7]);
            else
                plot(EpochVec, ParMat(:, Ic), '--', 'LineWidth', 0.5, ...
                    'Color', [Cmap(Iic,:) 0.3]);
            end
        end

        % Median over crops (bold)
        MedPar = nanmedian(ParMat(:, CropsToUse), 2);
        plot(EpochVec, MedPar, '-k', 'LineWidth', 3.5);

        box on; grid on;
        xlabel('Epoch');
        ylabel(strrep(PName, '_', '\_'));
        title(sprintf('%s vs epoch (%d crops, median in black)', ...
            strrep(PName, '_', '\_'), NcropUse));
    end
end
