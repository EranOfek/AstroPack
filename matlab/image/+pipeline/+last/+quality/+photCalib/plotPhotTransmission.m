function Fig = plotPhotTransmission(PC, Args)
    % Plot per-crop transmission curves for one visit (multi-crop aggregator).
    % Description: For a single epoch, plots each crop's transmission curve
    %              (from PhotCalibTrans.evaluateTransmission) on one panel,
    %              with the reference crop optionally highlighted. Companion
    %              of the per-PhotCalibTrans PhotCalibTrans.plotTransmission
    %              method: this version aggregates multiple PCs (per crop)
    %              into a single overlay so spread across the focal plane is
    %              visible at a glance. Mode-keyed inputs and the legacy
    %              shapeimage / perimage / perset paths are gone.
    %
    % Input  : - PC - a PhotCalibTrans array, a cell of arrays (one per
    %            epoch), or a struct with a .PC field (anything resolveInput
    %            accepts).
    %          * ...,key,val,...
    %            'EpochIdx'       - Epoch (visit) index to plot. Default 1.
    %            'CropsToAnalyze' - Crop indices. Default [] (all).
    %            'RefCrop'        - Reference crop index. Default 10.
    %            'HighlightRef'   - Draw the reference-crop curve bold.
    %                               Default true.
    %            'Lambda'         - Wavelength grid [Angstrom]. Default []
    %                               (use the PC's TransWvl).
    % Output : - Fig - the created figure handle ([] when there is no data).
    % Author : photCalib package refactor (2026-05)
    % Example: plotPhotTransmission(R.PC, 'EpochIdx', 1);
    %          plotPhotTransmission(PCarray, 'RefCrop', 10);

    arguments
        PC
        Args.EpochIdx       (1,1) double {mustBeInteger, mustBePositive} = 1
        Args.CropsToAnalyze double  = []
        Args.RefCrop        (1,1) double {mustBeInteger, mustBePositive} = 10
        Args.HighlightRef   logical = true
        Args.Lambda                 = []
    end

    Fig = [];
    PCcell = resolveInput(PC);
    if isempty(PCcell); return; end
    if Args.EpochIdx > numel(PCcell); return; end

    PCvis = PCcell{Args.EpochIdx};
    if isempty(PCvis); return; end

    Crops = Args.CropsToAnalyze;
    if isempty(Crops); Crops = 1:numel(PCvis); end
    Crops = Crops(Crops >= 1 & Crops <= numel(PCvis));
    if isempty(Crops); return; end

    Lambda = Args.Lambda;
    if isempty(Lambda)
        % First PC with a populated TransWvl
        for K = Crops
            if ~isempty(PCvis(K).TransModel) && ~isempty(PCvis(K).TransWvl)
                Lambda = PCvis(K).TransWvl;
                break;
            end
        end
    end
    if isempty(Lambda); return; end

    Cmap = lines(numel(Crops));
    Fig  = figure('Name', sprintf('Transmission per crop (epoch %d)', Args.EpochIdx), ...
                  'Position', [50 50 700 450]);
    hold on;
    for Ii = 1:numel(Crops)
        Ic = Crops(Ii);
        if isempty(PCvis(Ic).TransModel); continue; end
        try
            Trans = PCvis(Ic).evaluateTransmission('Lambda', Lambda);
        catch
            continue;
        end
        LW    = 0.5;
        Alpha = 0.5;
        if Args.HighlightRef && Ic == Args.RefCrop
            LW    = 1.5;
            Alpha = 1.0;
        end
        plot(Lambda, Trans, 'LineWidth', LW, 'Color', [Cmap(Ii,:) Alpha]);
    end

    box on; grid on;
    Angstrom = char(197);
    xlabel(['Wavelength [' Angstrom ']']);
    ylabel('Transmission');
    title(sprintf('Transmission per crop (epoch %d, %d crops)', ...
        Args.EpochIdx, numel(Crops)));
end
