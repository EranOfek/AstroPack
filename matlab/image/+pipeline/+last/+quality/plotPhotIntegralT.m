function plotPhotIntegralT(PC, Args)
    % Plot integral transmission per crop (mosaic) and per epoch (time series)
    % Description: Computes integralTransmission (mean fractional throughput
    %              including Norm) for each crop and epoch from percrop PC
    %              objects. Produces two figures:
    %                (a) Mosaic: integral T per crop for a selected epoch.
    %                (b) Time series: T vs epoch for each crop (thin lines)
    %                    with median over all crops (thick line).
    %
    % Input  : - PC struct with PC.percrop{Iv}(Ic) PhotCalibTrans arrays,
    %            or the full Result struct from testPhotCalib.
    %          * ...,key,val,...
    %            'MosaicEpoch' - Epoch index for the mosaic plot. Default is 1.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Ncrop'     - Number of crops. Default is 24.
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    %            'WvlRange'  - [min max] wavelength range [Angstrom] for integral.
    %                          Default is [] (full range).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotIntegralT(R.PC);
    %          pipeline.last.quality.plotPhotIntegralT(R, 'MosaicEpoch', 5);

    arguments
        PC
        Args.MosaicEpoch    = 1
        Args.CropsToAnalyze = []
        Args.Ncrop          = 24
        Args.TileOrder      = 'rowmajor'
        Args.WvlRange       = []
    end

    % Resolve input to cell array of PhotCalibTrans arrays
    PCcell = pipeline.last.quality.resolvePC(PC);
    if isempty(PCcell)
        return;
    end

    Nvisits = numel(PCcell);
    CropsToUse = Args.CropsToAnalyze;

    % Find first valid epoch to determine crop count
    FirstValid = find(~cellfun(@isempty, PCcell), 1);
    if isempty(FirstValid); return; end
    if isempty(CropsToUse)
        CropsToUse = 1:numel(PCcell{FirstValid});
    end
    Ncrop = numel(CropsToUse);

    % Compute integral T for all epochs and crops
    Tmat = nan(Nvisits, Args.Ncrop);
    for Iv = 1:Nvisits
        if isempty(PCcell{Iv}); continue; end
        for Ic = CropsToUse
            if Ic > numel(PCcell{Iv}); continue; end
            PCobj = PCcell{Iv}(Ic);
            if ~PCobj.Success; continue; end
            if isempty(Args.WvlRange)
                Tmat(Iv, Ic) = PCobj.integralTransmission();
            else
                Tmat(Iv, Ic) = PCobj.integralTransmission('WvlRange', Args.WvlRange);
            end
        end
    end

    % === (a) Mosaic: integral T per crop for selected epoch ===
    Nrows = 6;
    Ncols = 4;
    GrayMap = flipud(gray(256));
    GrayMap = GrayMap(26:230, :);

    EpIdx = Args.MosaicEpoch;
    Tvals = Tmat(EpIdx, :);

    MosaicImg = nan(Nrows, Ncols);
    for Ic = 1:Args.Ncrop
        [Row, Col] = PhotCalibTrans.cropID2RowCol(Ic, Nrows, Ncols, Args.TileOrder);
        if Ic <= numel(Tvals)
            MosaicImg(Row, Col) = Tvals(Ic);
        end
    end

    figure('Name', sprintf('Integral T Mosaic — epoch %d', EpIdx), ...
           'Position', [100, 100, 500, 600]);
    imagesc(MosaicImg);
    axis xy equal tight;
    colormap(GrayMap);
    cb = colorbar;
    ylabel(cb, 'Integral T');

    hold on;
    for Ic = 1:Args.Ncrop
        [Row, Col] = PhotCalibTrans.cropID2RowCol(Ic, Nrows, Ncols, Args.TileOrder);
        Val = Tvals(Ic);
        if isfinite(Val)
            text(Col, Row, sprintf('%d\n%.4f', Ic, Val), ...
                'HorizontalAlignment', 'center', 'Color', 'w', ...
                'FontSize', 8, 'FontWeight', 'bold');
        else
            text(Col, Row, sprintf('%d', Ic), ...
                'HorizontalAlignment', 'center', 'Color', 'w', ...
                'FontSize', 8);
        end
    end
    hold off;
    title(sprintf('Integral transmission — epoch %d', EpIdx));

    % === (b) Time series: T vs epoch for each crop ===
    figure('Name', 'Integral T vs Epoch', ...
           'Position', [100, 100, 800, 450]);
    hold on;

    EpochVec = 1:Nvisits;
    Cmap = lines(Ncrop);

    % Central crops: bold solid; edge crops: thin dashed
    switch lower(Args.TileOrder)
        case 'colmajor'
            CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise
            CentralCrops = [];
    end

    for Iic = 1:Ncrop
        Ic = CropsToUse(Iic);
        if ismember(Ic, CentralCrops)
            plot(EpochVec, Tmat(:, Ic), '-', 'LineWidth', 1.5, 'Color', [Cmap(Iic,:) 0.7]);
        else
            plot(EpochVec, Tmat(:, Ic), '--', 'LineWidth', 0.5, 'Color', [Cmap(Iic,:) 0.3]);
        end
    end

    % Median over all crops (bold)
    MedT = nanmedian(Tmat(:, CropsToUse), 2);
    plot(EpochVec, MedT, '-k', 'LineWidth', 3.5);

    box on; grid on;
    xlabel('Epoch');
    ylabel('Integral T');
    title(sprintf('Integral transmission vs epoch (%d crops, median in black)', Ncrop));
end
