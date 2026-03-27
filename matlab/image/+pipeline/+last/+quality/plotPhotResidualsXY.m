function plotPhotResidualsXY(PC, Args)
    % Plot calibrator residuals vs X and Y position
    % Description: Shows spatial dependence of calibration residuals.
    %              Reveals Tran2D model quality and any uncorrected
    %              position-dependent systematics.
    %              Uses percrop calibration only.
    %
    % Input  : - PC struct with PC.percrop{Iv}(Ic) PhotCalibTrans arrays.
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Normalize'   - Plot Residual/MagErr. Default is false.
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'NTrendBins'  - Number of bins for trend. Default is 20.
    %            'YLim'        - Y-axis limits for residual. Default is [] (auto).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotResidualsXY(R.PC);
    %          pipeline.last.quality.plotPhotResidualsXY(R.PC, 'Normalize', true);

    arguments
        PC struct
        Args.CropsToAnalyze = []
        Args.Normalize logical = false
        Args.OverlayTrend   = 'median'
        Args.NTrendBins     = 20
        Args.YLim           = []
    end

    if ~isfield(PC, 'percrop')
        return;
    end

    allX   = [];
    allY   = [];
    allRes = [];

    Nvisits = numel(PC.percrop);
    for Iv = 1:Nvisits
        if isempty(PC.percrop{Iv}); continue; end

        CropsToUse = Args.CropsToAnalyze;
        if isempty(CropsToUse)
            CropsToUse = 1:numel(PC.percrop{Iv});
        end

        for Ic = CropsToUse
            if Ic > numel(PC.percrop{Iv}); continue; end
            PCobj = PC.percrop{Iv}(Ic);
            if ~PCobj.Success || isempty(PCobj.SourceData); continue; end

            Tab = PCobj.SourceData.Table;
            ColNames = Tab.Properties.VariableNames;
            if ~ismember('Residuals', ColNames); continue; end
            if ~ismember('X', ColNames) || ~ismember('Y', ColNames); continue; end

            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(height(Tab), 1);
            end

            Residuals = Tab.Residuals(UsedMask);
            X = Tab.X(UsedMask);
            Y = Tab.Y(UsedMask);

            if Args.Normalize
                if ~ismember('MagErr', ColNames); continue; end
                MagErr = Tab.MagErr(UsedMask);
                Residuals = Residuals ./ MagErr;
            end

            ValidMask = isfinite(Residuals) & isfinite(X) & isfinite(Y);
            allX   = [allX; X(ValidMask)];
            allY   = [allY; Y(ValidMask)];
            allRes = [allRes; Residuals(ValidMask)];
        end
    end

    if isempty(allRes)
        return;
    end

    % --- Plot ---
    figure('Name', 'Residuals vs X, Y', 'Position', [50, 50, 1000, 450]);

    if Args.Normalize
        ResLabel = 'Residual / MagErr';
    else
        ResLabel = 'Residual [mag]';
    end

    for Ipanel = 1:2
        subplot(1, 2, Ipanel);
        hold on;

        if Ipanel == 1
            PosData = allX;
            PosLabel = 'X [pix]';
        else
            PosData = allY;
            PosLabel = 'Y [pix]';
        end

        plot(PosData, allRes, '.', 'MarkerSize', 2);
        plot(xlim, [0 0], 'k--');

        if ~strcmp(Args.OverlayTrend, 'none')
            XRange = [min(PosData), max(PosData)];
            BinWidth = diff(XRange) / Args.NTrendBins;
            TrendFun = str2func(['nan' Args.OverlayTrend]);
            R = timeSeries.bin.binningFast([PosData(:), allRes(:)], ...
                BinWidth, XRange, {'MidBin', @numel, TrendFun});
            ValidBins = R(:,2) >= 5;
            plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);
        end

        box on; grid on;
        xlabel(PosLabel);
        ylabel(ResLabel);
        if ~isempty(Args.YLim)
            ylim(Args.YLim);
        end
    end
    sgtitle(sprintf('Residuals vs position (%d calibrators, %d epochs)', ...
        numel(allRes), Nvisits));
end
