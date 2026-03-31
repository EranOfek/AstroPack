function plotPhotResidualsVs(PC, Args)
    % Plot calibrator residuals vs any SourceData column or expression
    % Description: General-purpose calibrator diagnostic. Plots residuals
    %              (or Residual/MagErr) vs a user-specified field from
    %              SourceData.Table. Supports direct column names and simple
    %              difference expressions like 'X-XPEAK' or 'Y-YPEAK'.
    %              Uses percrop calibration only.
    %
    % Input  : - PC struct with PCcell{Iv}(Ic) PhotCalibTrans arrays.
    %          * ...,key,val,...
    %            'XField'    - Column name or expression for X axis.
    %                          Direct column: 'FLAGS', 'PSF_CHI2DOF', etc.
    %                          Difference: 'X-XPEAK', 'Y-YPEAK'.
    %                          Default is 'X-XPEAK'.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Normalize' - Plot Residual/MagErr. Default is false.
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'NTrendBins'- Number of bins. Default is 30.
    %            'YLim'      - Y-axis limits. Default is [] (auto).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotResidualsVs(R.PC);
    %          pipeline.last.quality.plotPhotResidualsVs(R.PC, 'XField', 'Y-YPEAK');
    %          pipeline.last.quality.plotPhotResidualsVs(R.PC, 'XField', 'FLAGS');
    %          pipeline.last.quality.plotPhotResidualsVs(R.PC, 'XField', 'PSF_CHI2DOF');

    arguments
        PC
        Args.XField         = 'X-XPEAK'
        Args.CropsToAnalyze = []
        Args.Normalize logical = false
        Args.OverlayTrend   = 'median'
        Args.NTrendBins     = 30
        Args.YLim           = []
    end

    PCcell = pipeline.last.quality.resolvePC(PC);
    if isempty(PCcell)
        return;
    end

    % Parse XField: check for difference expression 'A-B'
    Tokens = strsplit(Args.XField, '-');
    IsDiff = numel(Tokens) == 2 && ~isempty(Tokens{1}) && ~isempty(Tokens{2});

    AllXval = [];
    AllRes  = [];

    Nvisits = numel(PCcell);
    for Iv = 1:Nvisits
        if isempty(PCcell{Iv}); continue; end

        CropsToUse = Args.CropsToAnalyze;
        if isempty(CropsToUse)
            CropsToUse = 1:numel(PCcell{Iv});
        end

        for Ic = CropsToUse
            if Ic > numel(PCcell{Iv}); continue; end
            PCobj = PCcell{Iv}(Ic);
            if ~PCobj.Success || isempty(PCobj.SourceData); continue; end

            Tab = PCobj.SourceData.Table;
            ColNames = Tab.Properties.VariableNames;
            if ~ismember('Residuals', ColNames); continue; end

            % Filter to used calibrators
            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(height(Tab), 1);
            end

            Residuals = Tab.Residuals(UsedMask);

            % Normalize if requested
            if Args.Normalize
                if ~ismember('MagErr', ColNames); continue; end
                MagErr = Tab.MagErr(UsedMask);
                Residuals = Residuals ./ MagErr;
            end

            % Extract X values
            if IsDiff
                ColA = strtrim(Tokens{1});
                ColB = strtrim(Tokens{2});
                if ~ismember(ColA, ColNames) || ~ismember(ColB, ColNames); continue; end
                Xval = Tab.(ColA)(UsedMask) - Tab.(ColB)(UsedMask);
            else
                if ~ismember(Args.XField, ColNames); continue; end
                Xval = Tab.(Args.XField)(UsedMask);
            end

            ValidMask = isfinite(Residuals) & isfinite(Xval);
            AllXval = [AllXval; Xval(ValidMask)];
            AllRes  = [AllRes; Residuals(ValidMask)];
        end
    end

    if isempty(AllXval)
        warning('plotPhotResidualsVs:NoData', 'No valid data for %s.', Args.XField);
        return;
    end

    % --- Plot ---
    figure('Name', sprintf('Residuals vs %s', Args.XField), ...
           'Position', [50, 50, 600, 500]);
    hold on;

    plot(AllXval, AllRes, '.', 'MarkerSize', 2);
    plot(xlim, [0 0], 'k--');

    if ~strcmp(Args.OverlayTrend, 'none')
        XRange = [min(AllXval), max(AllXval)];
        BinWidth = max(diff(XRange) / Args.NTrendBins, eps);
        TrendFun = str2func(['nan' Args.OverlayTrend]);
        R = timeSeries.bin.binningFast([AllXval(:), AllRes(:)], ...
            BinWidth, XRange, {'MidBin', @numel, TrendFun});
        ValidBins = R(:,2) >= 5;
        plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);
    end

    box on; grid on;
    xlabel(strrep(Args.XField, '_', '\_'));
    if Args.Normalize
        ylabel('Residual / MagErr');
    else
        ylabel('Residual [mag]');
    end
    if ~isempty(Args.YLim)
        ylim(Args.YLim);
    end
    title(sprintf('Residuals vs %s (%d calibrators, %d epochs)', ...
        strrep(Args.XField, '_', '\_'), numel(AllRes), Nvisits));
end
