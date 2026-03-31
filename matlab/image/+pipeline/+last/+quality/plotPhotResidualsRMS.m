function plotPhotResidualsRMS(PC, Args)
    % Plot calibrator residual RMS vs magnitude
    % Description: Collects calibrator residuals and magnitudes from percrop
    %              PhotCalibTrans objects across all epochs and crops.
    %              Bins by magnitude and computes RMS of residuals per bin.
    %              Shows calibration accuracy as a function of brightness.
    %
    % Input  : - PC struct with PCcell{Iv}(Ic) PhotCalibTrans arrays.
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'MagField'    - Magnitude for x-axis: 'MAG_AB' (default),
    %                            'instrumental', or any SourceData column.
    %            'BinWidth'    - Magnitude bin width. Default is 0.5.
    %            'MinCount'    - Min calibrators per bin. Default is 10.
    %            'ShowScatter' - Overlay individual |Residual| dots. Default is true.
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotResidualsRMS(R.PC);
    %          pipeline.last.quality.plotPhotResidualsRMS(R.PC, 'MagField', 'instrumental');

    arguments
        PC
        Args.CropsToAnalyze = []
        Args.MagField       = 'MAG_AB'
        Args.BinWidth       = 0.5
        Args.MinCount       = 10
        Args.ShowScatter logical = false
    end

    PCcell = pipeline.last.quality.resolvePC(PC);
    if isempty(PCcell)
        return;
    end

    AllMag = [];
    AllRes = [];

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

            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(height(Tab), 1);
            end

            Residuals = Tab.Residuals(UsedMask);

            % Get magnitude
            if strcmp(Args.MagField, 'instrumental')
                if ~ismember('Flux', ColNames); continue; end
                Flux = Tab.Flux(UsedMask);
                Mag = -2.5 * log10(Flux);
            elseif ismember(Args.MagField, ColNames)
                Mag = Tab.(Args.MagField)(UsedMask);
            elseif ismember('Flux', ColNames)
                Flux = Tab.Flux(UsedMask);
                Mag = -2.5 * log10(Flux);
            else
                continue;
            end

            ValidMask = isfinite(Residuals) & isfinite(Mag);
            AllMag = [AllMag; Mag(ValidMask)];
            AllRes = [AllRes; Residuals(ValidMask)];
        end
    end

    if isempty(AllMag)
        return;
    end

    % --- Plot ---
    figure('Name', 'Calibrator Residual RMS vs Magnitude', ...
           'Position', [50, 50, 600, 500]);
    hold on;

    % Scatter of individual |residuals|
    if Args.ShowScatter
        plot(AllMag, abs(AllRes), '.', 'Color', [0.75 0.75 0.75], 'MarkerSize', 2);
    end

    % Binned RMS
    MagRange = [floor(min(AllMag)), ceil(max(AllMag))];
    R = timeSeries.bin.binningFast([AllMag(:), AllRes(:)], ...
        Args.BinWidth, MagRange, {'MidBin', @numel, @nanstd});
    ValidBins = R(:,2) >= Args.MinCount;
    HRMS = plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);

    % % Binned median |residual| for comparison
    % Rmed = timeSeries.bin.binningFast([AllMag(:), abs(AllRes(:))], ...
    %     Args.BinWidth, MagRange, {'MidBin', @numel, @nanmedian});
    % ValidBinsMed = Rmed(:,2) >= Args.MinCount;
    % hMed = plot(Rmed(ValidBinsMed,1), Rmed(ValidBinsMed,3), '--b', 'LineWidth', 1.5);

    set(gca, 'YScale', 'log');
    yl = ylim;
    ylim([yl(1) - 0.0005, yl(2) + 0.00075]);
    box on; grid on;
    if strcmp(Args.MagField, 'instrumental')
        xlabel('-2.5 log_{10}(Flux)');
    else
        xlabel(strrep(Args.MagField, '_', '\_'));
    end
    ylabel('Residual RMS [mag]');
    Th = title(sprintf('Calibrator residual RMS vs magnitude (%d calibrators, %d epochs)', ...
        numel(AllRes), Nvisits));
    Th.Units = 'normalized';
    Th.Position(2) = Th.Position(2) + 0.03;
end
