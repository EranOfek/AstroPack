function plotPhotAsympRMS(PC, Args)
    % Plot asymptotic RMS of absolute vs relative photometry
    % Description: Reads ARMS from PhotCalibTrans objects (absolute photometry)
    %              and PH_RMS from headers (relative photometry). Plots both
    %              on the same panel vs epoch: ARMS as colored dots/lines,
    %              PH_RMS as grey dots/lines. Per-crop lines with median.
    %
    % Input  : - PC struct, Result struct, or PhotCalibTrans array/cell
    %            (any format accepted by resolvePC).
    %            If Result struct, also reads .HeaderData.PH_RMS.
    %          * ...,key,val,...
    %            'HeaderData' - HeaderData struct with .PH_RMS field.
    %                          Default is [] (extracted from Result struct if available).
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Ncrop'     - Number of crops. Default is 24.
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    % Author : D. Kovaleva (Apr 2026)
    % Example: pipeline.last.quality.plotPhotAsympRMS(R);
    %          pipeline.last.quality.plotPhotAsympRMS(R.PC, 'HeaderData', R.HeaderData);

    arguments
        PC
        Args.HeaderData  = []
        Args.CropsToAnalyze = []
        Args.Ncrop       = 24
        Args.TileOrder   = 'rowmajor'
    end

    % Resolve PC
    PCcell = pipeline.last.quality.resolvePC(PC);
    if isempty(PCcell); return; end

    % Try to get HeaderData from Result struct
    if isempty(Args.HeaderData) && isstruct(PC) && isfield(PC, 'HeaderData')
        Args.HeaderData = PC.HeaderData;
    end

    Nvisits = numel(PCcell);
    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end
    CropsToUse = Args.CropsToAnalyze;
    NcropUse = numel(CropsToUse);

    % Central crops
    switch lower(Args.TileOrder)
        case 'colmajor'
            CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise
            CentralCrops = [];
    end

    % --- Extract ARMS from PC objects ---
    ARMSmat = nan(Nvisits, Args.Ncrop);
    for Iv = 1:Nvisits
        if isempty(PCcell{Iv}); continue; end
        for Ic = CropsToUse
            if Ic > numel(PCcell{Iv}); continue; end
            if PCcell{Iv}(Ic).Success
                ARMSmat(Iv, Ic) = PCcell{Iv}(Ic).ARMS;
            end
        end
    end

    % --- Extract PH_RMS from HeaderData ---
    HasPHRMS = false;
    PHRMSmat = nan(Nvisits, Args.Ncrop);
    if ~isempty(Args.HeaderData) && isfield(Args.HeaderData, 'PH_RMS')
        PHRMSmat = Args.HeaderData.PH_RMS;
        HasPHRMS = true;
    end

    EpochVec = 1:Nvisits;
    Cmap = lines(NcropUse);

    % --- Plot: single panel, ARMS vs PH_RMS ---
    figure('Name', 'Asymptotic RMS: absolute vs relative', ...
           'Position', [50, 50, 800, 450]);
    hold on;

    % PH_RMS (relative) — grey, behind
    if HasPHRMS
        for Iic = 1:NcropUse
            Ic = CropsToUse(Iic);
            if ismember(Ic, CentralCrops)
                plot(EpochVec, PHRMSmat(:, Ic), '-', 'LineWidth', 1.0, ...
                    'Color', [0.7 0.7 0.7 0.6]);
            else
                plot(EpochVec, PHRMSmat(:, Ic), '--', 'LineWidth', 0.3, ...
                    'Color', [0.85 0.85 0.85 0.4]);
            end
        end
        MedPH = nanmedian(PHRMSmat(:, CropsToUse), 2);
        plot(EpochVec, MedPH, '-', 'Color', [0.5 0.5 0.5], 'LineWidth', 3);
    end

    % ARMS (absolute) — colored, on top
    for Iic = 1:NcropUse
        Ic = CropsToUse(Iic);
        if ismember(Ic, CentralCrops)
            plot(EpochVec, ARMSmat(:, Ic), '-', 'LineWidth', 1.5, ...
                'Color', [Cmap(Iic,:) 0.7]);
        else
            plot(EpochVec, ARMSmat(:, Ic), '--', 'LineWidth', 0.5, ...
                'Color', [Cmap(Iic,:) 0.3]);
        end
    end
    MedARMS = nanmedian(ARMSmat(:, CropsToUse), 2);
    plot(EpochVec, MedARMS, '-k', 'LineWidth', 3.5);

    box on; grid on;
    xlabel('Epoch');
    ylabel('Asymptotic RMS [mag]');

    % Legend: only the two median lines
    if HasPHRMS
        HrelMed = plot(NaN, NaN, '-', 'Color', [0.5 0.5 0.5], 'LineWidth', 3);
        HabsMed = plot(NaN, NaN, '-k', 'LineWidth', 3.5);
        legend([HrelMed, HabsMed], {'Relative (PH\_RMS)', 'Absolute (PT\_ARMS)'}, ...
            'Location', 'best');
        title(sprintf('ARMS median=%.4f, PH\\_RMS median=%.4f', ...
            nanmedian(MedARMS), nanmedian(MedPH)));
    else
        title(sprintf('ARMS median=%.4f', nanmedian(MedARMS)));
    end
end
