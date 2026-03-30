function plotPhotFWHM(AI, Args)
    % Plot FWHM from image headers vs epoch number
    % Description: Reads FWHM keyword from AstroHeader of each crop/epoch.
    %              Plots thin lines per crop and thick line for median over crops.
    %
    % Input  : - AI cell(Nepochs,1) of AstroImage arrays (from loadVisitData).
    %          * ...,key,val,...
    %            'HeaderKey' - FWHM keyword name. Default is 'FWHM'.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Ncrop'     - Number of crops. Default is 24.
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotFWHM(AI);

    arguments
        AI cell
        Args.HeaderKey   = 'FWHM'
        Args.CropsToAnalyze = []
        Args.Ncrop       = 24
        Args.TileOrder   = 'rowmajor'
    end

    Nepochs = numel(AI);
    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end
    Ncrop = numel(Args.CropsToAnalyze);

    % Extract FWHM from headers
    FWHMmat = nan(Nepochs, Args.Ncrop);
    for Iv = 1:Nepochs
        if isempty(AI{Iv}); continue; end
        for Ic = Args.CropsToAnalyze
            if Ic > numel(AI{Iv}); continue; end
            try
                Val = AI{Iv}(Ic).HeaderData.getVal(Args.HeaderKey);
                if isnumeric(Val) && isfinite(Val)
                    FWHMmat(Iv, Ic) = Val;
                end
            catch
                % Skip if header or key not available
            end
        end
    end

    % --- Plot ---
    figure('Name', 'FWHM vs Epoch', 'Position', [50, 50, 800, 450]);
    hold on;

    EpochVec = 1:Nepochs;
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
        Ic = Args.CropsToAnalyze(Iic);
        if ismember(Ic, CentralCrops)
            plot(EpochVec, FWHMmat(:, Ic), '-', 'LineWidth', 1.5, 'Color', [Cmap(Iic,:) 0.7]);
        else
            plot(EpochVec, FWHMmat(:, Ic), '--', 'LineWidth', 0.5, 'Color', [Cmap(Iic,:) 0.3]);
        end
    end

    % Median over crops (bold)
    MedFWHM = nanmedian(FWHMmat(:, Args.CropsToAnalyze), 2);
    plot(EpochVec, MedFWHM, '-k', 'LineWidth', 3.5);

    box on; grid on;
    xlabel('Epoch');
    ylabel(sprintf('%s [arcsec]', Args.HeaderKey));
    title(sprintf('%s vs epoch (%d crops, median in black)', Args.HeaderKey, Ncrop));
end
