function plotPhotFWHM(Input, Args)
    % Plot FWHM vs epoch number per crop
    % Description: Accepts either AI cell array (extracts from headers) or
    %              HeaderData struct (from extractHeaderData / Result.HeaderData).
    %              Plots per-crop lines (bold for central, dashed for edge)
    %              and thick median line.
    %
    % Input  : - AI cell(Nepochs,1) of AstroImage arrays, or
    %            HeaderData struct with .FWHM field [Nepochs x Ncrop],
    %            or Result struct with .HeaderData.FWHM.
    %          * ...,key,val,...
    %            'HeaderKey' - Keyword name (for AI mode). Default is 'FWHM'.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'Ncrop'     - Number of crops. Default is 24.
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotFWHM(AI);
    %          pipeline.last.quality.plotPhotFWHM(R.HeaderData);
    %          pipeline.last.quality.plotPhotFWHM(R);

    arguments
        Input
        Args.HeaderKey   = 'FWHM'
        Args.CropsToAnalyze = []
        Args.Ncrop       = 24
        Args.TileOrder   = 'rowmajor'
    end

    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end
    Ncrop = numel(Args.CropsToAnalyze);

    % Resolve input: AI cell, HeaderData struct, or Result struct
    FieldName = matlab.lang.makeValidName(Args.HeaderKey);
    if isstruct(Input) && isfield(Input, 'HeaderData')
        % Result struct
        FWHMmat = Input.HeaderData.(FieldName);
    elseif isstruct(Input) && isfield(Input, FieldName)
        % HeaderData struct directly
        FWHMmat = Input.(FieldName);
    elseif iscell(Input)
        % AI cell array — extract from headers
        Nepochs = numel(Input);
        FWHMmat = nan(Nepochs, Args.Ncrop);
        for Iv = 1:Nepochs
            if isempty(Input{Iv}); continue; end
            for Ic = Args.CropsToAnalyze
                if Ic > numel(Input{Iv}); continue; end
                try
                    Val = Input{Iv}(Ic).HeaderData.getVal(Args.HeaderKey);
                    if isnumeric(Val) && isscalar(Val) && isfinite(Val)
                        FWHMmat(Iv, Ic) = Val;
                    end
                catch
                end
            end
        end
    else
        warning('plotPhotFWHM:BadInput', 'Input must be AI cell, HeaderData struct, or Result struct.');
        return;
    end

    Nepochs = size(FWHMmat, 1);

    % --- Plot ---
    figure('Name', sprintf('%s vs Epoch', Args.HeaderKey), ...
           'Position', [50, 50, 800, 450]);
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
    MedVal = nanmedian(FWHMmat(:, Args.CropsToAnalyze), 2);
    plot(EpochVec, MedVal, '-k', 'LineWidth', 3.5);

    box on; grid on;
    xlabel('Epoch');
    ylabel(sprintf('%s', Args.HeaderKey));
    title(sprintf('%s vs epoch (%d crops, median in black)', Args.HeaderKey, Ncrop));
end
