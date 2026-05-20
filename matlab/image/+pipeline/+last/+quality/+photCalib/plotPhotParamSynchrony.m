function [Result, Fig] = plotPhotParamSynchrony(PC, Args)
    % Analyse cross-crop synchrony of fitted parameters (or shape transmission).
    % Description: For each requested quantity, produces:
    %   (1) Delta plot: Quantity(Iv,Ic) - crop median, vs epoch, one line per
    %       crop - reveals crops that deviate from the global trend.
    %   (2) Cross-crop correlation heatmap of the per-crop time series.
    %   (3) A console summary: variance decomposition and mean / median
    %       cross-crop correlation, for STATE (deviation from each crop's
    %       temporal median) and DYNAMICS (epoch-to-epoch differences).
    %
    %   The special quantity name 'ShapeT' is the shape-only integral
    %   transmission (the transmission curve evaluated with Norm = 1 and no
    %   Tran2D, then integrated) - it isolates atmospheric/spectral shape
    %   variation from crop-to-crop Norm differences. This replaces the
    %   former separate plotPhotShapeSynchrony.
    %
    % Input  : - PC - a PhotCalibTrans array, a cell of such arrays, or a
    %            struct with a .PC field (anything resolveInput accepts).
    %          * ...,key,val,...
    %            'ParamNames'     - Quantity name(s); fitted-parameter names
    %                               and/or 'ShapeT'. Default
    %                               {'TauAod500','PWV_cm','Center_Ang','Norm'}.
    %            'CropsToAnalyze' - Crop indices. Default [] (all).
    %            'TileOrder'      - 'rowmajor' | 'colmajor' central-crop
    %                               classification. Default 'rowmajor'.
    %            'WvlRange'       - [min max] wavelength range [Angstrom] for
    %                               the 'ShapeT' quantity. Default [] (full).
    %            'PlotDelta'      - Draw the delta plot. Default true.
    %            'PlotHeatmap'    - Draw the correlation heatmap. Default true.
    %            'Verbose'        - Print the numerical summary. Default true.
    % Output : - Result - struct with one field per quantity (valid struct
    %            name), each carrying .ParMat, .DeltaMat, .DiffMat and
    %            .All / .Central / .Peripheral synchrony stats.
    %          - Fig - array of created figure handles.
    % Author : photCalib package refactor (2026-05)
    % Example: plotPhotParamSynchrony(R.PC, 'ParamNames', {'TauAod500','PWV_cm'});
    %          plotPhotParamSynchrony(R.PC, 'ParamNames', {'ShapeT'});

    arguments
        PC
        Args.ParamNames      cell    = {'TauAod500','PWV_cm','Center_Ang','Norm'}
        Args.CropsToAnalyze  double  = []
        Args.TileOrder       {mustBeTextScalar} = 'rowmajor'
        Args.WvlRange                = []
        Args.PlotDelta       logical = true
        Args.PlotHeatmap     logical = true
        Args.Verbose         logical = true
    end

    Result = struct();
    Fig    = gobjects(0);

    PCcell = resolveInput(PC);
    if isempty(PCcell); return; end

    Nvisits    = numel(PCcell);
    FirstValid = find(~cellfun(@isempty, PCcell), 1);
    if isempty(FirstValid); return; end
    Ncrop = numel(PCcell{FirstValid});

    CropsToUse = Args.CropsToAnalyze;
    if isempty(CropsToUse); CropsToUse = 1:Ncrop; end
    NcropUse = numel(CropsToUse);

    % Fitted-parameter names available in the model
    AllParNames = {};
    for Ic = 1:Ncrop
        if PCcell{FirstValid}(Ic).Success
            P = PCcell{FirstValid}(Ic).TransModel.getAllFunPar();
            AllParNames = P.Name;
            break;
        end
    end

    CentralCropSet   = centralCrops(Args.TileOrder, Ncrop);
    CentralMask      = ismember(CropsToUse, CentralCropSet);
    CentralCropsUsed = CropsToUse(CentralMask);
    PeriphCrops      = CropsToUse(~CentralMask);

    EpochVec = 1:Nvisits;

    if Args.Verbose
        fprintf('\n=== Cross-crop synchrony — STATE (per-epoch level) ===\n');
        fprintf('%-20s | %11s %11s %11s | %11s %11s %11s | %11s %11s %11s\n', ...
            'Quantity', 'ShVar(%)','MnCorr','MdCorr', ...
            'ShVar(%)','MnCorr','MdCorr', 'ShVar(%)','MnCorr','MdCorr');
        fprintf('%s\n', repmat('-', 1, 140));
    end
    DynRows = {};

    for Ip = 1:numel(Args.ParamNames)
        PName = char(Args.ParamNames{Ip});

        [Mat, Ok] = i_quantityMatrix(PCcell, PName, AllParNames, Args, Nvisits, Ncrop);
        if ~Ok
            warning('photCalib:plotPhotParamSynchrony:NotFound', ...
                'Quantity %s could not be resolved.', PName);
            continue;
        end

        SubMat     = Mat(:, CropsToUse);
        MedPerCrop = median(SubMat, 1, 'omitnan');
        DeltaMat   = SubMat - MedPerCrop;

        [SharedVar, MeanCorr, MedianCorr, CorrMat] = i_syncStats(SubMat, DeltaMat);

        % Dynamics: epoch-to-epoch differences
        DiffMat      = diff(SubMat, 1, 1);
        DiffDeltaMat = DiffMat - median(DiffMat, 1, 'omitnan');
        [DynShVar, DynMeanCorr, DynMedCorr, DynCorrMat] = ...
            i_syncStats(DiffMat, DiffDeltaMat);

        CentralIdx = find(CentralMask);
        PeriphIdx  = find(~CentralMask);
        [VR_c,MC_c,MDC_c, DVR_c,DMC_c,DMDC_c] = ...
            i_groupStats(SubMat, DeltaMat, DiffMat, DiffDeltaMat, CentralIdx);
        [VR_p,MC_p,MDC_p, DVR_p,DMC_p,DMDC_p] = ...
            i_groupStats(SubMat, DeltaMat, DiffMat, DiffDeltaMat, PeriphIdx);

        if Args.Verbose
            fprintf('%-20s | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f\n', ...
                PName, SharedVar, MeanCorr, MedianCorr, ...
                VR_c, MC_c, MDC_c, VR_p, MC_p, MDC_p);
            DynRows{end+1} = sprintf(['%-20s | %11.1f %11.3f %11.3f | ', ...
                '%11.1f %11.3f %11.3f | %11.1f %11.3f %11.3f'], ...
                PName, DynShVar, DynMeanCorr, DynMedCorr, ...
                DVR_c, DMC_c, DMDC_c, DVR_p, DMC_p, DMDC_p); %#ok<AGROW>
        end

        FN = matlab.lang.makeValidName(PName);
        Result.(FN).ParMat   = Mat;
        Result.(FN).DeltaMat = DeltaMat;
        Result.(FN).DiffMat  = DiffMat;
        Result.(FN).All = struct('SharedVar',SharedVar, 'MeanCorr',MeanCorr, ...
            'MedianCorr',MedianCorr, 'CorrMatrix',CorrMat, ...
            'DynSharedVar',DynShVar, 'DynMeanCorr',DynMeanCorr, ...
            'DynMedianCorr',DynMedCorr, 'DynCorrMatrix',DynCorrMat);
        Result.(FN).Central = struct('SharedVar',VR_c, 'MeanCorr',MC_c, ...
            'MedianCorr',MDC_c, 'DynSharedVar',DVR_c, 'DynMeanCorr',DMC_c, ...
            'DynMedianCorr',DMDC_c);
        Result.(FN).Peripheral = struct('SharedVar',VR_p, 'MeanCorr',MC_p, ...
            'MedianCorr',MDC_p, 'DynSharedVar',DVR_p, 'DynMeanCorr',DMC_p, ...
            'DynMedianCorr',DMDC_p);

        % --- (1) Delta plot --------------------------------------------
        if Args.PlotDelta
            DeltaFull = nan(Nvisits, Ncrop);
            DeltaFull(:, CropsToUse) = DeltaMat;
            F = figure('Name', sprintf('%s delta vs epoch', PName), ...
                       'Position', [50 50 800 450]);
            lineVsEpoch(gca, EpochVec, DeltaFull, 'CropsToUse', CropsToUse, ...
                'CentralCrops', CentralCropSet, 'ShowMedian', false);
            plot(EpochVec, zeros(size(EpochVec)), '-k', 'LineWidth', 2);
            xlabel('Epoch');
            ylabel(sprintf('%s - crop median', strrep(PName,'_','\_')));
            title(sprintf('%s deviation from cross-crop median', ...
                strrep(PName,'_','\_')));
            text(0.02, 0.98, sprintf( ...
                'ShVar: Total=%.1f%%, Central=%.1f%%, Periph=%.1f%%', ...
                SharedVar, VR_c, VR_p), 'Units','normalized', ...
                'VerticalAlignment','top', 'HorizontalAlignment','left', ...
                'FontSize',9, 'BackgroundColor',[1 1 1 0.7], ...
                'EdgeColor',[0.6 0.6 0.6]);
            Fig(end+1) = F; %#ok<AGROW>
        end

        % --- (2) Correlation heatmap -----------------------------------
        if Args.PlotHeatmap
            F = figure('Name', sprintf('%s cross-crop correlation', PName), ...
                       'Position', [50 50 600 550]);
            imagesc(CorrMat);
            colorbar; colormap(jet); caxis([-1 1]);
            axis equal tight;
            xlabel('Crop ID'); ylabel('Crop ID');
            set(gca, 'XTick', 1:NcropUse, 'XTickLabel', CropsToUse, ...
                     'YTick', 1:NcropUse, 'YTickLabel', CropsToUse);
            title(sprintf('%s cross-crop correlation', strrep(PName,'_','\_')));
            Fig(end+1) = F; %#ok<AGROW>
        end
    end

    if Args.Verbose
        fprintf('%s\n', repmat('-', 1, 140));
        if ~isempty(DynRows)
            fprintf('\n=== Cross-crop synchrony — DYNAMICS (epoch-to-epoch change) ===\n');
            fprintf('%-20s | %11s %11s %11s | %11s %11s %11s | %11s %11s %11s\n', ...
                'Quantity', 'ShVar(%)','MnCorr','MdCorr', ...
                'ShVar(%)','MnCorr','MdCorr', 'ShVar(%)','MnCorr','MdCorr');
            fprintf('%s\n', repmat('-', 1, 140));
            for Ir = 1:numel(DynRows)
                fprintf('%s\n', DynRows{Ir});
            end
            fprintf('%s\n', repmat('-', 1, 140));
        end
        fprintf('STATE: deviations from each crop''s temporal median\n');
        fprintf('DYNAMICS: epoch-to-epoch differences (diff along time)\n');
        fprintf('SharedVar: %% of variance carried by the cross-crop median signal\n');
        fprintf('Central crops: %s\n', mat2str(CentralCropsUsed));
        fprintf('Peripheral crops: %s\n', mat2str(PeriphCrops));
    end
end

% =========================================================================
function [Mat, Ok] = i_quantityMatrix(PCcell, Name, AllParNames, Args, Nvisits, Ncrop)
    % Build a [Nvisit x Ncrop] matrix of one quantity. 'ShapeT' is the
    % shape-only integral transmission; any other name is a fitted parameter.
    Mat = nan(Nvisits, Ncrop);
    Ok  = true;

    if strcmpi(Name, 'ShapeT')
        NormIdx = find(strcmp(AllParNames, 'Norm'), 1);
        if isempty(NormIdx); Ok = false; return; end
        for Iv = 1:Nvisits
            if isempty(PCcell{Iv}); continue; end
            for Ic = 1:min(numel(PCcell{Iv}), Ncrop)
                PCobj = PCcell{Iv}(Ic);
                if ~PCobj.Success; continue; end
                Lambda = PCobj.TransWvl;
                P = PCobj.TransModel.getAllFunPar();
                V = P.Val(:).';
                V(NormIdx) = 1;                       % shape-only: Norm = 1
                Trans = PCobj.TransModel.evaluateAllFunParInput(Lambda, V);
                Trans = Trans(:);
                Lam   = Lambda(:);
                if ~isempty(Args.WvlRange)
                    M = Lam >= Args.WvlRange(1) & Lam <= Args.WvlRange(2);
                    Lam = Lam(M); Trans = Trans(M);
                end
                if numel(Lam) < 2; continue; end
                Mat(Iv,Ic) = trapz(Lam, Trans) / (Lam(end) - Lam(1));
            end
        end
    else
        PIdx = find(strcmp(AllParNames, Name), 1);
        if isempty(PIdx); Ok = false; return; end
        for Iv = 1:Nvisits
            if isempty(PCcell{Iv}); continue; end
            for Ic = 1:min(numel(PCcell{Iv}), Ncrop)
                if ~PCcell{Iv}(Ic).Success; continue; end
                P = PCcell{Iv}(Ic).TransModel.getAllFunPar();
                Mat(Iv,Ic) = P.Val(PIdx);
            end
        end
    end
end

% =========================================================================
function [SharedVar, MeanCorr, MedianCorr, CorrMat] = i_syncStats(SubMat, DeltaMat)
    MedDeltaEpoch = median(DeltaMat, 2, 'omitnan');
    TotalVar  = var(DeltaMat(:), 'omitnan');
    MedianVar = var(MedDeltaEpoch, 'omitnan');
    if TotalVar > 0
        SharedVar = 100 * MedianVar / TotalVar;
    else
        SharedVar = NaN;
    end
    Nc = size(SubMat, 2);
    if Nc >= 2
        CorrMat = corr(SubMat, 'rows', 'pairwise');
        OffDiag = CorrMat(~eye(Nc));
        MeanCorr   = mean(OffDiag, 'omitnan');
        MedianCorr = median(OffDiag, 'omitnan');
    else
        CorrMat = NaN(Nc);
        MeanCorr = NaN; MedianCorr = NaN;
    end
end

% =========================================================================
function [VR,MC,MDC, DVR,DMC,DMDC] = i_groupStats(SubMat, DeltaMat, ...
        DiffMat, DiffDeltaMat, Idx)
    % Synchrony stats for a crop subset (central or peripheral); NaN if <2.
    if numel(Idx) >= 2
        [VR,  MC,  MDC ] = i_syncStats(SubMat(:,Idx),  DeltaMat(:,Idx));
        [DVR, DMC, DMDC] = i_syncStats(DiffMat(:,Idx), DiffDeltaMat(:,Idx));
    else
        VR = NaN; MC = NaN; MDC = NaN;
        DVR = NaN; DMC = NaN; DMDC = NaN;
    end
end
