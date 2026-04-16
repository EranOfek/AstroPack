function plotPhotTransmission(PC, Args)
    % Plot transmission curves per mode
    % Description: One panel per mode. For per-epoch modes, shows per-crop
    %              transmission curves from a selected epoch. For shapeimage
    %              and perimage, overlays the reference transmission (thick
    %              black). For visit-level modes, shows the visit-averaged
    %              transmission with per-crop Norm variations.
    %
    % Input  : - Struct with .PC.(mode){Iv}(Ic) PhotCalibTrans arrays,
    %            and optionally .PersetInfo for visit-level modes.
    %            (Result struct from testPhotCalib or calibratePhotModes.)
    %          * ...,key,val,...
    %            'Modes'       - Cell array of modes. Default is {'percrop'}.
    %            'EpochIdx'    - Which epoch to plot. Default is 1.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'RefCrop'     - Reference crop index. Default is 10.
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotTransmission(R, 'Modes', {'percrop'});
    %          pipeline.last.quality.plotPhotTransmission(R, ...
    %              'Modes', {'percrop','shapeimage','perset'});

    arguments
        PC
        Args.Modes          = {'percrop'}
        Args.EpochIdx       = 1
        Args.CropsToAnalyze = []
        Args.RefCrop        = 10
    end

    Nmodes = numel(Args.Modes);
    VisitModes = {'perset', 'perset_raw', 'shapeset'};
    RefModes   = {'shapeimage', 'perimage', 'perimage_raw'};

    % Resolve input: Result struct, PC struct, or raw PhotCalibTrans array
    PersetInfo = [];
    if isstruct(PC) && isfield(PC, 'PC')
        % Full Result struct
        if isfield(PC, 'PersetInfo')
            PersetInfo = PC.PersetInfo;
        end
        PCdata = PC.PC;
    elseif isstruct(PC)
        % PC struct (with mode fields)
        PCdata = PC;
    elseif isa(PC, 'PhotCalibTrans')
        % Raw PhotCalibTrans array — wrap as percrop
        PCdata.percrop = {PC};
    elseif iscell(PC)
        % Cell array of PhotCalibTrans arrays
        PCdata.percrop = PC;
    else
        return;
    end

    Angstrom = char(197);

    figure('Name', 'Transmission Curves', ...
           'Position', [50, 50, 500*Nmodes, 450]);

    for Im = 1:Nmodes
        Mode = Args.Modes{Im};
        subplot(1, Nmodes, Im);
        hold on;

        Iv = Args.EpochIdx;
        IsVisitMode = ismember(Mode, VisitModes);
        IsRefMode   = ismember(Mode, RefModes);

        % For visit-level modes, use percrop PCs with PersetInfo
        if IsVisitMode
            SourceMode = 'percrop';
        else
            SourceMode = Mode;
        end

        if ~isfield(PCdata, SourceMode) || isempty(PCdata.(SourceMode){Iv})
            title(sprintf('%s — no data', Mode));
            continue;
        end

        PCvis = PCdata.(SourceMode){Iv};
        CropsToUse = Args.CropsToAnalyze;
        if isempty(CropsToUse)
            CropsToUse = 1:numel(PCvis);
        end

        Lambda = PCvis(1).TransWvl;
        Ncrop = numel(CropsToUse);
        Cmap = lines(Ncrop);

        % Get Norm index (needed for visit-level modes)
        NormIdx = [];
        if IsVisitMode || IsRefMode
            AllFunPar = PCvis(1).TransModel.getAllFunPar();
            NormIdx = find(strcmp(AllFunPar.Name, 'Norm'));
        end

        % For visit-level modes, get VisitRefParams
        VisitRefParams = [];
        Info = [];
        if IsVisitMode && ~isempty(PersetInfo) && isfield(PersetInfo, Mode)
            Info = PersetInfo.(Mode);
            VisitRefParams = Info.VisitRefParams;
        end

        % Plot per-crop curves
        for Iic = 1:Ncrop
            Ic = CropsToUse(Iic);
            if Ic > numel(PCvis) || ~PCvis(Ic).Success; continue; end

            if IsVisitMode && ~isempty(VisitRefParams)
                % Visit-level: evaluate with visit-averaged params
                CropParams = VisitRefParams;
                if Info.IsShapeset
                    % shapeset: use per-crop Norm
                    CropNorm = PCvis(Ic).TransModel.getAllFunPar();
                    CropParams(NormIdx) = CropNorm.Val(NormIdx);
                else
                    % perset: adjusted Norm to TargetZP
                    PCvisEval = PCvis(Ic).derivePC(VisitRefParams, ...
                        'UseRefNorm', true, ...
                        'NormTran2DToCenter', Info.DoNormTran2D);
                    ZPvisitBase = PCvisEval.evaluateZP();
                    if isfinite(ZPvisitBase) && Ic <= numel(Info.TargetZP) && ...
                       isfinite(Info.TargetZP(Ic))
                        DeltaZP = Info.TargetZP(Ic) - ZPvisitBase;
                        CropParams(NormIdx) = VisitRefParams(NormIdx) * 10^(DeltaZP / 2.5);
                    end
                end
                Trans = PCvis(Ic).TransModel.evaluateAllFunParInput(Lambda, CropParams(:)');
            else
                % Per-epoch: evaluate each crop's own transmission
                Trans = PCvis(Ic).evaluateTransmission('Lambda', Lambda);
            end

            LW = 0.5;
            Alpha = 0.5;
            if Ic == Args.RefCrop; LW = 1.5; Alpha = 1; end
            hLine = plot(Lambda, Trans, 'LineWidth', LW, 'Color', [Cmap(Iic,:) Alpha]);
        end

        % Overlay reference curve for shapeimage/perimage modes
        if IsRefMode && Args.RefCrop <= numel(PCvis) && PCvis(Args.RefCrop).Success
            RefTrans = PCvis(Args.RefCrop).evaluateTransmission('Lambda', Lambda);
            plot(Lambda, RefTrans, '-k', 'LineWidth', 2.5);
        end

        % Overlay percrop curves (gray) for visit-level modes for comparison
        if IsVisitMode
            for Iic = 1:Ncrop
                Ic = CropsToUse(Iic);
                if Ic > numel(PCvis) || ~PCvis(Ic).Success; continue; end
                PercropTrans = PCvis(Ic).evaluateTransmission('Lambda', Lambda);
                plot(Lambda, PercropTrans, '-', 'LineWidth', 0.3, 'Color', [0.7 0.7 0.7 0.3]);
            end
        end

        box on; grid on;
        xlabel(['Wavelength [' Angstrom ']']);
        ylabel('Transmission');
        title(sprintf('%s (epoch %d, %d crops)', Mode, Args.EpochIdx, Ncrop));
    end
end
