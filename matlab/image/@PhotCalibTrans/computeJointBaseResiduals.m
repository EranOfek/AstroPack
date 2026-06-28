function [BaseResid, MagErr, XFULL, YFULL, CropID] = computeJointBaseResiduals(PC_array, Args)
    % DEPRECATED (Apr 2026): the refactored PhotCalibTrans.fitJointVisit
    % constructs a single joint CompositeFun and lets its own fitMultiStage
    % handlers compute base residuals as part of each stage. This helper
    % is retained for diagnostics — e.g., to compare per-crop residuals
    % at a particular (Norm=1, ParX=0) state before / after a joint fit.
    %
    % Per-crop baseline residual computation for joint LS.
    %
    % For each PC(i):
    %   - Snapshot current Norm + Tran2D.ParX.
    %   - Set Norm = 1, Tran2D.ParX = 0 (atmospheric-only prediction).
    %   - Build CostArgs from PC's stored state (mirrors the cell in
    %     PhotCalibTrans.calibrate around line 1126).
    %   - Call PC.TransModel.costFun to get the 4th output
    %     (UnweightedResiduals = m_obs - m_predicted_atmospheric, in mag).
    %     and the 5th output (MagErr).
    %   - Restore Norm + ParX.
    %
    % Then concatenate across all crops, with CropID, XFULL, YFULL tags
    % (XFULL, YFULL must already be on PC(i).SourceData, written by
    % selectCalibratorsJoint).
    % Input  : - PC_array - 1xN PhotCalibTrans array. Each PC must have
    %                       TransModel, SpecData, SourceData populated, and
    %                       SourceData carrying X, Y, XFULL, YFULL columns
    %                       (the latter two written by
    %                       selectCalibratorsJoint via imProc.cat.addXYfull).
    %          * Args - struct or key/val with:
    %             .Verbose - logical, default false.
    % Output : - BaseResid - Nx1 pooled UnweightedResiduals (mag), where
    %                        N = sum over crops of height(PC(i).SourceData).
    %          - MagErr    - Nx1 pooled per-source MagErr (mag).
    %          - XFULL     - Nx1 pooled full-image X.
    %          - YFULL     - Nx1 pooled full-image Y.
    %          - CropID    - Nx1 integer tag of the origin crop.
    % Author : D. Kovaleva (April 2026)
    % Example: [Resid, MagErr, XFULL, YFULL, CropID] = ...
    %              PhotCalibTrans.computeJointBaseResiduals(PC);

    arguments
        PC_array
        Args.Verbose logical = false
    end

    Ncrops = numel(PC_array);
    ResidCell  = cell(Ncrops, 1);
    MagErrCell = cell(Ncrops, 1);
    XfullCell  = cell(Ncrops, 1);
    YfullCell  = cell(Ncrops, 1);
    CropIDCell = cell(Ncrops, 1);

    for I = 1:Ncrops
        PC = PC_array(I);
        if isempty(PC.TransModel) || isempty(PC.SpecData) || isempty(PC.SourceData)
            continue;
        end

        SD = PC.SourceData;
        if height(SD.Table) == 0
            continue;
        end

        % ----- Snapshot current params -------------------------------------
        AllFunPar = PC.TransModel.getAllFunPar();
        NormIdx   = find(strcmp(AllFunPar.Name, 'Norm'), 1);
        NormSaved = AllFunPar.Val(NormIdx);
        ParXSaved = PC.TransModel.Tran2DObj.ParX;

        % ----- Set Norm=1, ParX=0 ------------------------------------------
        AllFunPar.Val(NormIdx) = 1.0;
        PC.TransModel.setAllFunPar(AllFunPar);
        PC.TransModel.Tran2DObj.ParX = zeros(1, numel(ParXSaved));

        % ----- Build CostArgs from PC stored state -------------------------
        Flux = SD.getCol('Flux');
        X    = SD.getCol('X');
        Y    = SD.getCol('Y');
        ExpTime_eff = PC.ExpTime_eff;

        % MagErr: prefer the precomputed column on SourceData (written by
        % calibrate) if it exists, else compute via propagateCalibratorMagErr.
        VarNames = SD.Table.Properties.VariableNames;
        if ismember('MagErr', VarNames)
            PrecomputedMagErr = SD.getCol('MagErr');
        else
            % Fall back to default weighting; this matches the calibrate
            % path's default before any user override.
            FluxErrColName = 'FLUXERR_APER_3';
            FluxErrVector = [];
            if ismember(FluxErrColName, VarNames)
                FluxErrVector = SD.getCol(FluxErrColName);
            end
            PrecomputedMagErr = PC.propagateCalibratorMagErr(Flux, FluxErrVector, ...
                'WeightingMode', 'spectral', 'ExpTime', ExpTime_eff);
        end

        SpecFluxMatrix = PC.resampleCalibratorSpectra();

        CostArgs = { ...
            'WeightMatrix',               PC.SpecData.Spec', ...
            'PrecomputedMagErr',          PrecomputedMagErr, ...
            'PrecomputedSpecFluxMatrix',  SpecFluxMatrix, ...
            'TransmissionMode',           true, ...
            'CalibWavelength',            PC.SpecData.SpecWvl, ...
            'ExpTime',                    ExpTime_eff, ...
            'Aperture_area_m2',           PC.Aperture, ...
            'X',                          X, ...
            'Y',                          Y };

        % ----- Call costFun -> UnweightedResiduals (4th output) -----------
        [~, ~, ~, UnweightedRes, MagErrI] = PC.TransModel.costFun( ...
            PC.TransWvl, Flux, CostArgs{:});

        % ----- Restore params ----------------------------------------------
        AllFunPar.Val(NormIdx) = NormSaved;
        PC.TransModel.setAllFunPar(AllFunPar);
        PC.TransModel.Tran2DObj.ParX = ParXSaved;

        % ----- Collect XFULL/YFULL -----------------------------------------
        if ~ismember('XFULL', VarNames) || ~ismember('YFULL', VarNames)
            error('PhotCalibTrans:computeJointBaseResiduals:NoXYFULL', ...
                  ['PC(%d).SourceData lacks XFULL/YFULL. Run ', ...
                   'selectCalibratorsJoint first so addXYfull populates them.'], I);
        end
        Xfull_i = SD.getCol('XFULL');
        Yfull_i = SD.getCol('YFULL');

        ResidCell{I}  = UnweightedRes(:);
        MagErrCell{I} = MagErrI(:);
        XfullCell{I}  = Xfull_i(:);
        YfullCell{I}  = Yfull_i(:);
        CropIDCell{I} = repmat(double(I), numel(UnweightedRes), 1);

        if Args.Verbose
            fprintf('  PC(%d): %d calibrators, RMS(BaseResid)=%.4f mag\n', ...
                    I, numel(UnweightedRes), sqrt(mean(UnweightedRes.^2)));
        end
    end

    NonEmpty = ~cellfun(@isempty, ResidCell);
    if ~any(NonEmpty)
        error('PhotCalibTrans:computeJointBaseResiduals:NoData', ...
              'No PC in the array produced any calibrator residuals.');
    end

    BaseResid = vertcat(ResidCell{NonEmpty});
    MagErr    = vertcat(MagErrCell{NonEmpty});
    XFULL     = vertcat(XfullCell{NonEmpty});
    YFULL     = vertcat(YfullCell{NonEmpty});
    CropID    = vertcat(CropIDCell{NonEmpty});

    if Args.Verbose
        fprintf('  Joint pool: %d total calibrators\n', numel(BaseResid));
    end
end
