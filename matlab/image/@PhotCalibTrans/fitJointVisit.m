function [PC_array, Result] = fitJointVisit(PC_array, AI_array, Args)
    % Joint-visit calibration orchestrator (refactored Apr 2026).
    %
    % Architecture: pool all 24 crops' calibrators into one big (X, Y,
    % Flux, Spec) set, construct ONE CompositeFun (copy of PC(1).TransModel
    % with its Tran2DObj swapped to a field-frame Tran2D), and run the
    % existing CompositeFun.fitMultiStage on the pool. Any OptSeq that
    % works at per-crop scope (LAST_Joint_2Iter, LAST_NormLin, ...) also
    % works at field scope — fitMultiStage doesn't care where the (X, Y)
    % pool came from. After the fit, the same CompositeFun handle is
    % assigned to every PC(i).TransModel (handle-shared by design, per
    % [[photcalibtrans_joint_visit_plan]] Q2=(b)).
    %
    % Pipeline:
    %   1. selectCalibratorsJoint - pool calibrators across crops via
    %      findCalibCandidates → poolCalibCandidates → audit → quality →
    %      partitionByCrop, writing each PC(i).SourceData with XFULL/YFULL.
    %   2. buildFieldTran2D - field-frame Tran2DObj from CCDSEC.
    %   3. poolForJointFit - vertcat/horzcat the per-crop SourceData and
    %      SpecData into single Flux/XFULL/YFULL/Spec/SpecFluxMatrix/MagErr.
    %   4. Build JointCF = copy of PC(1).TransModel, swap Tran2DObj to
    %      FieldT2D.
    %   5. fitMultiStage on JointCF with the pooled CostArgs and OptSeqName.
    %   6. Broadcast JointCF (by handle) to every PC(i).TransModel.
    % Input  : - PC_array - 1xN PhotCalibTrans array, each PC already
    %                       constructed (TransModel built, metadata loaded).
    %                       Per-crop calibrate need not have succeeded.
    %          - AI_array - 1xN AstroImage array, same order.
    %          * Args - struct or key/val with:
    %             .CalibArgs       - cell forwarded to selectCalibratorsJoint
    %                                AND to fitMultiStage. Default {}.
    %                                Args known to selectCalibratorsJoint
    %                                are filtered through; the rest go to
    %                                fitMultiStage.
    %             .OptSeqName      - char. Default 'LAST_Joint_2Iter' — runs
    %                                the existing per-crop joint OptSeq at
    %                                field scope. Any other OptSeq works too.
    %             .OuterMaxIter    - default 2. Forwarded to fitMultiStage.
    %             .OuterSigmaClip  - default true. Forwarded.
    %             .OuterSigmaThresh - default 3.0.
    %             .OuterStdFunc    - default 'std'.
    %             .WeightingMode   - default 'combined'.
    %             .WeightedOuterIters - default [false true].
    %             .Tran2DType      - default 'cheby1_4_xt'. Forwarded to
    %                                buildFieldTran2D.
    %             .Verbose         - default false.
    % Output : - PC_array - mutated array. Each PC(i).TransModel handles
    %                       points to the same fitted joint CompositeFun.
    %                       PC(i).SourceData / SpecData carry the per-crop
    %                       slice from joint selection.
    %          - Result struct with:
    %             .JointCF        - the fitted joint CompositeFun handle.
    %             .FieldTran2DObj - the field-frame Tran2D used.
    %             .FitResult      - the struct returned by fitMultiStage
    %                               (.Cost, .RMS, .Chi2, .NCalUsed,
    %                                .StageHistory, .OuterClipHistory, ...).
    %             .Pool           - the pooled candidate table from
    %                               selectCalibratorsJoint.
    %             .JointPool      - the pooled fit-data struct from
    %                               poolForJointFit.
    % Author : D. Kovaleva (April 2026)
    % Example: [PC, R] = PhotCalibTrans.fitJointVisit(PC, AI, ...
    %              'CalibArgs', {'AuditCalibrators', true}, ...
    %              'OptSeqName', 'LAST_Joint_2Iter', 'Verbose', true);
    %          fprintf('Joint RMS = %.4f mag (%d calibrators)\n', ...
    %                  R.FitResult.RMS, R.FitResult.NCalUsed);

    arguments
        PC_array
        AI_array
        Args.CalibArgs          cell    = {}
        Args.OptSeqName         char    = 'LAST_Joint_2Iter'
        Args.OuterMaxIter       double  = 2
        Args.OuterSigmaClip     logical = true
        Args.OuterSigmaThresh   double  = 3.0
        Args.OuterStdFunc       char    = 'std'
        Args.WeightingMode      char    = 'combined'
        Args.WeightedOuterIters         = [false true]
        Args.Tran2DType         char    = 'cheby1_4_xt'
        Args.Verbose            logical = false
    end

    Ncrops = numel(PC_array);
    assert(numel(AI_array) == Ncrops, ...
        'PhotCalibTrans:fitJointVisit:LengthMismatch', ...
        'PC_array (%d) and AI_array (%d) must match.', Ncrops, numel(AI_array));

    % ----- Step 1: joint calibrator selection (pool + audit + partition) ---
    if Args.Verbose
        fprintf('=== fitJointVisit (refactored) ===\nStep 1: joint calibrator selection\n');
    end

    % Filter CalibArgs to those selectCalibratorsJoint accepts; the rest
    % (e.g., SigmaClipMethod, AirmassSource) are calibrate/fitMultiStage
    % args and not relevant for selection.
    SelectKnown = {'SearchRadius','MagRange','MinSN','MaxSN', ...
        'FilterBadFlags','FluxColName','MagColName','FilterNegFlux', ...
        'MinSN2','CalibCatName','SpFluxCol','BadBitNames', ...
        'match_catsHTMArgs','AuditCalibrators','AuditCatName', ...
        'AuditBPRPExcessFactorMax','AuditBPRPMax','AuditLASTNearestDist', ...
        'AuditLASTDeltaMag','AttachBP_RP','Verbose'};
    SelectArgs = {};
    for K = 1:2:numel(Args.CalibArgs)
        Name = Args.CalibArgs{K};
        if ischar(Name) && ismember(Name, SelectKnown)
            SelectArgs(end+1:end+2) = Args.CalibArgs(K:K+1);
        end
    end
    [PC_array, Pool] = PhotCalibTrans.selectCalibratorsJoint( ...
        PC_array, AI_array, SelectArgs{:});

    if isempty(Pool) || height(Pool) == 0
        error('PhotCalibTrans:fitJointVisit:NoCalibrators', ...
              'Joint selection produced zero calibrators. Cannot fit.');
    end

    % ----- Step 2: build field-frame Tran2D --------------------------------
    if Args.Verbose
        fprintf('Step 2: build field-frame Tran2D\n');
    end
    FieldT2D = PhotCalibTrans.buildFieldTran2D(AI_array, ...
        'Tran2DType', Args.Tran2DType, 'Verbose', Args.Verbose);

    % ----- Step 3: pool calibrator data across crops -----------------------
    if Args.Verbose
        fprintf('Step 3: pool calibrator data\n');
    end
    FluxColName = pickArg(Args.CalibArgs, 'FluxColName', 'FLUX_APER_3');
    JP = PhotCalibTrans.poolForJointFit(PC_array, ...
        'FluxColName',           FluxColName, ...
        'ComputeSpecFluxMatrix', true, ...
        'Verbose',               Args.Verbose);

    % If the joint pool MagErr is missing or has NaNs, re-propagate on the
    % pool using PC(1)'s atm model as a stand-in (simplest correct choice
    % for the joint pool, since the propagation is mostly spectral-shape
    % driven and the per-crop atm pieces would average out anyway).
    if any(~isfinite(JP.MagErr))
        if Args.Verbose
            fprintf('  Re-propagating MagErr on the pool (some PC slices were missing it)\n');
        end
        try
            JP.MagErr = PC_array(1).propagateCalibratorMagErr(JP.Flux, JP.FluxErr, ...
                'WeightingMode', Args.WeightingMode, ...
                'ExpTime',       JP.ExpTime_eff);
        catch ME
            warning('PhotCalibTrans:fitJointVisit:MagErrFallback', ...
                'propagateCalibratorMagErr failed (%s) — using unweighted fit.', ME.message);
            JP.MagErr = [];
        end
    end

    % ----- Step 4: build the joint CompositeFun ----------------------------
    if Args.Verbose
        fprintf('Step 4: build joint CompositeFun (field-frame Tran2D)\n');
    end
    % CompositeFun is a handle. We share PC(1)'s CompositeFun and swap
    % its Tran2DObj to field frame. The original per-crop Tran2DObj is
    % discarded (we don't need it back — broadcast in Step 6 overwrites
    % every PC's TransModel handle anyway).
    JointCF = PC_array(1).TransModel;
    JointCF.Tran2DObj = FieldT2D;

    % ----- Step 5: fitMultiStage on the pooled data ------------------------
    if Args.Verbose
        fprintf('Step 5: fitMultiStage on pool (OptSeq=%s)\n', Args.OptSeqName);
    end
    CostArgs = { ...
        'WeightMatrix',              JP.Spec, ...
        'PrecomputedMagErr',         JP.MagErr, ...
        'PrecomputedSpecFluxMatrix', JP.SpecFluxMatrix, ...
        'TransmissionMode',          true, ...
        'CalibWavelength',           JP.SpecWvl, ...
        'ExpTime',                   JP.ExpTime_eff, ...
        'Aperture_area_m2',          JP.Aperture, ...
        'X',                         JP.XFULL, ...
        'Y',                         JP.YFULL};

    [FitResult, JointCF] = JointCF.fitMultiStage( ...
        JP.TransWvl, JP.Flux, CostArgs{:}, ...
        'OptSeqName',         Args.OptSeqName, ...
        'OuterMaxIter',       Args.OuterMaxIter, ...
        'OuterSigmaClip',     Args.OuterSigmaClip, ...
        'OuterSigmaThresh',   Args.OuterSigmaThresh, ...
        'OuterStdFunc',       Args.OuterStdFunc, ...
        'WeightedOuterIters', Args.WeightedOuterIters, ...
        'Verbose',            Args.Verbose);

    % ----- Step 6: broadcast JointCF to every PC ---------------------------
    if Args.Verbose
        fprintf('Step 6: broadcast joint CompositeFun to %d PCs\n', Ncrops);
    end
    PC_array = PhotCalibTrans.broadcastJointFitResult(PC_array, JointCF);

    % ----- Return ----------------------------------------------------------
    Result = struct();
    Result.JointCF        = JointCF;
    Result.FieldTran2DObj = FieldT2D;
    Result.FitResult      = FitResult;
    Result.Pool           = Pool;
    Result.JointPool      = JP;

    if Args.Verbose
        fprintf('=== fitJointVisit done: NCalUsed=%d, RMS=%.4f mag ===\n', ...
                FitResult.NCalUsed, FitResult.RMS);
    end
end

% =========================================================================
function Val = pickArg(NVCell, Name, Default)
    Val = Default;
    for K = 1:2:numel(NVCell)
        if ischar(NVCell{K}) && strcmp(NVCell{K}, Name)
            Val = NVCell{K+1};
            return;
        end
    end
end
