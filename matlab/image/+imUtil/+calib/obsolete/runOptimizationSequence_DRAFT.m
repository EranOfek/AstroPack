function [SequenceResults, FinalCalibData] = runOptimizationSequence_DRAFT(CalibData, TransFun, Sequence, Args)
    % Execute multi-stage optimization sequence for transmission-based photometric calibration
    % This function orchestrates the sequential optimization of transmission parameters,
    % managing FitPar flags between stages and accumulating results.
    %
    % Input  : - CalibData - Structure with calibrator data (see calculateCostFunction_DRAFT)
    %          - TransFun - CompositeFun object with transmission functions
    %          - Sequence - CompositeMinSequence object with optimization stages
    %          * ...,key,val,...
    %            'Verbose' - Enable verbose output. Default is true.
    %            'OptimOptions' - optimoptions for lsqnonlin (nonlinear stages). Default is [].
    %
    % Output : - SequenceResults - Cell array with results from each stage:
    %                               {iStage}.StageName - Name of the stage
    %                               {iStage}.Method - Optimization method ('linear' or 'nonlinear')
    %                               {iStage}.OptParams - Optimized parameter values (struct)
    %                               {iStage}.Fval - Final cost function value
    %                               {iStage}.RMS - RMS of residuals (mmag)
    %                               {iStage}.NumCalibrators - Number of calibrators used
    %                               {iStage}.ExitFlag - Optimization exit flag
    %          - FinalCalibData - Final calibrator data after all stages (after sigma clipping)
    %
    % Author : D. Kovaleva (Nov 2025)
    % Example:
    %   % Create sequence
    %   Seq = tools.math.fun.CompositeMinSequence.createSimpleSequence();
    %
    %   % Run optimization sequence
    %   [Results, FinalCalibData] = imUtil.calib.runOptimizationSequence_DRAFT(...
    %       CalibData, TransFun, Seq, 'Verbose', true);
    %
    %   % Extract final parameters
    %   FinalParams = TransFun.getAllParStruct();

    arguments
        CalibData struct
        TransFun  % tools.math.fun.CompositeFun object
        Sequence  % tools.math.fun.CompositeMinSequence object
        Args.Verbose logical = true
        Args.OptimOptions = []
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS
    % ====================================================================

    % Validate TransFun
    if ~isa(TransFun, 'tools.math.fun.CompositeFun')
        error('TransFun must be a tools.math.fun.CompositeFun object');
    end

    % Validate Sequence
    if ~isa(Sequence, 'tools.math.fun.CompositeMinSequence')
        error('Sequence must be a tools.math.fun.CompositeMinSequence object');
    end

    NumStages = Sequence.numStages();
    if NumStages == 0
        error('Sequence has no stages defined');
    end

    if Args.Verbose
        fprintf('\n=== OPTIMIZATION SEQUENCE EXECUTION ===\n');
        fprintf('Total stages: %d\n', NumStages);
        fprintf('========================================\n\n');
    end

    % ====================================================================
    % STEP 2: INITIALIZE RESULTS STORAGE
    % ====================================================================

    SequenceResults = cell(NumStages, 1);
    CurrentCalibData = CalibData;

    % ====================================================================
    % STEP 3: LOOP THROUGH OPTIMIZATION STAGES
    % ====================================================================

    for iStage = 1:NumStages
        % Get stage definition
        Stage = Sequence.getStage(iStage);

        if Args.Verbose
            fprintf('\n========================================\n');
            fprintf('STAGE %d/%d: %s [%s]\n', iStage, NumStages, Stage.Name, Stage.Method);
            fprintf('========================================\n');
        end

        % --------------------------------------------------------------------
        % SUBSTEP A: Set FitPar flags for this stage
        % --------------------------------------------------------------------
        if Args.Verbose
            fprintf('Setting FitPar flags for free parameters: [%s]\n', num2str(Stage.FreeParams));
        end

        setFitParForStage(TransFun, Stage.FreeParams, Args.Verbose);

        % --------------------------------------------------------------------
        % SUBSTEP B: Call appropriate minimizer
        % --------------------------------------------------------------------

        if Stage.Method == "nonlinear"
            % Nonlinear optimization (uses lsqNonLinWithFixed)
            if Args.Verbose
                fprintf('Calling nonlinear minimizer...\n');
            end

            [OptParams, Fval, ExitFlag, Output, ResultData] = ...
                imUtil.calib.minimizeNonlinear_DRAFT(CurrentCalibData, TransFun, Stage, ...
                                                     'Verbose', Args.Verbose, ...
                                                     'OptimOptions', Args.OptimOptions);

        elseif Stage.Method == "linear"
            % Linear least squares optimization
            if Args.Verbose
                fprintf('Calling linear minimizer...\n');
            end

            [OptParams, Fval, ExitFlag, Output, ResultData] = ...
                imUtil.calib.minimizeLinear_DRAFT(CurrentCalibData, TransFun, Stage, ...
                                                   'Verbose', Args.Verbose);
        else
            error('Unknown optimization method: %s. Must be "linear" or "nonlinear".', Stage.Method);
        end

        % --------------------------------------------------------------------
        % SUBSTEP C: TransFun already updated by minimizer (handle class)
        % --------------------------------------------------------------------
        % minimizeNonlinear_DRAFT already calls TransFun.setAllParStruct()
        % Sigma clipping already handled inside minimizer

        % --------------------------------------------------------------------
        % SUBSTEP D: Store stage results
        % --------------------------------------------------------------------

        SequenceResults{iStage} = struct();
        SequenceResults{iStage}.StageName = Stage.Name;
        SequenceResults{iStage}.Method = Stage.Method;
        SequenceResults{iStage}.FreeParams = Stage.FreeParams;
        SequenceResults{iStage}.OptParams = OptParams;
        SequenceResults{iStage}.Fval = Fval;
        SequenceResults{iStage}.RMS = ResultData.RMS;
        SequenceResults{iStage}.NumCalibrators = ResultData.NumCalibrators;
        SequenceResults{iStage}.ExitFlag = ExitFlag;
        SequenceResults{iStage}.Output = Output;

        if Args.Verbose
            fprintf('\n--- Stage %d Results ---\n', iStage);
            fprintf('RMS: %.4f mmag\n', ResultData.RMS * 1000);
            fprintf('Calibrators: %d\n', ResultData.NumCalibrators);
            fprintf('Exit flag: %d\n', ExitFlag);
            fprintf('Cost: %.4e\n', Fval);
        end

        % --------------------------------------------------------------------
        % SUBSTEP E: Update calibrator data for next stage
        % --------------------------------------------------------------------
        % After sigma clipping, some calibrators may have been removed
        CurrentCalibData = ResultData.CalibData;

        if Args.Verbose && iStage < NumStages
            fprintf('\nCalibrators passed to next stage: %d\n', length(CurrentCalibData.Spec));
        end
    end

    % ====================================================================
    % STEP 4: FINALIZE
    % ====================================================================

    FinalCalibData = CurrentCalibData;

    if Args.Verbose
        fprintf('\n========================================\n');
        fprintf('SEQUENCE COMPLETE\n');
        fprintf('========================================\n');
        fprintf('Final number of calibrators: %d\n', length(FinalCalibData.Spec));

        % Print summary of all stages
        fprintf('\n--- Sequence Summary ---\n');
        for iStage = 1:NumStages
            fprintf('%d. %s: RMS=%.4f mmag, N=%d\n', ...
                    iStage, SequenceResults{iStage}.StageName, ...
                    SequenceResults{iStage}.RMS * 1000, ...
                    SequenceResults{iStage}.NumCalibrators);
        end
        fprintf('\n');
    end
end

%% ========================================================================
%  HELPER FUNCTION: Set FitPar Flags for Current Stage
%  ========================================================================

function setFitParForStage(TransFun, FreeParams, Verbose)
    % Set FitPar flags in CompositeFun: true for FreeParams, false for others
    % Input  : - TransFun - CompositeFun object (handle, modified in-place)
    %          - FreeParams - Global parameter indices to mark as fitted [vector]
    %          - Verbose - Enable verbose output (logical)
    % Output : - None (modifies TransFun in-place)

    % Get current parameter structure
    AllPar = TransFun.getAllParStruct();
    TotalParams = length(AllPar.Values);

    % Validate FreeParams
    if any(FreeParams < 1) || any(FreeParams > TotalParams)
        error('FreeParams contains invalid indices. Valid range: 1-%d', TotalParams);
    end

    % Create new FitPar mask: false for all, true for FreeParams
    NewFitPar = false(TotalParams, 1);
    NewFitPar(FreeParams) = true;

    % Update FitPar in structure
    AllPar.FitPar = NewFitPar;

    % Set updated structure back to CompositeFun
    TransFun.setAllParStruct(AllPar);

    if Verbose
        fprintf('FitPar flags set: %d parameters marked for fitting, %d fixed\n', ...
                sum(NewFitPar), sum(~NewFitPar));
    end
end
