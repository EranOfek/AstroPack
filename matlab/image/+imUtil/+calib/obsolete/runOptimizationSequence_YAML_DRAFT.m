function [SequenceResults, FinalCalibData, FinalFieldParams] = runOptimizationSequence_YAML_DRAFT(CalibData, TransFun, YAMLConfig, Args)
    % Execute multi-stage optimization sequence defined in YAML configuration
    %
    % Input  : - CalibData - Structure with calibrator data (from findCalibrators)
    %          - TransFun - CompositeFun object with transmission functions
    %          - YAMLConfig - YAML configuration structure (or path to file)
    %          * ...,key,val,...
    %            'Verbose' - Enable verbose output. Default is true.
    %
    % Output : - SequenceResults - Cell array with results from each stage
    %          - FinalCalibData - Updated CalibData after sigma clipping
    %          - FinalFieldParams - Final field correction parameters [1 x 10]
    %
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example:
    %   [Results, CalibData] = imUtil.calib.runOptimizationSequence_YAML_DRAFT(...
    %                              CalibData, TransFun, YAMLConfig);

    arguments
        CalibData struct
        TransFun  % tools.math.fun.CompositeFun
        YAMLConfig  % Structure or path string
        Args.Verbose logical = true
    end

    % ====================================================================
    % STEP 1: LOAD YAML CONFIG IF NEEDED
    % ====================================================================

    if ischar(YAMLConfig) || isstring(YAMLConfig)
        YAMLFile = YAMLConfig;
        if startsWith(YAMLFile, '~')
            YAMLFile = strrep(YAMLFile, '~', getenv('HOME'));
        end
        YAMLConfig = yaml.ReadYaml(YAMLFile);
    end

    if ~isfield(YAMLConfig, 'OptimizationSequence')
        error('YAML config missing required field: OptimizationSequence');
    end

    % ====================================================================
    % STEP 2: SETUP
    % ====================================================================

    OptSequence = YAMLConfig.OptimizationSequence;
    NumStages = length(OptSequence);

    SequenceResults = cell(NumStages, 1);
    CurrentCalibData = CalibData;

    % Get initial parameter values from TransFun
    CurrentParamValues = TransFun.valuesAllPar();

    % Get initial field correction parameters from YAML
    if isfield(YAMLConfig, 'FieldCorrectionParams')
        FieldParams = YAMLConfig.FieldCorrectionParams.values;
        % Ensure it's a row vector
        if iscell(FieldParams)
            FieldParams = cell2mat(FieldParams);
        end
        if iscolumn(FieldParams)
            FieldParams = FieldParams';
        end
        CurrentFieldParams = FieldParams;
    else
        % Default: 10 zeros if not specified
        CurrentFieldParams = zeros(1, 10);
    end

    if Args.Verbose
        fprintf('\n=== RUNNING OPTIMIZATION SEQUENCE (%d STAGES) ===\n\n', NumStages);
    end

    % ====================================================================
    % STEP 3: LOOP OVER OPTIMIZATION STAGES
    % ====================================================================

    for iStage = 1:NumStages
        Stage = OptSequence{iStage};

        StageName = Stage.stagename;
        Method = Stage.method;
        FreeParamsSpec = Stage.freeparams;  % Cell array of [FunctionName, ParamName] pairs
        SigmaClip = Stage.sigmaclip;
        SigmaThresh = Stage.sigmathresh;
        SigmaIter = Stage.sigmaiter;

        if Args.Verbose
            fprintf('--- Stage %d/%d: %s [%s] ---\n', iStage, NumStages, StageName, Method);
            fprintf('Description: %s\n', Stage.description);
        end

        % ----------------------------------------------------------------
        % STEP 3.1: Detect field correction stage (empty freeparams)
        % ----------------------------------------------------------------

        IsFieldCorrectionStage = isempty(FreeParamsSpec) || (iscell(FreeParamsSpec) && isempty([FreeParamsSpec{:}]));

        if IsFieldCorrectionStage
            % Field correction stage: optimize all 10 field parameters
            if Args.Verbose
                fprintf('Field correction stage: optimizing 10 spatial parameters\n');
            end
            FreeParamIndices = [];  % Not used for field correction
        else
            % ----------------------------------------------------------------
            % STEP 3.2: Map free parameters to global indices
            % ----------------------------------------------------------------

            FreeParamIndices = mapParametersToIndices(TransFun, YAMLConfig, FreeParamsSpec);

            if Args.Verbose
                fprintf('Free parameters: %d (global indices: %s)\n', ...
                        length(FreeParamIndices), mat2str(FreeParamIndices));
            end

            % ----------------------------------------------------------------
            % STEP 3.3: Set FitPar flags for this stage
            % ----------------------------------------------------------------

            setFitParForStage(TransFun, FreeParamIndices);
        end

        % ----------------------------------------------------------------
        % STEP 3.4: Run minimizer
        % ----------------------------------------------------------------

        StageStruct = struct();
        StageStruct.Name = StageName;
        StageStruct.FreeParams = FreeParamIndices;
        StageStruct.Method = Method;
        StageStruct.SigmaClipping = SigmaClip;
        StageStruct.SigmaThreshold = SigmaThresh;
        StageStruct.SigmaIterations = SigmaIter;
        StageStruct.Description = Stage.description;
        StageStruct.IsFieldCorrection = IsFieldCorrectionStage;

        % Add regularization if present (for linear stages)
        if isfield(Stage, 'regularization')
            StageStruct.Regularization = Stage.regularization;
        end

        if strcmp(Method, 'nonlinear')
            % Nonlinear minimizer
            [OptParams, Fval, ExitFlag, Output, ResultData] = ...
                imUtil.calib.minimizeNonlinear_DRAFT(CurrentCalibData, CurrentParamValues, TransFun, StageStruct, ...
                    'FieldParams', CurrentFieldParams, 'Verbose', Args.Verbose);

        elseif strcmp(Method, 'linear')
            % Linear minimizer
            [OptParams, Fval, ExitFlag, Output, ResultData] = ...
                imUtil.calib.minimizeLinear_DRAFT(CurrentCalibData, CurrentParamValues, TransFun, StageStruct, ...
                    'FieldParams', CurrentFieldParams, 'Verbose', Args.Verbose);
        else
            error('Unknown optimization method: %s', Method);
        end

        % ----------------------------------------------------------------
        % STEP 3.5: Update parameter values
        % ----------------------------------------------------------------

        if IsFieldCorrectionStage
            % Update field correction parameters
            CurrentFieldParams = ResultData.OptimizedFieldParams;
            if Args.Verbose
                fprintf('Updated field parameters: [%.4f, %.4f, ...]\n', ...
                        CurrentFieldParams(1), CurrentFieldParams(2));
            end
        else
            % Update current transmission parameter values with optimized values
            CurrentParamValues = ResultData.AllParamValues;

            % Update TransFun object with optimized parameters
            AllPar = TransFun.getAllParStruct();
            AllPar.Values = CurrentParamValues;
            TransFun.setAllParStruct(AllPar);
        end

        % ----------------------------------------------------------------
        % STEP 3.5: Store stage results
        % ----------------------------------------------------------------

        StageResult = struct();
        StageResult.StageName = StageName;
        StageResult.Method = Method;
        StageResult.OptimizedParams = OptParams;
        StageResult.Fval = Fval;
        StageResult.ExitFlag = ExitFlag;
        StageResult.Output = Output;
        StageResult.Residuals = ResultData.Residuals;
        StageResult.RMS = ResultData.RMS;
        StageResult.NumCalibrators = ResultData.NumCalibrators;
        StageResult.SigmaClipping = SigmaClip;

        SequenceResults{iStage} = StageResult;

        % Update CalibData for next stage (after sigma clipping)
        CurrentCalibData = ResultData.CalibData;

        if Args.Verbose
            fprintf('Stage complete: RMS=%.4f mag, Calibrators=%d\n\n', ...
                    ResultData.RMS, ResultData.NumCalibrators);
        end
    end

    % ====================================================================
    % STEP 4: FINALIZE
    % ====================================================================

    FinalCalibData = CurrentCalibData;
    FinalFieldParams = CurrentFieldParams;

    if Args.Verbose
        fprintf('=== OPTIMIZATION SEQUENCE COMPLETE ===\n\n');
        fprintf('Final field correction parameters: [%.6f, %.6f, ...]\n', ...
                FinalFieldParams(1), FinalFieldParams(2));
    end
end

%% ========================================================================
%  HELPER FUNCTION: Map parameter names to global indices
%  ========================================================================

function GlobalIndices = mapParametersToIndices(TransFun, YAMLConfig, FreeParamsSpec)
    % Map [FunctionName, ParameterName] pairs to global parameter indices
    %
    % Input  : - TransFun - CompositeFun object
    %          - YAMLConfig - YAML configuration structure
    %          - FreeParamsSpec - Cell array of [FunctionName, ParamName] pairs
    %                             Example: {{'Water', 'PWV_cm'}, {'Aerosol', 'TauAod500'}}
    %
    % Output : - GlobalIndices - Vector of global parameter indices

    NumParams = length(FreeParamsSpec);
    GlobalIndices = zeros(1, NumParams);

    TransFunctions = YAMLConfig.TransmissionFunctions;

    for i = 1:NumParams
        % Extract function name and parameter name
        ParamPair = FreeParamsSpec{i};
        FunctionName = ParamPair{1};
        ParameterName = ParamPair{2};

        % Find function index in YAML
        FunIdx = [];
        for j = 1:length(TransFunctions)
            if strcmp(TransFunctions{j}.name, FunctionName)
                FunIdx = j;
                break;
            end
        end

        if isempty(FunIdx)
            error('Function "%s" not found in YAML TransmissionFunctions', FunctionName);
        end

        % Find parameter index within function
        ParamInfo = TransFunctions{FunIdx}.paraminfo;
        ParamIdx = [];
        for j = 1:length(ParamInfo)
            if strcmp(ParamInfo{j}.name, ParameterName)
                ParamIdx = j;
                break;
            end
        end

        if isempty(ParamIdx)
            error('Parameter "%s" not found in function "%s"', ParameterName, FunctionName);
        end

        % Get global index from ArgMapping
        GlobalIndices(i) = TransFun.Funs(FunIdx).ArgMapping(ParamIdx);
    end
end

%% ========================================================================
%  HELPER FUNCTION: Set FitPar flags for stage
%  ========================================================================

function setFitParForStage(TransFun, FreeParamIndices)
    % Set FitPar flags: true for free parameters, false for all others
    %
    % Input  : - TransFun - CompositeFun object (modified in-place)
    %          - FreeParamIndices - Vector of global parameter indices to fit

    % Update FitPar in each function using ArgMapping
    for iFun = 1:length(TransFun.Funs)
        NumParams = length(TransFun.Funs(iFun).Par);

        % Get global indices for this function from ArgMapping
        GlobalIndices = TransFun.Funs(iFun).ArgMapping;

        % Initialize FitPar to false for this function
        FunFitPar = false(NumParams, 1);

        % Set FitPar to true for parameters that are in FreeParamIndices
        for j = 1:NumParams
            GlobalIdx = GlobalIndices(j);
            if ismember(GlobalIdx, FreeParamIndices)
                FunFitPar(j) = true;
            end
        end

        % Update function FitPar
        TransFun.Funs(iFun).FitPar = FunFitPar;
    end
end
