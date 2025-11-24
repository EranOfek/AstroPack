function [OptimalParams, Fval, ExitFlag, Output, ResultData] = minimizeNonlinear(CalibData, ParamValues, TransFun, Stage, Args)
    % Nonlinear least squares optimizer for transmission parameters
    % Input  : - CalibData - Structure with calibrator data (see calculateCostFunction) with fields:
    %                   .Spec - Cell array of Gaia XP spectra
    %                   .LASTData - Table with LAST catalog data
    %                   .Metadata - Observation metadata
    %          - ParamValues - Current parameter values vector (all parameters).
    %          - TransFun - CompositeFun object (read-only, used for structure only).
    %          - Stage - Optimization stage structure with fields:
    %                   .Name - Stage name (string)
    %                   .FreeParams - Global parameter indices to optimize (vector)
    %                   .SigmaClipping - Enable sigma clipping (logical)
    %                   .SigmaThreshold - Sigma threshold for outlier removal
    %                   .SigmaIterations - Number of sigma clipping iterations
    %          * ...,key,val,...
    %            'FieldParams' - Field correction parameters [1 x 10].
    %                   Default is zeros(1, 10).
    %            'Verbose' - Enable verbose output.
    %                   Default is false.
    %            'OptimOptions' - optimoptions for lsqnonlin.
    %                   Default is [].
    % Output : - OptimalParams - Structure with optimized parameter values
    %                   (global param indices as field names: 'Param_1', 'Param_3', etc.).
    %          - Fval - Final cost function value (sum of squared residuals).
    %          - ExitFlag - Exit flag from lsqnonlin.
    %          - Output - Output structure from lsqnonlin.
    %          - ResultData - Structure with fields:
    %                   .CalibData - Final calibrator data (after sigma clipping)
    %                   .Residuals - Final residuals (magnitude differences)
    %                   .RMS - RMS of residuals [mag]
    %                   .NumCalibrators - Number of calibrators used
    %                   .AllParamValues - Full parameter vector (all parameters)
    % Author : D. Kovaleva (Nov 2025)
    % Example: Stage = struct('Name', "Normalization", 'FreeParams', [1], ...
    %                         'SigmaClipping', true, 'SigmaThreshold', 3.0, ...
    %                         'SigmaIterations', 3);
    %          [OptParams, Fval] = imUtil.calib.minimizeNonlinear(...
    %              CalibData, ParamVals, TransFun, Stage, 'Verbose', true);

    arguments
        CalibData struct
        ParamValues double  % Current parameter values vector
        TransFun  % tools.math.fun.CompositeFun object (read-only)
        Stage struct
        Args.FieldParams = zeros(1, 10)  % Field correction parameters
        Args.Verbose logical = false
        Args.OptimOptions = []
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS AND SETUP
    % ====================================================================

    % Validate TransFun
    if ~isa(TransFun, 'tools.math.fun.CompositeFun')
        error('TransFun must be a tools.math.fun.CompositeFun object');
    end

    % Validate Stage structure
    RequiredFields = {'Name', 'FreeParams', 'SigmaClipping', 'SigmaThreshold', 'SigmaIterations'};
    for i = 1:length(RequiredFields)
        if ~isfield(Stage, RequiredFields{i})
            error('Stage missing required field: %s', RequiredFields{i});
        end
    end

    if Args.Verbose
        fprintf('=== NONLINEAR MINIMIZATION: %s ===\n', Stage.Name);
        fprintf('Free parameters: [%s]\n', num2str(Stage.FreeParams));
        if Stage.SigmaClipping
            fprintf('Sigma clipping: ON (threshold=%.1f, iterations=%d)\n', ...
                    Stage.SigmaThreshold, Stage.SigmaIterations);
        else
            fprintf('Sigma clipping: OFF\n');
        end
    end

    % Get parameter structure from CompositeFun (for bounds and metadata only)
    AllPar = TransFun.getAllParStruct();
    TotalParams = AllPar.TotalParams;

    % Validate ParamValues
    if length(ParamValues) ~= TotalParams
        error('ParamValues length (%d) does not match TotalParams (%d)', ...
              length(ParamValues), TotalParams);
    end

    % Initialize current parameter values
    CurrentParamValues = ParamValues(:);  % Ensure column vector

    % Create fit mask for lsqNonLinWithFixed
    FitMask = false(TotalParams, 1);
    FitMask(Stage.FreeParams) = true;

    if Args.Verbose
        fprintf('Total parameters: %d, Fitting: %d, Fixed: %d\n', ...
                TotalParams, sum(FitMask), sum(~FitMask));
    end

    % Setup optimization options
    if isempty(Args.OptimOptions)
        Args.OptimOptions = optimoptions('lsqnonlin', ...
            'Display', 'off', ...
            'MaxIterations', 200, ...
            'MaxFunctionEvaluations', 1000, ...
            'FunctionTolerance', 1e-6, ...
            'StepTolerance', 1e-6);

        if Args.Verbose
            Args.OptimOptions.Display = 'iter';
        end
    end

    % Initialize results
    ExitFlag = 0;
    Output = struct();

    % ====================================================================
    % STEP 2: SIGMA CLIPPING LOOP (if enabled)
    % ====================================================================

    CurrentCalibData = CalibData;

    if Stage.SigmaClipping
        for SigmaIter = 1:Stage.SigmaIterations
            if Args.Verbose
                fprintf('\n--- Sigma Clipping Iteration %d/%d ---\n', ...
                        SigmaIter, Stage.SigmaIterations);
                fprintf('Current calibrators: %d\n', length(CurrentCalibData.Spec));
            end

            % Run optimization with current calibrator set
            [OptimizedPar, Cov, Info, ExitFlag, Output] = ...
                optimizeSingleIteration(CurrentCalibData, CurrentParamValues, TransFun, AllPar, FitMask, ...
                                       Args.FieldParams, Args.OptimOptions, Args.Verbose);

            % Update current parameter values
            CurrentParamValues = OptimizedPar;

            % Calculate residuals with optimized parameters
            [~, Residuals, ~] = imUtil.calib.calculateCostFunction(...
                CurrentCalibData, CurrentParamValues, TransFun, ...
                'FieldParams', Args.FieldParams, 'Verbose', false);

            % Apply sigma clipping
            [ClippedData, OutlierMask] = applySigmaClipping(...
                CurrentCalibData, Residuals, Stage.SigmaThreshold);

            NumOutliers = sum(OutlierMask);
            if Args.Verbose
                fprintf('Outliers removed: %d (%.1f%%)\n', ...
                        NumOutliers, 100*NumOutliers/length(Residuals));
            end

            % Check convergence
            if NumOutliers == 0
                if Args.Verbose
                    fprintf('No outliers found. Sigma clipping converged.\n');
                end
                break;
            end

            % Update calibrator data for next iteration
            CurrentCalibData = ClippedData;
        end

        % Final RMS after sigma clipping
        Fval = Info.Chi2;

    else
        % Single optimization without sigma clipping
        [OptimizedPar, Cov, Info, ExitFlag, Output] = ...
            optimizeSingleIteration(CurrentCalibData, CurrentParamValues, TransFun, AllPar, FitMask, ...
                                   Args.FieldParams, Args.OptimOptions, Args.Verbose);

        CurrentParamValues = OptimizedPar;
        Fval = Info.Chi2;
    end

    % ====================================================================
    % STEP 3: PREPARE OUTPUT
    % ====================================================================

    % Create OptimalParams structure with global parameter indices as field names
    OptimalParams = struct();
    for i = 1:length(Stage.FreeParams)
        ParamIdx = Stage.FreeParams(i);
        FieldName = sprintf('Param_%d', ParamIdx);
        OptimalParams.(FieldName) = CurrentParamValues(ParamIdx);
    end

    % Calculate final residuals and statistics
    [~, FinalResiduals, ~] = imUtil.calib.calculateCostFunction(...
        CurrentCalibData, CurrentParamValues, TransFun, ...
        'FieldParams', Args.FieldParams, 'Verbose', false);

    NumCalibrators = length(CurrentCalibData.Spec);
    RMS = sqrt(Fval / NumCalibrators);

    % Store result data
    ResultData = struct();
    ResultData.CalibData = CurrentCalibData;
    ResultData.Residuals = FinalResiduals;
    ResultData.RMS = RMS;
    ResultData.NumCalibrators = NumCalibrators;
    ResultData.AllParamValues = CurrentParamValues;
    ResultData.Covariance = Cov;
    ResultData.Info = Info;

    if Args.Verbose
        fprintf('\n=== OPTIMIZATION COMPLETE ===\n');
        fprintf('Optimized parameters:\n');
        ParamNames = AllPar.Names;
        for i = 1:length(Stage.FreeParams)
            ParamIdx = Stage.FreeParams(i);
            fprintf('  [%d] %s: %.6f\n', ParamIdx, ParamNames{ParamIdx}, ...
                    CurrentParamValues(ParamIdx));
        end
        fprintf('Final cost: %.4e\n', Fval);
        fprintf('RMS residual: %.4f mmag\n', RMS * 1000);
        fprintf('Calibrators used: %d\n', NumCalibrators);
        fprintf('Exit flag: %d\n', ExitFlag);
        fprintf('============================\n\n');
    end
end

%% ========================================================================
%  HELPER FUNCTION: Single Optimization Iteration
%  ========================================================================

function [OptimizedPar, Cov, Info, ExitFlag, Output] = ...
    optimizeSingleIteration(CalibData, ParamValues, TransFun, AllPar, FitMask, FieldParams, OptimOptions, Verbose)
    % Run single lsqNonLinWithFixed optimization iteration
    %
    % Input  : - CalibData - Calibrator data structure.
    %          - ParamValues - Current parameter values to use as initial guess.
    %          - TransFun - CompositeFun object.
    %          - AllPar - Parameter structure from TransFun.
    %          - FitMask - Logical mask indicating which parameters to fit.
    %          - FieldParams - Field correction parameters (passed through to cost function).
    %          - OptimOptions - Optimization options for lsqnonlin.
    %          - Verbose - Verbose output flag.
    %
    % Output : - OptimizedPar - Optimized parameter values [N x 1].
    %          - Cov - Covariance matrix.
    %          - Info - Information structure from optimizer.
    %          - ExitFlag - Exit flag (1=success).
    %          - Output - Output structure.

    NumCalibrators = length(CalibData.Spec);

    % Define model function for lsqNonLinWithFixed
    % We're minimizing magnitude differences DiffMag = 2.5*log10(PredictedFlux/ObservedFlux)
    % The model returns DiffMag for each calibrator, and we fit to Y = 0
    Model = @(Lambda, P) getDiffMag(CalibData, P, TransFun, FieldParams);

    % Local helper to extract DiffMag from cost function
    function DiffMag = getDiffMag(CalibData, P, TransFun, FieldParams)
        [~, ~, DiffMag, ~] = imUtil.calib.calculateCostFunction(...
            CalibData, P, TransFun, 'FieldParams', FieldParams, 'Verbose', false);
    end

    % Target: minimize magnitude differences (fit to zero)
    Y = zeros(NumCalibrators, 1);

    % Weights (uniform for now - could be based on magnitude errors)
    Sigma = ones(NumCalibrators, 1);

    % Wavelength grid (dummy X for lsqNonLinWithFixed - actual wavelengths handled internally)
    X = (1:NumCalibrators)';  % Just calibrator indices

    % Initial parameters, bounds
    InitPar = ParamValues(:);  % Use provided parameter values as initial guess
    Lb = AllPar.Min;
    Ub = AllPar.Max;

    % Validate model function returns correct size
    if Verbose
        [~, ~, TestDiffMag, ~] = imUtil.calib.calculateCostFunction(...
            CalibData, InitPar, TransFun, 'FieldParams', FieldParams, 'Verbose', false);
        if size(TestDiffMag, 1) ~= NumCalibrators || size(TestDiffMag, 2) ~= 1
            error('Model function returns wrong size! Got [%d x %d], expected [%d x 1]', ...
                  size(TestDiffMag, 1), size(TestDiffMag, 2), NumCalibrators);
        end
    end

    % Call lsqNonLinWithFixed
    try
        [OptimizedPar, Cov, Info] = (...
            X, Y, Sigma, Model, ...
            'InitPar', InitPar, ...
            'FitPar', FitMask, ...
            'Lb', Lb, ...
            'Ub', Ub, ...
            'Opts', OptimOptions);

        ExitFlag = 1;  % Success
        Output = struct('message', 'Optimization completed', ...
                       'iterations', Info.Nobs);  % Placeholder

    catch ME
        error('Nonlinear optimization failed: %s', ME.message);
    end
end

%% ========================================================================
%  HELPER FUNCTION: Sigma Clipping (OPTIMIZED)
%  ========================================================================

function [ClippedData, OutlierMask] = applySigmaClipping(CalibData, Residuals, Threshold)
    % Apply sigma clipping to remove outliers using robust statistics
    % OPTIMIZED VERSION: Direct cell array indexing instead of nested loops.
    %
    % Input  : - CalibData - Calibrator data structure.
    %          - Residuals - Residuals (magnitude differences) [N x 1].
    %          - Threshold - Sigma threshold for outlier removal.
    %
    % Output : - ClippedData - CalibData with outliers removed.
    %          - OutlierMask - Logical mask: true for outliers, false for good data.

    % Validate input sizes
    NumCalib = size(CalibData.Spec, 1);
    NumResid = length(Residuals);

    if NumResid ~= NumCalib
        error('Residuals size (%d) does not match CalibData.Spec size (%d)', ...
              NumResid, NumCalib);
    end

    % Calculate robust statistics using median and MAD
    MedianResid = median(Residuals);

    % Use robust standard deviation (MAD-based)
    MAD = median(abs(Residuals - MedianResid));
    RobustStd = 1.4826 * MAD;  % Conversion factor for normal distribution

    % Identify outliers based on robust statistics
    OutlierMask = abs(Residuals - MedianResid) > Threshold * RobustStd;

    % Create mask for good (non-outlier) data
    GoodMask = ~OutlierMask;

    % ----------------------------------------------------------------
    % OPTIMIZATION: Direct cell array indexing
    % Instead of nested loops, use MATLAB's built-in cell array indexing
    % ----------------------------------------------------------------

    ClippedData = struct();

    % Direct indexing of cell array using logical mask
    % This is much faster than manually looping and copying elements
    ClippedData.Spec = CalibData.Spec(GoodMask, :);

    % Index structure array and table using logical mask
    ClippedData.Coords = CalibData.Coords(GoodMask);  % Structure array
    ClippedData.LASTData = CalibData.LASTData(GoodMask, :);  % Table
    ClippedData.Metadata = CalibData.Metadata;  % Keep metadata unchanged
end
