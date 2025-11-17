function [OptimalParams, Fval, ExitFlag, Output, ResultData] = minimizeLinear_DRAFT(CalibData, ParamValues, TransFun, Stage, Args)
    % Linear least squares minimizer for field correction parameters
    % This function optimizes field correction parameters using a closed-form linear
    % least squares solution. It builds a design matrix from Chebyshev basis functions
    % and solves the system to minimize magnitude residuals.
    % OPTIMIZED VERSION: Vectorized cell array indexing in sigma clipping.
    %
    % Input  : - CalibData - Structure with calibrator data (see calculateCostFunction_DRAFT).
    %          - ParamValues - Current parameter values vector (all parameters).
    %          - TransFun - CompositeFun object (read-only, used for structure only).
    %          - Stage - Optimization stage structure with fields:
    %            .Name - Stage name (string)
    %            .FreeParams - Global parameter indices to optimize (vector)
    %            .SigmaClipping - Enable sigma clipping (logical)
    %            .SigmaThreshold - Sigma threshold for outlier removal (double)
    %            .SigmaIterations - Number of sigma clipping iterations (double)
    %          * ...,key,val,...
    %            'FieldParams' - Field correction parameters [1 x 10].
    %                   Default is zeros(1, 10).
    %            'Verbose' - Enable verbose output. Default is false.
    %            'Regularization' - L2 regularization parameter. Default is 0.
    %
    % Output : - OptimalParams - Structure with optimized parameter values
    %                            (global param indices as field names: 'Param_1', 'Param_3', etc.).
    %          - Fval - Final cost function value (sum of squared residuals).
    %          - ExitFlag - Exit flag (1=success, 0=failure).
    %          - Output - Output structure with solver details.
    %          - ResultData - Structure with:
    %            .CalibData - Final calibrator data (after sigma clipping)
    %            .Residuals - Final residuals (magnitude differences)
    %            .RMS - RMS of residuals
    %            .NumCalibrators - Number of calibrators used
    %            .AllParamValues - Full parameter vector (all parameters)
    %
    % Author : D. Kovaleva (Nov 2025)
    % Example: % Define stage
    %          Stage = struct('Name', "FieldCorrection", ...,
    %                         'FreeParams', [10, 11, 12, 13, 14], ...,
    %                         'SigmaClipping', true, ...,
    %                         'SigmaThreshold', 2.0, ...,
    %                         'SigmaIterations', 3);
    %          [OptParams, Fval] = imUtil.calib.minimizeLinear_DRAFT(...
    %              CalibData, TransFun, Stage, 'Verbose', true);

    arguments
        CalibData struct
        ParamValues double  % Current parameter values vector
        TransFun  % tools.math.fun.CompositeFun object (read-only)
        Stage struct
        Args.FieldParams = zeros(1, 10)  % Field correction parameters
        Args.Verbose logical = false
        Args.Regularization double = 0
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
        fprintf('=== LINEAR LEAST SQUARES MINIMIZATION: %s ===\n', Stage.Name);
        fprintf('Free parameters: [%s]\n', num2str(Stage.FreeParams));
        if Stage.SigmaClipping
            fprintf('Sigma clipping: ON (threshold=%.1f, iterations=%d)\n', ...
                    Stage.SigmaThreshold, Stage.SigmaIterations);
        else
            fprintf('Sigma clipping: OFF\n');
        end
        if Args.Regularization > 0
            fprintf('L2 regularization: λ = %.2e\n', Args.Regularization);
        end
    end

    % Check if this is field correction optimization
    if ~isfield(Stage, 'IsFieldCorrection') || ~Stage.IsFieldCorrection
        error('Linear minimizer currently only supports field correction optimization');
    end

    % Initialize current field parameters
    CurrentFieldParams = Args.FieldParams(:)';  % Ensure row vector (1 x 10)

    % Initialize current transmission parameters (remain fixed during field optimization)
    CurrentTransParams = ParamValues(:)';  % Ensure row vector

    if Args.Verbose
        fprintf('Optimizing 10 field correction parameters\n');
        fprintf('Transmission parameters: %d (fixed during this stage)\n', length(CurrentTransParams));
    end

    % Initialize results
    ExitFlag = 1;  % Assume success
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

            % Run linear optimization with current calibrator set
            [OptimizedFieldParams, Cov, Info, ExitFlagIter, OutputIter] = ...
                optimizeSingleIterationLinear(CurrentCalibData, CurrentTransParams, CurrentFieldParams, TransFun, ...
                                              Args.Regularization, Args.Verbose);

            % Update current field parameters
            CurrentFieldParams = OptimizedFieldParams;

            % Calculate residuals with optimized field parameters
            [~, Residuals, ~] = imUtil.calib.calculateCostFunction_DRAFT(...
                CurrentCalibData, CurrentTransParams, TransFun, ...
                'FieldParams', CurrentFieldParams, 'Verbose', false);

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

        % Final cost after sigma clipping
        Fval = Info.Chi2;

    else
        % Single optimization without sigma clipping
        [OptimizedFieldParams, Cov, Info, ExitFlag, Output] = ...
            optimizeSingleIterationLinear(CurrentCalibData, CurrentTransParams, CurrentFieldParams, TransFun, ...
                                          Args.Regularization, Args.Verbose);

        CurrentFieldParams = OptimizedFieldParams;
        Fval = Info.Chi2;
    end

    % ====================================================================
    % STEP 3: PREPARE OUTPUT
    % ====================================================================

    % Create OptimalParams structure (empty for field correction - params returned separately)
    OptimalParams = struct();

    % Calculate final residuals and statistics
    [~, FinalResiduals, ~] = imUtil.calib.calculateCostFunction_DRAFT(...
        CurrentCalibData, CurrentTransParams, TransFun, ...
        'FieldParams', CurrentFieldParams, 'Verbose', false);

    NumCalibrators = length(CurrentCalibData.Spec);
    RMS = sqrt(Fval / NumCalibrators);

    % Store result data
    ResultData = struct();
    ResultData.CalibData = CurrentCalibData;
    ResultData.Residuals = FinalResiduals;
    ResultData.RMS = RMS;
    ResultData.NumCalibrators = NumCalibrators;
    ResultData.AllParamValues = CurrentTransParams;  % Transmission params unchanged
    ResultData.OptimizedFieldParams = CurrentFieldParams;  % Field params optimized
    ResultData.Covariance = Cov;
    ResultData.Info = Info;

    if Args.Verbose
        fprintf('\n=== OPTIMIZATION COMPLETE ===\n');
        fprintf('Optimized field correction parameters:\n');
        FieldParamNames = {'kx0', 'kx', 'kx2', 'kx3', 'kx4', 'ky', 'ky2', 'ky3', 'ky4', 'kxy'};
        for i = 1:10
            fprintf('  [%d] %s: %.6f\n', i, FieldParamNames{i}, CurrentFieldParams(i));
        end
        fprintf('Final cost: %.4e\n', Fval);
        fprintf('RMS residual: %.4f mmag\n', RMS * 1000);
        fprintf('Calibrators used: %d\n', NumCalibrators);
        fprintf('Exit flag: %d\n', ExitFlag);
        if isfield(Info, 'ConditionNumber')
            fprintf('Condition number: %.2e\n', Info.ConditionNumber);
        end
        fprintf('============================\n\n');
    end
end

%% ========================================================================
%  HELPER FUNCTION: Single Linear Optimization Iteration
%  ========================================================================

function [OptimizedFieldParams, Cov, Info, ExitFlag, Output] = ...
    optimizeSingleIterationLinear(CalibData, TransParams, FieldParams, TransFun, Regularization, Verbose)
    % Run single linear least squares optimization iteration for field correction
    % This optimizes all 10 field correction parameters using Chebyshev basis.
    %
    % Input  : - CalibData - Calibrator data.
    %          - TransParams - Transmission parameters (fixed during this optimization).
    %          - FieldParams - Current field correction parameters (1 x 10).
    %          - TransFun - CompositeFun object.
    %          - Regularization - L2 regularization parameter.
    %          - Verbose - Verbose output flag.
    %
    % Output : - OptimizedFieldParams - Optimized field parameters (1 x 10).
    %          - Cov - Covariance matrix (10 x 10).
    %          - Info - Info structure with Chi2, Dof, etc.
    %          - ExitFlag - Exit flag (1=success).
    %          - Output - Output structure.

    % Step 1: Calculate base magnitude differences WITHOUT field correction
    % Use current transmission parameters and zero field parameters
    ZeroFieldParams = zeros(1, 10);

    [~, ~, BaseDiffMag] = imUtil.calib.calculateCostFunction_DRAFT(...
        CalibData, TransParams, TransFun, ...
        'FieldParams', ZeroFieldParams, 'Verbose', false);

    % Step 2: Build design matrix A from Chebyshev polynomials
    % Each column of A represents the contribution of one field correction parameter
    X_coord = CalibData.LASTData.X;
    Y_coord = CalibData.LASTData.Y;
    Coords = [X_coord(:), Y_coord(:)];

    A = buildChebyshevDesignMatrix(Coords);

    % Step 3: Solve linear system A * coeffs = -BaseDiffMag
    b = -BaseDiffMag;

    % Add regularization if requested
    if Regularization > 0
        nParams = size(A, 2);
        A_reg = [A; sqrt(Regularization) * eye(nParams)];
        b_reg = [b; zeros(nParams, 1)];
    else
        A_reg = A;
        b_reg = b;
    end

    % Solve using backslash operator
    try
        if Verbose
            fprintf('Solving linear system: %d calibrators, %d parameters\n', ...
                    size(A,1), size(A,2));
        end

        coeffs = A_reg \ b_reg;

        % Calculate condition number for diagnostics
        CondNum = cond(A_reg' * A_reg);

        if Verbose
            fprintf('Solution found. Condition number: %.2e\n', CondNum);
        end

        % Optimized field parameters are the solution coefficients
        % All 10 parameters (including kxy) are used directly from linear solution
        % NOTE: fieldCorrection.m now uses linear kxy (not kxy²), allowing negative values
        OptimizedFieldParams = coeffs(:)';  % Return as row vector (1 x 10)

        % Calculate final cost
        Residuals = A * coeffs + BaseDiffMag;
        Chi2 = sum(Residuals.^2);

        % Approximate covariance
        % Cov = σ^2 * (A'A)^-1
        NumCalib = length(BaseDiffMag);
        NumParams = 10;  % Always 10 field parameters
        Sigma2 = Chi2 / max(NumCalib - NumParams, 1);

        % Covariance matrix for field parameters (10 x 10)
        try
            Cov = Sigma2 * inv(A' * A + Regularization * eye(NumParams));
        catch
            % If inversion fails, return zeros
            if Verbose
                warning('Could not compute covariance matrix');
            end
            Cov = zeros(NumParams, NumParams);
        end

        % Info structure
        Info = struct();
        Info.Chi2 = Chi2;
        Info.Dof = NumCalib - NumParams;
        Info.Resid = Residuals;
        Info.Nobs = NumCalib;
        Info.ConditionNumber = CondNum;
        Info.Rank = rank(A_reg);

        ExitFlag = 1;  % Success
        Output = struct('message', 'Linear least squares converged', ...
                       'iterations', 1, ...
                       'funcCount', 1);

    catch ME
        error('Linear least squares optimization failed: %s', ME.message);
    end
end

%% ========================================================================
%  HELPER FUNCTION: Build Chebyshev Design Matrix
%  ========================================================================

function A = buildChebyshevDesignMatrix(Coords)
    % Build design matrix for field correction from Chebyshev polynomial basis
    % Returns a matrix with 10 columns corresponding to the 10 field parameters.
    %
    % Input  : - Coords - [N x 2] matrix [X_coord, Y_coord].
    %
    % Output : - A - Design matrix [NumCalibrators x 10].
    %
    % Column order matches fieldCorrection.m parameters:
    %   Columns 1-5: kx0, kx, kx2, kx3, kx4 → T0(x), T1(x), T2(x), T3(x), T4(x)
    %   Columns 6-9: ky, ky2, ky3, ky4 → T1(y), T2(y), T3(y), T4(y)
    %   Column 10: kxy → T1(x)*T1(y)
    %
    % NOTE: fieldCorrection.m now applies kxy * T1(x)*T1(y) (linear formulation).
    %       The optimized coefficient is used directly, allowing negative values.

    NumCalib = size(Coords, 1);
    A = zeros(NumCalib, 10);

    % Generate Chebyshev function for orders 0-4
    ChebyFun = tools.math.fun.chebyshevFun(1, [0, 1, 2, 3, 4]);

    % Normalize coordinates to [-1, 1] for LAST 1726x1726 detector
    MIN_COORD = 0;
    MAX_COORD = 1726;
    X_norm = 2 * (Coords(:, 1) - MIN_COORD) / (MAX_COORD - MIN_COORD) - 1;
    Y_norm = 2 * (Coords(:, 2) - MIN_COORD) / (MAX_COORD - MIN_COORD) - 1;

    % Evaluate Chebyshev polynomials
    % T: [NumCalib x 5] with columns [T0, T1, T2, T3, T4]
    T_x = ChebyFun(X_norm);
    T_y = ChebyFun(Y_norm);

    % Build design matrix columns
    % Columns 1-5: X terms (kx0 through kx4)
    A(:, 1:5) = T_x;

    % Columns 6-9: Y terms (ky through ky4)
    % Note: ky0 = 0 (hardcoded), so we only fit ky, ky2, ky3, ky4
    % These correspond to T1(y), T2(y), T3(y), T4(y)
    A(:, 6:9) = T_y(:, 2:5);  % Skip T0(y), use T1-T4

    % Column 10: Cross term (kxy)
    A(:, 10) = T_x(:, 2) .* T_y(:, 2);  % T1(x) * T1(y)
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
