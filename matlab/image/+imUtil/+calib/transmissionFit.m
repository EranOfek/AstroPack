function [Model, FieldParams, Results] = transmissionFit(Lambda, Spec, Flux, FluxErr, X, Y, PolyCheb, Args)
    % Transmission-based photometric calibration: optimization
    % Input  : - Lambda - Wavelength grid for integration range [N_lambda x 1] in nm
    %          - Spec - Gaia XP spectra cell array {N_calib x 2}
    %                   Column 1: Flux values [N_GaiaWvl x 1]
    %                   Column 2: Flux errors [N_GaiaWvl x 1]
    %          - Flux - Observed LAST flux [N_calib x 1]
    %          - FluxErr - Observed flux errors [N_calib x 1]
    %          - X - Source X coordinates [N_calib x 1]
    %          - Y - Source Y coordinates [N_calib x 1]
    %          - PolyCheb - Field correction function handle @(X, Y, FieldParams) (REQUIRED)
    %          * ...,key,val,...
    %            'YAMLConfig' - YAML configuration structure or path to YAML file.
    %                   If empty and no explicit specs provided, uses '~/config/CalibPhotAB.yml'.
    %                   Default is [].
    %            'TransmissionFunctions' - Explicit transmission function specification.
    %                   Cell array with function specs (overrides YAML). Default is [].
    %            'OptimizationSequence' - Explicit optimization sequence (overrides YAML).
    %                   Default is [].
    %            'DefaultPressure_mbar' - Atmospheric pressure [mbar].
    %                   Default is 965.
    %            'GaiaWavelength' - Gaia XP wavelength grid [N_gaia x 1] nm.
    %                   Default is linspace(336, 1020, 343)'.
    %            'Airmass' - Atmospheric airmass.
    %                   Default is 1.2.
    %            'Temperature' - Temperature [C].
    %                   Default is 15.
    %            'ExpTime' - Exposure time [s].
    %                   Default is 20.
    %            'Aperture_area_m2' - Telescope aperture area [m^2].
    %                   Default is pi * (0.1397)^2.
    %            'Verbose' - Enable verbose output.
    %                   Default is true.
    % Output : - Model - Fitted CompositeFun object with optimized parameters.
    %          - FieldParams - Optimized field correction parameters [1 x 10].
    %          - Results - Cell array with results from each optimization stage.
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Example 1: Using YAML configuration
    %          Lambda = linspace(336, 1020, 343)';
    %          Spec = cell(3, 2);
    %          Spec{1,1} = (5e-17) ./ (Lambda / 400).^2;  % Blue star
    %          Spec{1,2} = 0.05 * Spec{1,1};
    %          Spec{2,1} = (3e-17) ./ (Lambda / 550).^0.5;  % Solar-type star
    %          Spec{2,2} = 0.05 * Spec{2,1};
    %          Spec{3,1} = (2e-17) * (Lambda / 700).^1.5;  % Red star
    %          Spec{3,2} = 0.05 * Spec{3,1};
    %          Flux = [1.2e5; 8.5e4; 6.3e4];  % Observed photons
    %          FluxErr = [5e3; 4e2; 3e2];
    %          X = [500; 1000; 1500];  % Pixel coordinates
    %          Y = [500; 1000; 1500];
    %          %% In the pipeline, [Spec, Flux, FluxErr, X, Y] come from
    %          %% upper-level wrapper function
    %          PolyCheb = @(X, Y, FP) telescope.optics.fieldCorrectionLAST([X(:), Y(:)], FP);
    %          YAMLPath = '~/matlab/AstroPack/config/CalibPhotAB.yml';
    %          [Model, FieldParams, Results] = imUtil.calib.transmissionFit(...
    %              Lambda, Spec, Flux, FluxErr, X, Y, PolyCheb, 'YAMLConfig', YAMLPath);
    %
    %          % Example 2: Without YAML - explicit 2-stage optimization
    %          % Prepare data
    %          Lambda = linspace(336, 1020, 343)';
    %          Spec = cell(3, 2);
    %          Spec{1,1} = (5e-17) ./ (Lambda / 400).^2;  % Blue star
    %          Spec{1,2} = 0.05 * Spec{1,1};
    %          Spec{2,1} = (3e-17) ./ (Lambda / 550).^0.5;  % Solar-type star
    %          Spec{2,2} = 0.05 * Spec{2,1};
    %          Spec{3,1} = (2e-17) * (Lambda / 700).^1.5;  % Red star
    %          Spec{3,2} = 0.05 * Spec{3,1};
    %          Flux = [1.2e5; 8.5e4; 6.3e4];  % Observed photons
    %          FluxErr = [5e3; 4e2; 3e2];
    %          X = [500; 1000; 1500];  % Pixel coordinates
    %          Y = [500; 1000; 1500];
    %          PolyCheb = @(X, Y, FP) telescope.optics.fieldCorrectionLAST([X(:), Y(:)], FP);
    %          % Define transmission functions list
    %          TransFunList = cell(2, 1);
    %          TransFunList{1} = struct('name', 'Ozone', ...
    %              'handle', '@astro.transmission.ozoneTransmission', ...
    %              'handletype', 'named', ...
    %              'params', [30, 300], ...
    %              'paraminfo', {{struct('name', 'ZenithAngle_deg', 'min', 0, 'max', 90), ...
    %                             struct('name', 'DobsonUnits', 'min', 200, 'max', 400)}});
    %          TransFunList{2} = struct('name', 'Aerosol', ...
    %              'handle', '@astro.transmission.aerosolTransmission', ...
    %              'handletype', 'named', ...
    %              'params', [30, 0.05, 1.2], ...
    %              'paraminfo', {{struct('name', 'ZenithAngle_deg', 'min', 0, 'max', 90), ...
    %                             struct('name', 'TauAod500', 'min', 0.0, 'max', 0.5), ...
    %                             struct('name', 'Alpha', 'min', 0.5, 'max', 2.5)}});
    %          % Define 2-stage optimization sequence
    %          OptSeq = cell(2, 1);
    %          OptSeq{1}.stagename = 'AerosolOpt';
    %          OptSeq{1}.freeparams = {{'Aerosol', 'TauAod500'}};  % Cell array of one [FuncName, ParamName] pair
    %          OptSeq{1}.sigmaclip = true;
    %          OptSeq{1}.sigmathresh = 3.0;
    %          OptSeq{1}.sigmaiter = 3;
    %          OptSeq{1}.description = 'Optimize aerosol optical depth';
    %          OptSeq{2}.stagename = 'FieldCorr';
    %          OptSeq{2}.freeparams = {};  % Empty for field correction
    %          OptSeq{2}.sigmaclip = true;
    %          OptSeq{2}.sigmathresh = 2.0;
    %          OptSeq{2}.sigmaiter = 2;
    %          OptSeq{2}.regularization = 1e-6;
    %          OptSeq{2}.description = 'Field correction (always linear)';
    %          % Run fitting
    %          [Model, FieldParams, Results] = imUtil.calib.transmissionFit(...
    %              Lambda, Spec, Flux, FluxErr, X, Y, PolyCheb, ...
    %              'TransmissionFunctions', TransFunList, 'OptimizationSequence', OptSeq);

    arguments
        Lambda double               % Wavelength grid [N_lambda x 1]
        Spec cell                   % Gaia XP spectra {N_calib x 2}
        Flux double                 % Observed LAST flux [N_calib x 1]
        FluxErr double              % Observed flux errors [N_calib x 1]
        X double                    % X coordinates [N_calib x 1]
        Y double                    % Y coordinates [N_calib x 1]
        PolyCheb function_handle    % Field correction function handle
        Args.YAMLConfig = []        % YAML config (optional)
        Args.TransmissionFunctions = []  % Explicit transmission functions (optional)
        Args.OptimizationSequence = []  % Explicit optimization sequence (optional)
        Args.DefaultPressure_mbar = 965  % Atmospheric pressure
        Args.GaiaWavelength = linspace(336, 1020, 343)'
        Args.Airmass = 1.2
        Args.Temperature = 15
        Args.ExpTime = 20
        Args.Aperture_area_m2 = pi * (0.1397)^2
        Args.Verbose logical = true
    end

    % ====================================================================
    % STEP 1: LOAD OR BUILD TRANSMISSION MODEL
    % ====================================================================

    if Args.Verbose
        fprintf('\n=== TRANSMISSION-BASED PHOTOMETRIC CALIBRATION ===\n\n');
    end

    % ====================================================================
    % STEP 2: LOAD YAML OR USE EXPLICIT CONFIGURATION
    % ====================================================================

    % Determine source of configuration
    % Use explicit config if OptimizationSequence is provided (TransmissionFunctions optional)
    UseExplicit = ~isempty(Args.OptimizationSequence);

    if UseExplicit
        % Explicit configuration provided
        if Args.Verbose
            fprintf('Using explicit optimization sequence\n');
        end
        OptSequence = Args.OptimizationSequence;

        % TransmissionFunctions is optional - user may pass pre-built Model
        if ~isempty(Args.TransmissionFunctions)
            TransFunList = Args.TransmissionFunctions;
        else
            TransFunList = [];  % Will skip model building
        end
        Pressure_mbar = Args.DefaultPressure_mbar;

    else
        % Load from YAML
        if isempty(Args.YAMLConfig)
            % Use default YAML path
            YAMLPath = '~/matlab/AstroPack/config/CalibPhotAB.yml';
            if Args.Verbose
                fprintf('Using default YAML configuration: %s\n', YAMLPath);
            end
        else
            YAMLPath = Args.YAMLConfig;
            if Args.Verbose
                fprintf('Loading YAML configuration: %s\n', YAMLPath);
            end
        end
        
        YAMLConfig = loadYAMLConfig(YAMLPath, Args.Verbose);
        TransFunList = YAMLConfig.TransmissionFunctions;
        OptSequence = YAMLConfig.OptimizationSequence;

        if isfield(YAMLConfig, 'DefaultPressure_mbar')
            Pressure_mbar = YAMLConfig.DefaultPressure_mbar;
        else
            Pressure_mbar = Args.DefaultPressure_mbar;
        end
    end

    % ====================================================================
    % STEP 3: BUILD TRANSMISSION MODEL (CompositeFun)
    % ====================================================================

    if Args.Verbose
        fprintf('Building transmission model (CompositeFun)...\n');
    end

    Model = buildTransmissionModel(TransFunList, Args.Airmass, Args.Temperature, ...
                                    Pressure_mbar, Args.Verbose);

    NumStages = length(OptSequence);

    if Args.Verbose
        fprintf('Optimization sequence: %d stages\n\n', NumStages);
    end

    % ====================================================================
    % STEP 2: INITIALIZE PARAMETERS
    % ====================================================================

    % Get initial transmission parameter values from Model
    CurrentParamValues = Model.valuesAllPar();

    % Initialize field correction parameters (always start from zeros)
    CurrentFieldParams = zeros(1, 10);

    % Prepare current calibrator data structure
    CurrentSpec = Spec;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;
    NumCalibrators = size(Spec, 1);

    if Args.Verbose
        fprintf('Initial calibrators: %d\n', NumCalibrators);
        fprintf('Initial transmission parameters: [%.3f, %.3f, ...]\n', ...
                CurrentParamValues(1), CurrentParamValues(2));
        fprintf('Initial field parameters: [%.3f, %.3f, ...]\n\n', ...
                CurrentFieldParams(1), CurrentFieldParams(2));
    end

    % ====================================================================
    % STEP 3: RUN OPTIMIZATION SEQUENCE
    % ====================================================================

    Results = cell(NumStages, 1);

    for iStage = 1:NumStages
        Stage = OptSequence{iStage};

        StageName = Stage.stagename;
        FreeParamsStage = Stage.freeparams;

        % FreeParamsStage should be a cell array of pairs: {{'Func1', 'Param1'}, {'Func2', 'Param2'}}
        % Or empty {} for field correction
        % No unwrapping needed - the format {{'Aerosol', 'TauAod500'}} is correct as-is

        SigmaClip = Stage.sigmaclip;
        SigmaThresh = Stage.sigmathresh;
        SigmaIter = Stage.sigmaiter;

        % Detect field correction stage (empty freeparams) - always linear
        % Transmission parameter stages are always nonlinear
        IsFieldCorrectionStage = isempty(FreeParamsStage) || (iscell(FreeParamsStage) && isempty([FreeParamsStage{:}]));

        % Extract regularization parameter if present (for field correction)
        if isfield(Stage, 'regularization')
            Regularization = Stage.regularization;
        else
            Regularization = 0;
        end

        if Args.Verbose
            if IsFieldCorrectionStage
                Method = 'linear';
            else
                Method = 'nonlinear';
            end
            fprintf('--- Stage %d/%d: %s [%s] ---\n', iStage, NumStages, StageName, Method);
            fprintf('Description: %s\n', Stage.description);
        end

        if IsFieldCorrectionStage
            % Field correction stage
            if Args.Verbose
                fprintf('Field correction stage: optimizing spatial parameters\n');
            end

            % Run field correction optimization (always linear)
            [OptFieldParams, Cost, Residuals, ClippedData] = optimizeFieldCorrection(...
                Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                CurrentParamValues, CurrentFieldParams, Model, PolyCheb, ...
                SigmaClip, SigmaThresh, SigmaIter, Regularization, Args);

            % Update field parameters
            CurrentFieldParams = OptFieldParams;

            % Update calibrator data after sigma clipping
            if ~isempty(ClippedData)
                CurrentSpec = ClippedData.Spec;
                CurrentFlux = ClippedData.Flux;
                CurrentFluxErr = ClippedData.FluxErr;
                CurrentX = ClippedData.X;
                CurrentY = ClippedData.Y;
            end

        else
            % Transmission parameter optimization
            % Set FitPar flags for this stage's parameters
            AllPar = Model.getAllParStruct();
            AllPar.FitPar(:) = false;  % Reset all to false

            % Set FitPar for parameters specified in this stage
            for i = 1:length(FreeParamsStage)
                ParamPair = FreeParamsStage{i};
                FunctionName = ParamPair{1};
                ParameterName = ParamPair{2};
                Idx = find(strcmp(AllPar.Names, ParameterName), 1);
                if isempty(Idx)
                    fprintf('ERROR: Parameter "%s" (from function "%s") not found\n', ParameterName, FunctionName);
                    fprintf('Available parameters in Model:\n');
                    for j = 1:length(AllPar.Names)
                        fprintf('  [%d] %s\n', j, AllPar.Names{j});
                    end
                    error('Parameter "%s" not found in Model', ParameterName);
                end
                AllPar.FitPar(Idx) = true;
            end

            % Apply FitPar flags (keeps parameter values from previous stage)
            Model.setAllParStruct(AllPar);

            % Get fitted parameter indices
            FreeParamIndices = find(AllPar.FitPar);

            if Args.Verbose
                fprintf('Free parameters: %d\n', length(FreeParamIndices));
                for i = 1:length(FreeParamIndices)
                    fprintf('  [%d] %s\n', FreeParamIndices(i), AllPar.Names{FreeParamIndices(i)});
                end
            end

            % Run transmission parameter optimization (always nonlinear)
            [OptTransParams, Cost, Residuals, ClippedData] = optimizeTransmission(...
                Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                CurrentParamValues, CurrentFieldParams, Model, PolyCheb, ...
                FreeParamIndices, SigmaClip, SigmaThresh, SigmaIter, Args);

            % Update transmission parameters
            CurrentParamValues = OptTransParams;

            % Update Model with optimized parameters
            AllPar = Model.getAllParStruct();
            AllPar.Values = CurrentParamValues;
            Model.setAllParStruct(AllPar);

            % Update calibrator data after sigma clipping
            if ~isempty(ClippedData)
                CurrentSpec = ClippedData.Spec;
                CurrentFlux = ClippedData.Flux;
                CurrentFluxErr = ClippedData.FluxErr;
                CurrentX = ClippedData.X;
                CurrentY = ClippedData.Y;
            end
        end

        % Store stage results
        RMS = sqrt(Cost / length(Residuals));
        Results{iStage} = struct(...
            'StageName', StageName, ...
            'Method', Method, ...
            'Cost', Cost, ...
            'RMS', RMS, ...
            'Residuals', Residuals, ...
            'NumCalibrators', length(Residuals), ...
            'IsFieldCorrection', IsFieldCorrectionStage);

        if Args.Verbose
            fprintf('Stage complete: Cost=%.4e, RMS=%.4f mag, Calibrators=%d\n\n', ...
                    Cost, RMS, length(Residuals));
        end
    end

    % ====================================================================
    % STEP 4: FINALIZE OUTPUT
    % ====================================================================

    FieldParams = CurrentFieldParams;

    if Args.Verbose
        fprintf('=== OPTIMIZATION COMPLETE ===\n\n');
        fprintf('Final transmission parameters:\n');
        AllPar = Model.getAllParStruct();
        for i = 1:length(AllPar.Values)
            fprintf('  [%d] %s: %.6f\n', i, AllPar.Names{i}, AllPar.Values(i));
        end
        fprintf('\nFinal field correction parameters:\n');
        FieldParamNames = {'kx0', 'kx', 'kx2', 'kx3', 'kx4', 'ky', 'ky2', 'ky3', 'ky4', 'kxy'};
        for i = 1:10
            fprintf('  [%d] %s: %.6f\n', i, FieldParamNames{i}, FieldParams(i));
        end
        fprintf('\nFinal calibrators: %d\n', length(Residuals));
        fprintf('Final RMS: %.4f mag\n', RMS);
    end
end

%% ========================================================================
%  HELPER FUNCTION: Build Transmission Model
%  ========================================================================

function Model = buildTransmissionModel(TransFunList, Airmass, Temperature, Pressure_mbar, Verbose)
    % Build CompositeFun and inject metadata using CompositeFun's parameter mapping

    % Create CompositeFun object
    Model = tools.math.fun.CompositeFun();

    NumFunctions = length(TransFunList);

    % Add all transmission functions
    for i = 1:NumFunctions
        FunDef = TransFunList{i};

        % Extract function definition
        FunName = FunDef.name;
        HandleStr = FunDef.handle;
        HandleType = FunDef.handletype;
        Params = FunDef.params;
        ParamInfo = FunDef.paraminfo;

        % Ensure Params is numeric array
        if iscell(Params)
            Params = cell2mat(Params);
        end
        if ~isnumeric(Params)
            error('Parameters for function %s are not numeric', FunName);
        end

        % Convert to row vector
        if iscolumn(Params)
            Params = Params';
        end

        % Create FitPar array (all false initially)
        NumParams = length(Params);
        FitPar = false(1, NumParams);

        % Build ArgNames structure
        % Note: CompositeFun's getAllParStruct().Names returns the Description field,
        % so we put the parameter name in Description to enable lookup by name
        ArgNames = struct([]);
        for j = 1:NumParams
            PInfo = ParamInfo{j};
            ArgNames(j).Name = PInfo.name;  % Actual parameter name (used for lookup)
            ArgNames(j).Description = PInfo.name;  % Put name here so getAllParStruct() finds it
            ArgNames(j).Min = PInfo.min;
            ArgNames(j).Max = PInfo.max;
        end

        % Convert string handle to function handle
        if strcmp(HandleType, 'anonymous')
            FunHandle = str2func(HandleStr);
        elseif strcmp(HandleType, 'named')
            if startsWith(HandleStr, '@')
                HandleStr = HandleStr(2:end);
            end
            FunHandle = str2func(HandleStr);
        else
            error('Unknown handletype: %s', HandleType);
        end

        % Add function to CompositeFun
        Model.addFun(FunName, FunHandle, ArgNames, 'Par', Params, 'FitPar', FitPar);

        if Verbose
            fprintf('  [%d/%d] Added: %s (%d params)\n', i, NumFunctions, FunName, NumParams);
        end
    end

    % Inject metadata into parameters by name using CompositeFun's mapping
    if Airmass < 1.0
        error('Invalid Airmass: %.3f (must be >= 1.0)', Airmass);
    end
    ZenithAngle_deg = acosd(1.0 / Airmass);

    if Verbose
        fprintf('Injecting observation metadata:\n');
        fprintf('  Airmass: %.3f -> Zenith angle: %.2f deg\n', Airmass, ZenithAngle_deg);
        fprintf('  Temperature: %.1f C\n', Temperature);
        fprintf('  Pressure: %.1f mbar\n', Pressure_mbar);
    end

    % Get all parameters structure from CompositeFun
    AllPar = Model.getAllParStruct();

    % Update parameters by name matching
    for i = 1:length(AllPar.Names)
        ParamName = AllPar.Names{i};

        if strcmp(ParamName, 'ZenithAngle_deg')
            AllPar.Values(i) = ZenithAngle_deg;
            if Verbose
                fprintf('  Injected %s = %.3f\n', ParamName, ZenithAngle_deg);
            end
        elseif strcmp(ParamName, 'Pressure_mbar')
            AllPar.Values(i) = Pressure_mbar;
            if Verbose
                fprintf('  Injected %s = %.3f\n', ParamName, Pressure_mbar);
            end
        elseif strcmp(ParamName, 'Temperature_C')
            AllPar.Values(i) = Temperature;
            if Verbose
                fprintf('  Injected %s = %.3f\n', ParamName, Temperature);
            end
        end
    end

    % Apply updated parameters back to CompositeFun
    Model.setAllParStruct(AllPar);

    if Verbose
        fprintf('CompositeFun built: %d functions, %d total parameters\n', ...
                NumFunctions, Model.numAllPar());
    end
end

%% ========================================================================
%  HELPER FUNCTION: Load YAML Configuration
%  ========================================================================

function YAMLConfig = loadYAMLConfig(YAMLPath, Verbose)
    % Load YAML configuration file
    if ischar(YAMLPath) || isstring(YAMLPath)
        YAMLFile = YAMLPath;
        if startsWith(YAMLFile, '~')
            YAMLFile = strrep(YAMLFile, '~', getenv('HOME'));
        end
        if Verbose
            fprintf('Loading YAML configuration: %s\n', YAMLFile);
        end
        YAMLConfig = yaml.ReadYaml(YAMLFile);
    else
        YAMLConfig = YAMLPath;  % Already a structure
    end

    % Validate required fields
    if ~isfield(YAMLConfig, 'TransmissionFunctions')
        error('YAML config missing required field: TransmissionFunctions');
    end
    if ~isfield(YAMLConfig, 'OptimizationSequence')
        error('YAML config missing required field: OptimizationSequence');
    end
end

%% ========================================================================
%  HELPER FUNCTION: Optimize Field Correction
%  ========================================================================

function [OptFieldParams, Cost, Residuals, ClippedData] = optimizeFieldCorrection(...
    Lambda, Spec, Flux, FluxErr, X, Y, TransParams, FieldParams, Model, PolyCheb, ...
    SigmaClip, SigmaThresh, SigmaIter, Regularization, Args)
    % Optimize field correction parameters using linear least squares

    % Sigma clipping loop
    CurrentSpec = Spec;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;
    CurrentFieldParams = FieldParams;

    if SigmaClip
        for iter = 1:SigmaIter
            % Linear optimization using regularized least squares
            % Get residuals without field correction (FieldParams = 0)
            ZeroFieldParams = zeros(1, 10);
            [Residuals0, ~, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
                CurrentX, CurrentY, TransParams, Model, PolyCheb, ...
                'FieldParams', ZeroFieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
                'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
                'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

            % Build design matrix for field correction
            % FieldCorrection is linear in parameters: PolyCheb(X, Y, FieldParams)
            % We solve: Residuals0 + PolyCheb(X, Y, FieldParams) = 0
            NumCalib = length(CurrentX);
            DesignMatrix = zeros(NumCalib, 10);
            for j = 1:10
                TestParams = zeros(1, 10);
                TestParams(j) = 1.0;
                DesignMatrix(:, j) = PolyCheb(CurrentX, CurrentY, TestParams);
            end

            % Solve with regularization: min ||A*x + b||^2 + lambda*||x||^2
            if Regularization > 0
                CurrentFieldParams = -(DesignMatrix' * DesignMatrix + Regularization * eye(10)) \ (DesignMatrix' * Residuals0);
            else
                CurrentFieldParams = -DesignMatrix \ Residuals0;
            end
            CurrentFieldParams = CurrentFieldParams';  % Ensure row vector

            % Calculate residuals with optimized field parameters
            [Residuals, ~, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
                CurrentX, CurrentY, TransParams, Model, PolyCheb, ...
                'FieldParams', CurrentFieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
                'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
                'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

            % Apply sigma clipping
            [ClippedData, NumOutliers] = applySigmaClipping(...
                CurrentSpec, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                Residuals, SigmaThresh);

            if NumOutliers == 0
                break;
            end

            % Update data for next iteration
            CurrentSpec = ClippedData.Spec;
            CurrentFlux = ClippedData.Flux;
            CurrentFluxErr = ClippedData.FluxErr;
            CurrentX = ClippedData.X;
            CurrentY = ClippedData.Y;
        end
    else
        % Single optimization without sigma clipping
        % Linear optimization using regularized least squares
        ZeroFieldParams = zeros(1, 10);
        [Residuals0, ~, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
            CurrentX, CurrentY, TransParams, Model, PolyCheb, ...
            'FieldParams', ZeroFieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

        % Build design matrix
        NumCalib = length(CurrentX);
        DesignMatrix = zeros(NumCalib, 10);
        for j = 1:10
            TestParams = zeros(1, 10);
            TestParams(j) = 1.0;
            DesignMatrix(:, j) = PolyCheb(CurrentX, CurrentY, TestParams);
        end

        % Solve with regularization
        if Regularization > 0
            CurrentFieldParams = -(DesignMatrix' * DesignMatrix + Regularization * eye(10)) \ (DesignMatrix' * Residuals0);
        else
            CurrentFieldParams = -DesignMatrix \ Residuals0;
        end
        CurrentFieldParams = CurrentFieldParams';  % Ensure row vector

        ClippedData = [];
    end

    % Final cost and residuals
    [Residuals, Cost, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
        CurrentX, CurrentY, TransParams, Model, PolyCheb, ...
        'FieldParams', CurrentFieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
        'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
        'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);
    OptFieldParams = CurrentFieldParams;
end

%% ========================================================================
%  HELPER FUNCTION: Optimize Transmission Parameters
%  ========================================================================

function [OptTransParams, Cost, Residuals, ClippedData] = optimizeTransmission(...
    Lambda, Spec, Flux, FluxErr, X, Y, TransParams, FieldParams, Model, PolyCheb, ...
    FreeParamIndices, SigmaClip, SigmaThresh, SigmaIter, Args)
    % Optimize transmission parameters using nonlinear least squares (lsqnonlin)

    % Create cost function wrapper that only varies free parameters
    FixedMask = true(size(TransParams));
    FixedMask(FreeParamIndices) = false;
    FixedValues = TransParams;

    % Nested function to reconstruct full parameter vector
    function FullParams = updateFullParams(Fixed, Free, Indices)
        FullParams = Fixed;
        FullParams(Indices) = Free;
    end

    % Sigma clipping loop
    CurrentSpec = Spec;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;
    CurrentTransParams = TransParams;
    FreeParams = CurrentTransParams(FreeParamIndices);

    if SigmaClip
        for iter = 1:SigmaIter
            % Optimize free parameters using nonlinear least squares
            Opts = optimoptions('lsqnonlin', 'Display', 'off', ...
                               'MaxIterations', 1000, 'FunctionTolerance', 1e-8);

            % Get bounds for free parameters
            AllPar = Model.getAllParStruct();
            Lb = AllPar.Min(FreeParamIndices);
            Ub = AllPar.Max(FreeParamIndices);

            ResFun = @(FP) imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
                CurrentX, CurrentY, updateFullParams(FixedValues, FP, FreeParamIndices), Model, PolyCheb, ...
                'FieldParams', FieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
                'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
                'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

            FreeParams = lsqnonlin(ResFun, FreeParams, Lb, Ub, Opts);

            % Update full parameter vector
            CurrentTransParams(FreeParamIndices) = FreeParams;

            % Calculate residuals
            [Residuals, ~, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
                CurrentX, CurrentY, updateFullParams(FixedValues, FreeParams, FreeParamIndices), Model, PolyCheb, ...
                'FieldParams', FieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
                'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
                'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

            % Apply sigma clipping
            [ClippedData, NumOutliers] = applySigmaClipping(...
                CurrentSpec, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                Residuals, SigmaThresh);

            if NumOutliers == 0
                break;
            end

            % Update data for next iteration
            CurrentSpec = ClippedData.Spec;
            CurrentFlux = ClippedData.Flux;
            CurrentFluxErr = ClippedData.FluxErr;
            CurrentX = ClippedData.X;
            CurrentY = ClippedData.Y;
        end
    else
        % Single optimization without sigma clipping
        Opts = optimoptions('lsqnonlin', 'Display', 'off', ...
                           'MaxIterations', 1000, 'FunctionTolerance', 1e-8);

        AllPar = Model.getAllParStruct();
        Lb = AllPar.Min(FreeParamIndices);
        Ub = AllPar.Max(FreeParamIndices);

        % Create residual function for lsqnonlin (transmission params vary, field params fixed)
        ResFun = @(FP) imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
            CurrentX, CurrentY, updateFullParams(FixedValues, FP, FreeParamIndices), Model, PolyCheb, ...
            'FieldParams', FieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

        FreeParams = lsqnonlin(ResFun, FreeParams, Lb, Ub, Opts);
        ClippedData = [];
    end

    % Update full parameter vector
    CurrentTransParams(FreeParamIndices) = FreeParams;

    % Final cost and residuals
    [Residuals, Cost, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentFlux, CurrentFluxErr, ...
        CurrentX, CurrentY, updateFullParams(FixedValues, FreeParams, FreeParamIndices), Model, PolyCheb, ...
        'FieldParams', FieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
        'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
        'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

    OptTransParams = CurrentTransParams;
end


%% ========================================================================
%  HELPER FUNCTION: Sigma Clipping
%  ========================================================================

function [ClippedData, NumOutliers] = applySigmaClipping(Spec, Flux, FluxErr, X, Y, Residuals, Threshold)
    % Apply sigma clipping using robust statistics
    MedianResid = median(Residuals);
    MAD = median(abs(Residuals - MedianResid));
    RobustStd = 1.4826 * MAD;

    OutlierMask = abs(Residuals - MedianResid) > Threshold * RobustStd;
    GoodMask = ~OutlierMask;
    NumOutliers = sum(OutlierMask);

    ClippedData = struct();
    ClippedData.Spec = Spec(GoodMask, :);
    ClippedData.Flux = Flux(GoodMask);
    ClippedData.FluxErr = FluxErr(GoodMask);
    ClippedData.X = X(GoodMask);
    ClippedData.Y = Y(GoodMask);
end

