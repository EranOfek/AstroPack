function [Model, FieldParams, Results] = transmissionFit(Lambda, Spec, SpecErr, Flux, FluxErr, X, Y, PolyCheb, Args)
    % The function performs optimization to fit free transmission parameters to
    % observations for a single image. The used composite transmission model includes wavelength-dependent basic component functions and 
    % position-dependent polynomial to correct for zero-point variations. The fit is done by comparing the instrumental fluxes of
    % stars in the image to the synthetic photometry of the stars given their spectrum and the transmission function which have free parameters. 
    % Based on Garrappa et al. 2025, A&A 699, A50.
    % Input: wavelength range of observations, externally calibrated [Gaia DR3]
    % spectra for the sources in the image, observational fluxes for these
    % same sources, (X,Y) positions for these same sources in the image, PolyCheb - 
    % the form for position-dependent polynomial to correct for zero-point variations. 
    %
    % Input  : - Lambda - Wavelength grid for integration range in nm
    %          - Spec - Gaia XP spectra matrix [N_GaiaWvl x N_calib]
    %                   Each column is the flux spectrum for one calibrator
    %          - SpecErr - Gaia XP spectra errors matrix [N_GaiaWvl x N_calib]
    %                   Each column is the flux error spectrum (currently unused)
    %          - Flux - Observed flux for calibrators
    %          - FluxErr - Observed flux errors for calibrators
    %          - X - Source X coordinates for calibrators
    %          - Y - Source Y coordinates for calibrators
    %          - PolyCheb - Field correction function handle @(X, Y, FieldParams) 
    %          * ...,key,val,...
    %            'TransmissionFunctions' - Explicit specification for the
    %                                      basic components of composite transmission function
    %                                      (astro.transmission.* for atmospheric transmission,
    %                                      telescope.detector.* and telescope.optics.* for
    %                                      instrumental transmission). Will be used to compile transmission model 
    %                                      in form of tools.math.fun.CompositeFun object).
    %                                      Cell array with function specifications. Default is [].
    %            'OptimizationSequence' -  Explicit optimization sequence (number of stages, free parameters to 
    %                                      each stage, sigma clipping yes/no, number of iterations.
    %                                      Default is [].
    %            'GaiaWavelength' - Gaia XP wavelength grid, in nm. Default is linspace(336, 1020, 343)'.
    %            'YAMLConfig' - YAML configuration structure or path to YAML file describing 'TransmissionFunctions',
    %                                     'OptimizationSequence'.
    %                                      If empty and no explicit specs provided, uses '~/config/CalibPhotAB.yml' 
    %                                      (description according to Garrappa et al. 2025).
    %                                      Default is [].
    %            'Airmass' - Atmospheric airmass. Default is 1.2.
    %            'Temperature' - Temperature [C]. Default is 15.
    %            'DefaultPressure_mbar' - Atmospheric pressure [mbar]. Default is 965.
    %            'ExpTime' - Exposure time [s]. Default is 20.
    %            'Aperture_area_m2' - Telescope aperture area [m^2]. 
    %                                    Default is pi * (0.1397)^2. (LAST)
    %            'Verbose' - Enable verbose output. Default is true.
    % Output : - Model - Fitted CompositeFun object with optimized parameters.
    %          - FieldParams - Optimized position-dependent ZP correction parameters [1 x 10].
    %          - Results - Cell array with results from each optimization stage.
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Example 1: Using YAML configuration
    %          Lambda = linspace(336, 1020, 343)';
    %          Spec = [(5e-17) ./ (Lambda / 400).^2, ...      % Blue star
    %                  (3e-17) ./ (Lambda / 550).^0.5, ...    % Solar-type star
    %                  (2e-17) * (Lambda / 700).^1.5];        % Red star [343 x 3]
    %          SpecErr = 0.05 * Spec;  % 5% errors
    %          Flux = [2.1e4; 3.2e4; 2.3e4];  % Observed photons
    %          FluxErr = [2e3; 3e2; 2e2];
    %          X = [500; 1000; 1500];  % Pixel coordinates
    %          Y = [500; 1000; 1500];
    %          % In the pipeline, [Spec, SpecErr, Flux, FluxErr, X, Y] come from
    %          % upper-level wrapper function.
    %          PolyCheb = @(X, Y, FP) telescope.optics.fieldCorrectionLAST([X(:), Y(:)], FP);
    %          YAMLPath = '~/matlab/AstroPack/config/CalibPhotAB.yml';
    %          [Model, FieldParams, Results] = imUtil.calib.transmissionFit(...
    %              Lambda, Spec, SpecErr, Flux, FluxErr, X, Y, PolyCheb, 'YAMLConfig', YAMLPath);
    %
    %          % Example 2: Without YAML - explicit 2-stage optimization
    %          % Prepare data
    %          Lambda = linspace(336, 1020, 343)';
    %          Spec = [(5e-17) ./ (Lambda / 400).^2, ...      % Blue star
    %                  (3e-17) ./ (Lambda / 550).^0.5, ...    % Solar-type star
    %                  (2e-17) * (Lambda / 700).^1.5];        % Red star [343 x 3]
    %          SpecErr = 0.05 * Spec;  % 5% errors
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
    %              Lambda, Spec, SpecErr, Flux, FluxErr, X, Y, PolyCheb, ...
    %              'TransmissionFunctions', TransFunList, 'OptimizationSequence', OptSeq);

    arguments
        Lambda                      % Wavelength grid [N_lambda x 1]
        Spec                        % Gaia XP spectra matrix [N_GaiaWvl x N_calib]
        SpecErr                     % Gaia XP spectra errors [N_GaiaWvl x N_calib]
        Flux                        % Observed LAST flux [N_calib x 1]
        FluxErr                     % Observed flux errors [N_calib x 1]
        X                           % X coordinates [N_calib x 1]
        Y                           % Y coordinates [N_calib x 1]
        PolyCheb function_handle    % Field correction function handle
        Args.TransmissionFunctions = []  % Explicit transmission functions (optional)
        Args.OptimizationSequence = []  % Explicit optimization sequence (optional)
        Args.GaiaWavelength = linspace(336, 1020, 343)'   
        Args.YAMLConfig = []        % YAML config (optional)
        Args.Airmass = 1.2          % From observations metadata
        Args.Temperature = 15       % From observations metadata
        Args.DefaultPressure_mbar = 965  % Atmospheric pressure
        Args.ExpTime = 20           % For LAST
        Args.Aperture_area_m2 = pi * (0.1397)^2 % For LAST
        Args.Verbose logical = true
    end

    if Args.Verbose
        fprintf('\n=== TRANSMISSION-BASED PHOTOMETRIC CALIBRATION ===\n\n');
    end

    % ====================================================================
    % STEP 1: LOAD YAML OR USE EXPLICIT CONFIGURATION
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
    % STEP 2: BUILD TRANSMISSION MODEL (CompositeFun)
    % ====================================================================

    if Args.Verbose
        fprintf('Building transmission model (CompositeFun)...\n');
    end

    Model = imUtil.calib.transmissionModel(TransFunList, Args.Airmass, Args.Temperature, ...
                                            Pressure_mbar, 'Verbose', Args.Verbose);

    NumStages = length(OptSequence);

    if Args.Verbose
        fprintf('Optimization sequence: %d stages\n\n', NumStages);
    end

    % ====================================================================
    % STEP 3: VALIDATE AND PREPARE INPUT DATA
    % ====================================================================

    % Validate Lambda
    if ~isvector(Lambda) || isempty(Lambda)
        error('Lambda must be a non-empty vector');
    end
    Lambda = Lambda(:);  % Ensure column vector

    % Validate Spec dimensions
    if ~ismatrix(Spec) || isempty(Spec)
        error('Spec must be a non-empty matrix [N_GaiaWvl x N_calib]');
    end
    [N_GaiaWvl, NumCalibrators] = size(Spec);

    % Validate SpecErr dimensions
    if ~ismatrix(SpecErr) || isempty(SpecErr)
        error('SpecErr must be a non-empty matrix [N_GaiaWvl x N_calib]');
    end
    if ~isequal(size(SpecErr), size(Spec))
        error('SpecErr dimensions [%d x %d] must match Spec dimensions [%d x %d]', ...
              size(SpecErr, 1), size(SpecErr, 2), N_GaiaWvl, NumCalibrators);
    end

    % Validate and enforce column vectors for Flux, FluxErr, X, Y
    if ~isvector(Flux) || length(Flux) ~= NumCalibrators
        error('Flux must be a vector with %d elements (number of calibrators)', NumCalibrators);
    end
    Flux = Flux(:);  % Ensure column vector

    if ~isvector(FluxErr) || length(FluxErr) ~= NumCalibrators
        error('FluxErr must be a vector with %d elements (number of calibrators)', NumCalibrators);
    end
    FluxErr = FluxErr(:);  % Ensure column vector

    if ~isvector(X) || length(X) ~= NumCalibrators
        error('X must be a vector with %d elements (number of calibrators)', NumCalibrators);
    end
    X = X(:);  % Ensure column vector

    if ~isvector(Y) || length(Y) ~= NumCalibrators
        error('Y must be a vector with %d elements (number of calibrators)', NumCalibrators);
    end
    Y = Y(:);  % Ensure column vector

    if Args.Verbose
        fprintf('Input validation complete:\n');
        fprintf('  Lambda: %d wavelength points\n', length(Lambda));
        fprintf('  Spec: [%d x %d] (GaiaWvl x Calibrators)\n', N_GaiaWvl, NumCalibrators);
        fprintf('  Flux, FluxErr, X, Y: [%d x 1] each\n', NumCalibrators);
    end

    % ====================================================================
    % STEP 4: INITIALIZE PARAMETERS
    % ====================================================================

    % Get initial transmission parameter values from Model
    CurrentParamValues = Model.valuesAllPar();

    % Initialize field correction parameters (always start from zeros)
    CurrentFieldParams = zeros(1, 10);

    % Prepare current calibrator data structure
    CurrentSpec = Spec;
    CurrentSpecErr = SpecErr;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;

    if Args.Verbose
        fprintf('Initial calibrators: %d\n', NumCalibrators);
        fprintf('Initial transmission parameters: [%.3f, %.3f, ...]\n', ...
                CurrentParamValues(1), CurrentParamValues(2));
        fprintf('Initial field parameters: [%.3f, %.3f, ...]\n\n', ...
                CurrentFieldParams(1), CurrentFieldParams(2));
    end

    % ====================================================================
    % STEP 5: RUN OPTIMIZATION SEQUENCE
    % ====================================================================

    Results = cell(NumStages, 1);

    for IStage = 1:NumStages
        Stage = OptSequence{IStage};

        StageName = Stage.stagename;
        FreeParamsStage = Stage.freeparams;

        % FreeParamsStage should be a cell array of pairs: {{'Func1', 'Param1'}, {'Func2', 'Param2'}}
        % Or empty {} for field correction
  
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
            fprintf('--- Stage %d/%d: %s [%s] ---\n', IStage, NumStages, StageName, Method);
            fprintf('Description: %s\n', Stage.description);
        end

        if IsFieldCorrectionStage
            % Field correction stage
            if Args.Verbose
                fprintf('Field correction stage: optimizing spatial parameters\n');
            end

            % Run field correction optimization (always linear)
            [OptFieldParams, Cost, Residuals, ClippedData] = optimizeFieldCorrection(...
                Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                CurrentParamValues, CurrentFieldParams, Model, PolyCheb, ...
                SigmaClip, SigmaThresh, SigmaIter, Regularization, Args);

            % Update field parameters
            CurrentFieldParams = OptFieldParams;

            % Update calibrator data after sigma clipping
            if ~isempty(ClippedData)
                CurrentSpec = ClippedData.Spec;
                CurrentSpecErr = ClippedData.SpecErr;
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
            for I = 1:length(FreeParamsStage)
                ParamPair = FreeParamsStage{I};
                FunctionName = ParamPair{1};
                ParameterName = ParamPair{2};
                Idx = find(strcmp(AllPar.Names, ParameterName), 1);
                if isempty(Idx)
                    fprintf('ERROR: Parameter "%s" (from function "%s") not found\n', ParameterName, FunctionName);
                    fprintf('Available parameters in Model:\n');
                    for J = 1:length(AllPar.Names)
                        fprintf('  [%d] %s\n', J, AllPar.Names{J});
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
                for I = 1:length(FreeParamIndices)
                    fprintf('  [%d] %s\n', FreeParamIndices(I), AllPar.Names{FreeParamIndices(I)});
                end
            end

            % Run transmission parameter optimization (always nonlinear)
            [OptTransParams, Cost, Residuals, ClippedData] = optimizeTransmission(...
                Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
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
                CurrentSpecErr = ClippedData.SpecErr;
                CurrentFlux = ClippedData.Flux;
                CurrentFluxErr = ClippedData.FluxErr;
                CurrentX = ClippedData.X;
                CurrentY = ClippedData.Y;
            end
        end

        % Store stage results
        RMS = sqrt(Cost / length(Residuals));
        Results{IStage} = struct(...
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
    % STEP 6: FINALIZE OUTPUT
    % ====================================================================

    FieldParams = CurrentFieldParams;

    if Args.Verbose
        fprintf('=== OPTIMIZATION COMPLETE ===\n\n');
        fprintf('Final transmission parameters:\n');
        AllPar = Model.getAllParStruct();
        for I = 1:length(AllPar.Values)
            fprintf('  [%d] %s: %.6f\n', I, AllPar.Names{I}, AllPar.Values(I));
        end
        fprintf('\nFinal field correction parameters:\n');
        FieldParamNames = {'kx0', 'kx', 'kx2', 'kx3', 'kx4', 'ky', 'ky2', 'ky3', 'ky4', 'kxy'};
        for I = 1:10
            fprintf('  [%d] %s: %.12f\n', I, FieldParamNames{I}, FieldParams(I));
        end
        fprintf('\nFinal calibrators: %d\n', length(Residuals));
        fprintf('Final RMS: %.4f mag\n', RMS);
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
    Lambda, Spec, SpecErr, Flux, FluxErr, X, Y, TransParams, FieldParams, Model, PolyCheb, ...
    SigmaClip, SigmaThresh, SigmaIter, Regularization, Args)
    % Optimize field correction parameters using linear least squares

    % Initialize current data
    CurrentSpec = Spec;
    CurrentSpecErr = SpecErr;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;
    CurrentFieldParams = FieldParams;

    % Set number of iterations: 1 if no sigma clipping, SigmaIter otherwise
    NumIterations = 1;
    if SigmaClip
        NumIterations = SigmaIter;
    end

    % Optimization loop with optional sigma clipping
    for Iter = 1:NumIterations
        % Linear optimization using regularized least squares
        % Get residuals without field correction (FieldParams = 0)
        ZeroFieldParams = zeros(1, 10);
        [Residuals0, ~, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
            CurrentX, CurrentY, TransParams, Model, PolyCheb, ...
            'FieldParams', ZeroFieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

        % Build design matrix for field correction
        % FieldCorrection is linear in parameters: PolyCheb(X, Y, FieldParams)
        % We solve: Residuals0 + PolyCheb(X, Y, FieldParams) = 0
        % Build design matrix by calling PolyCheb with identity matrix (vectorized)
        DesignMatrix = PolyCheb(CurrentX, CurrentY, eye(10));

        % Solve with regularization: min ||A*x + b||^2 + lambda*||x||^2
        if Regularization > 0
            CurrentFieldParams = -(DesignMatrix' * DesignMatrix + Regularization * eye(10)) \ (DesignMatrix' * Residuals0);
        else
            CurrentFieldParams = -DesignMatrix \ Residuals0;
        end
        CurrentFieldParams = CurrentFieldParams';  % Ensure row vector

        % Calculate residuals with optimized field parameters
        [Residuals, ~, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
            CurrentX, CurrentY, TransParams, Model, PolyCheb, ...
            'FieldParams', CurrentFieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

        % Apply sigma clipping (only if enabled)
        if SigmaClip
            [ClippedData, NumOutliers] = applySigmaClipping(...
                CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                Residuals, SigmaThresh);

            if NumOutliers == 0
                break;
            end

            % Update data for next iteration
            CurrentSpec = ClippedData.Spec;
            CurrentSpecErr = ClippedData.SpecErr;
            CurrentFlux = ClippedData.Flux;
            CurrentFluxErr = ClippedData.FluxErr;
            CurrentX = ClippedData.X;
            CurrentY = ClippedData.Y;
        else
            ClippedData = [];
        end
    end

    % Final cost and residuals
    [Residuals, Cost, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
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
    Lambda, Spec, SpecErr, Flux, FluxErr, X, Y, TransParams, FieldParams, Model, PolyCheb, ...
    FreeParamIndices, SigmaClip, SigmaThresh, SigmaIter, Args)
    % Optimize transmission parameters using nonlinear least squares (lsqNonLinWithFixed)

    % Create cost function wrapper that only varies free parameters
    FixedMask = true(size(TransParams));
    FixedMask(FreeParamIndices) = false;
    FixedValues = TransParams;

    % Nested function to reconstruct full parameter vector
    function FullParams = updateFullParams(Fixed, Free, Indices)
        FullParams = Fixed;
        FullParams(Indices) = Free;
    end

    % Optimization loop (with or without sigma clipping)
    CurrentSpec = Spec;
    CurrentSpecErr = SpecErr;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;
    CurrentTransParams = TransParams;
    FreeParams = CurrentTransParams(FreeParamIndices);

    % Set number of iterations: 1 if no sigma clipping, SigmaIter otherwise
    NumIterations = 1;
    if SigmaClip
        NumIterations = SigmaIter;
    end

    for Iter = 1:NumIterations
        % Optimize transmission parameters using lsqNonLinWithFixed
        % Create options for underlying lsqnonlin solver
        Opts = optimoptions('lsqnonlin', 'Display', 'off', ...
                           'MaxIterations', 1000, 'FunctionTolerance', 1e-8);

        % Get bounds and setup FitMask for all parameters
        AllPar = Model.getAllParStruct();
        FitMask = false(size(CurrentTransParams));
        FitMask(FreeParamIndices) = true;

        % Model function for lsqNonLinWithFixed (returns residuals for all calibrators)
        % X_dummy is ignored, transmissionFun returns [Residuals, Cost, PredictedFlux]
        ModelFun = @(X_dummy, P) imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentSpecErr, ...
            CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, P, Model, PolyCheb, ...
            'FieldParams', FieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, ...
            'Verbose', false);

        % Dummy X (calibrator indices), Y = 0 (fit residuals to zero), uniform weights
        NumCalib = length(CurrentFlux);
        X_dummy = (1:NumCalib)';
        Y_target = zeros(NumCalib, 1);
        Sigma_weights = ones(NumCalib, 1);

        % Call lsqNonLinWithFixed
        [CurrentTransParams, ~, ~] = tools.math.fit.lsqNonLinWithFixed(...
            X_dummy, Y_target, Sigma_weights, ModelFun, ...
            'InitPar', CurrentTransParams, ...
            'FitPar', FitMask, ...
            'Lb', AllPar.Min, ...
            'Ub', AllPar.Max, ...
            'Opts', Opts);

        FreeParams = CurrentTransParams(FreeParamIndices);

        % Calculate residuals
        [Residuals, ~, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
            CurrentX, CurrentY, CurrentTransParams, Model, PolyCheb, ...
            'FieldParams', FieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

        % Apply sigma clipping (if enabled)
        if SigmaClip
            [ClippedData, NumOutliers] = applySigmaClipping(...
                CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                Residuals, SigmaThresh);

            if NumOutliers == 0
                break;
            end

            % Update data for next iteration
            CurrentSpec = ClippedData.Spec;
            CurrentSpecErr = ClippedData.SpecErr;
            CurrentFlux = ClippedData.Flux;
            CurrentFluxErr = ClippedData.FluxErr;
            CurrentX = ClippedData.X;
            CurrentY = ClippedData.Y;
        else
            ClippedData = [];
        end
    end

    % Update full parameter vector
    CurrentTransParams(FreeParamIndices) = FreeParams;

    % Final cost and residuals
    [Residuals, Cost, ~] = imUtil.calib.transmissionFun(Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
        CurrentX, CurrentY, updateFullParams(FixedValues, FreeParams, FreeParamIndices), Model, PolyCheb, ...
        'FieldParams', FieldParams, 'GaiaWavelength', Args.GaiaWavelength, ...
        'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
        'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, 'Verbose', false);

    OptTransParams = CurrentTransParams;
end


%% ========================================================================
%  HELPER FUNCTION: Sigma Clipping
%  ========================================================================

function [ClippedData, NumOutliers] = applySigmaClipping(Spec, SpecErr, Flux, FluxErr, X, Y, Residuals, Threshold)
    % Apply sigma clipping using robust statistics
    MedianResid = median(Residuals);
    MAD = median(abs(Residuals - MedianResid));
    RobustStd = 1.4826 * MAD;

    OutlierMask = abs(Residuals - MedianResid) > Threshold * RobustStd;
    GoodMask = ~OutlierMask;
    NumOutliers = sum(OutlierMask);

    ClippedData = struct();
    ClippedData.Spec = Spec(:, GoodMask);  % Column indexing for [N_GaiaWvl x N_calib]
    ClippedData.SpecErr = SpecErr(:, GoodMask);  % Column indexing for [N_GaiaWvl x N_calib]
    ClippedData.Flux = Flux(GoodMask);
    ClippedData.FluxErr = FluxErr(GoodMask);
    ClippedData.X = X(GoodMask);
    ClippedData.Y = Y(GoodMask);
end

