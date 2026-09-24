function [Model, Results] = transmissionFit1(Lambda, Spec, SpecErr, Flux, FluxErr, Args)
    % Fit transmission parameters using integrated Tran2D position corrections.
    % The function performs optimization to fit free transmission parameters to
    % observations for a single image in the transmission-based photometric calibration pipeline.
    % The composite transmission model includes wavelength-dependent basic component functions and
    % position-dependent corrections integrated via Tran2D within CompositeFun.
    % The fit is done by comparing the instrumental fluxes of stars in the image to the synthetic
    % photometry of the stars given their spectrum and the transmission function which have free parameters.
    % Based on Garrappa et al. 2025, A&A 699, A50.
    %
    % Input  : - Lambda - Wavelength grid for integration range in nm
    %          - Spec - Gaia XP spectra matrix [N_GaiaWvl x N_calib]
    %                   Each column is the flux spectrum for one calibrator
    %          - SpecErr - Gaia XP spectra errors matrix [N_GaiaWvl x N_calib]
    %                   Each column is the flux error spectrum (currently unused)
    %          - Flux - Observed flux for calibrators (vector of N_calib values)
    %          - FluxErr - Observed flux errors for calibrators (vector of N_calib values)
    %          * ...,key,val,...
    %            'X' - Source X coordinates for calibrators (vector of N_calib values).
    %                   Default is [].
    %            'Y' - Source Y coordinates for calibrators (vector of N_calib values).
    %                   Default is [].
    %            'TransmissionFunctions' - Explicit specification for the
    %                                      basic components of composite transmission function
    %                                      (astro.transmission.* for atmospheric transmission,
    %                                      telescope.detector.* and telescope.optics.* for
    %                                      instrumental transmission). Will be used to compile transmission model
    %                                      in form of tools.math.fun.CompositeFun object).
    %                                      Struct array with function specifications. Default is [].
    %            'OptimizationSequence' -  Explicit optimization sequence (number of stages, free parameters to
    %                                      each stage, sigma clipping yes/no, number of iterations).
    %                                      Struct array with optimization stage specifications. Default is [].
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
    %            'ValInp' - Validate inputs (logical). Default is true.
    %            'Verbose' - Enable verbose output. Default is true.
    % Output : - Model - Fitted CompositeFun object with optimized parameters.
    %                   Includes both wavelength transmission parameters and Tran2D
    %                   position corrections (accessible via Model.Tran2DObj.ParX).
    %          - Results - Struct array with results from each optimization
    %                   stage: StageName (indicating which parameter(s) is optimized),
    %                   Method (linear/nonlinear), Cost after stage was completed,
    %                   RMS of residuals, Residuals vector of length of NumCalibrators,
    %                   NumCalibrators after stage was completed, isFieldCorrection (0/1).  
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
    %          YAMLPath = '~/matlab/AstroPack/config/CalibPhotAB.yml';
    %          [Model, Results] = imUtil.calib.transmissionFit1(...
    %              Lambda, Spec, SpecErr, Flux, FluxErr, 'X', X, 'Y', Y, ...
    %              'YAMLConfig', YAMLPath);
    %
    %          % Example 2: Without YAML - explicit 2-stage optimization with Tran2D
    %          % Prepare data (20 calibrators for stable position correction fitting)
    %          Lambda = linspace(336, 1020, 343)';
    %          % Generate 20 synthetic spectra with varying spectral indices
    %          N_calib = 20;
    %          Spec = zeros(343, N_calib);
    %          for i = 1:N_calib
    %              % Spectral index varies from -2 (blue) to +1.5 (red)
    %              alpha = -2 + 3.5 * (i-1)/(N_calib-1);
    %              Spec(:, i) = (3e-17) ./ (Lambda / 500).^alpha;
    %          end
    %          SpecErr = 0.05 * Spec;  % 5% errors
    %          % Observed fluxes with some scatter
    %          Flux = (8e4 + 4e4*rand(N_calib, 1));  % 80k-120k photons
    %          FluxErr = 0.05 * Flux;
    %          % Pixel coordinates distributed across 1726x1726 LAST field
    %          X = 200 + 1300 * rand(N_calib, 1);  % Random X: 200-1500
    %          Y = 200 + 1300 * rand(N_calib, 1);  % Random Y: 200-1500
    %          % Define transmission functions as struct array
    %          TransFunList(1).name = 'Ozone';
    %          TransFunList(1).handle = '@astro.transmission.ozoneTransmission';
    %          TransFunList(1).handletype = 'named';
    %          TransFunList(1).params = [30, 300];
    %          TransFunList(1).paraminfo(1).name = 'ZenithAngle_deg';
    %          TransFunList(1).paraminfo(1).min = 0;
    %          TransFunList(1).paraminfo(1).max = 90;
    %          TransFunList(1).paraminfo(2).name = 'DobsonUnits';
    %          TransFunList(1).paraminfo(2).min = 200;
    %          TransFunList(1).paraminfo(2).max = 400;
    %          TransFunList(2).name = 'Aerosol';
    %          TransFunList(2).handle = '@astro.transmission.aerosolTransmission';
    %          TransFunList(2).handletype = 'named';
    %          TransFunList(2).params = [30, 0.05, 1.2];
    %          TransFunList(2).paraminfo(1).name = 'ZenithAngle_deg';
    %          TransFunList(2).paraminfo(1).min = 0;
    %          TransFunList(2).paraminfo(1).max = 90;
    %          TransFunList(2).paraminfo(2).name = 'TauAod500';
    %          TransFunList(2).paraminfo(2).min = 0.0;
    %          TransFunList(2).paraminfo(2).max = 0.5;
    %          TransFunList(2).paraminfo(3).name = 'Alpha';
    %          TransFunList(2).paraminfo(3).min = 0.5;
    %          TransFunList(2).paraminfo(3).max = 2.5;
    %          % Define 2-stage optimization sequence as struct array
    %          OptSeq(1).stagename = 'AerosolOpt';
    %          OptSeq(1).freeparams(1).function = 'Aerosol';
    %          OptSeq(1).freeparams(1).parameter = 'TauAod500';
    %          OptSeq(1).sigmaclip = true;
    %          OptSeq(1).sigmathresh = 3.0;
    %          OptSeq(1).sigmaiter = 3;
    %          OptSeq(1).description = 'Optimize aerosol optical depth';
    %          OptSeq(2).stagename = 'Tran2DFieldCorr';
    %          OptSeq(2).freeparams = [];  % Empty for Tran2D position correction (linear fit)
    %          OptSeq(2).sigmaclip = true;
    %          OptSeq(2).sigmathresh = 2.0;
    %          OptSeq(2).sigmaiter = 2;
    %          OptSeq(2).description = 'Tran2D position correction (linear, 10 params)';
    %          % Run fitting (transmissionModel1 called internally with integrated Tran2D)
    %          [Model, Results] = imUtil.calib.transmissionFit1(...
    %              Lambda, Spec, SpecErr, Flux, FluxErr, 'X', X, 'Y', Y, ...
    %              'TransmissionFunctions', TransFunList, 'OptimizationSequence', OptSeq);
    %          % Access fitted Tran2D position correction parameters:
    %          PosParams = Model.Tran2DObj.ParX;  % [kx0, kx, kx2, kx3, kx4, ky, ky2, ky3, ky4, kxy]
    %          fprintf('Fitted position parameters: [%.6f, %.6f, ...]\n', PosParams(1), PosParams(2));
    %          % Evaluate transmission with position corrections at any (X,Y):
    %          X_test = [863.5; 500; 1400];
    %          Y_test = [863.5; 500; 1400];
    %          Trans = Model.evaluateAllParInputWithPosition(Lambda, X_test, Y_test);

    arguments
        Lambda                      % Wavelength vector
        Spec                        % Gaia XP spectra matrix [N_GaiaWvl x N_calib]
        SpecErr                     % Gaia XP spectra errors [N_GaiaWvl x N_calib]
        Flux                        % Observed flux: vector of N_calib fluxes
        FluxErr                     % Observed flux errors: vector of N_calib flux errors
        Args.X = []                 % X coordinates: vector of N_calib elements
        Args.Y = []                 % Y coordinates: vector of N_calib elements
        Args.TransmissionFunctions = []  % Explicit transmission functions (optional)
        Args.OptimizationSequence = []  % Explicit optimization sequence (optional)
        Args.GaiaWavelength = linspace(336, 1020, 343)'
        Args.YAMLConfig = []        % YAML config (optional)
        Args.Airmass = 1.2          % From observations metadata
        Args.Temperature = 15       % From observations metadata
        Args.DefaultPressure_mbar = 965  % Atmospheric pressure
        Args.ExpTime = 20           % For LAST
        Args.Aperture_area_m2 = pi * (0.1397)^2 % For LAST
        Args.ValInp logical = true  % Validate inputs
        Args.Verbose logical = true
    end

    if Args.Verbose
        fprintf('\n=== TRANSMISSION-BASED PHOTOMETRIC CALIBRATION ===\n\n');
    end

    % ====================================================================
    % STEP 1: VALIDATE AND PREPARE INPUT DATA (if enabled)
    % ====================================================================

    if Args.ValInp
        % Validate Lambda
        if ~isvector(Lambda) || isempty(Lambda)
            error('Lambda must be a non-empty vector');
        end
    end
    Lambda = Lambda(:);  % Ensure column vector 
    % Get dimensions
    [N_GaiaWvl, NumCalibrators] = size(Spec);

    if Args.ValInp
        % Validate Spec dimensions
        if ~ismatrix(Spec) || isempty(Spec)
            error('Spec must be a non-empty matrix [N_GaiaWvl x N_calib]');
        end

        % Validate SpecErr dimensions
        if ~ismatrix(SpecErr) || isempty(SpecErr)
            error('SpecErr must be a non-empty matrix [N_GaiaWvl x N_calib]');
        end
        if ~isequal(size(SpecErr), size(Spec))
            error('SpecErr dimensions [%d x %d] must match Spec dimensions [%d x %d]', ...
                  size(SpecErr, 1), size(SpecErr, 2), N_GaiaWvl, NumCalibrators);
        end

        % Validate and enforce column vectors for Flux, FluxErr
        if ~isvector(Flux) || length(Flux) ~= NumCalibrators
            error('Flux must be a vector with %d elements (number of calibrators)', NumCalibrators);
        end

        if ~isvector(FluxErr) || length(FluxErr) ~= NumCalibrators
            error('FluxErr must be a vector with %d elements (number of calibrators)', NumCalibrators);
        end

        % Validate X, Y if provided
        if ~isempty(Args.X)
            if ~isvector(Args.X) || length(Args.X) ~= NumCalibrators
                error('X must be a vector with %d elements (number of calibrators)', NumCalibrators);
            end
        end

        if ~isempty(Args.Y)
            if ~isvector(Args.Y) || length(Args.Y) ~= NumCalibrators
                error('Y must be a vector with %d elements (number of calibrators)', NumCalibrators);
            end
        end
    end

    % Ensure column vectors 
    Flux = Flux(:);
    FluxErr = FluxErr(:);
    if ~isempty(Args.X)
        Args.X = Args.X(:);
    end
    if ~isempty(Args.Y)
        Args.Y = Args.Y(:);
    end

    if Args.Verbose
        fprintf('Input validation complete:\n');
        fprintf('  Lambda: %d wavelength points\n', length(Lambda));
        fprintf('  Spec: [%d x %d] (GaiaWvl x Calibrators)\n', N_GaiaWvl, NumCalibrators);
        fprintf('  Flux, FluxErr, X, Y: [%d x 1] each\n\n', NumCalibrators);
    end

    % ====================================================================
    % STEP 2: LOAD CONFIGURATION AND BUILD TRANSMISSION MODEL
    % ====================================================================

    [TransFunList, OptSequence, Pressure_mbar] = loadConfiguration(Args);

    if Args.Verbose
        fprintf('Building transmission model (CompositeFun)...\n');
    end

    Model = imUtil.calib.transmissionModel1(TransFunList, ...
        'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
        'Pressure_mbar', Pressure_mbar, 'UseTran2D', true, ...
        'Verbose', Args.Verbose);

    NumStages = length(OptSequence);

    if Args.Verbose
        fprintf('Optimization sequence: %d stages\n\n', NumStages);
    end

    % ====================================================================
    % STEP 3: INITIALIZE CALIBRATOR DATA
    % ====================================================================

    % Prepare current calibrator data structure
    CurrentSpec = Spec;
    CurrentSpecErr = SpecErr;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = Args.X;
    CurrentY = Args.Y;

    if Args.Verbose
        fprintf('Initial calibrators: %d\n', NumCalibrators);
        AllPar = Model.getAllParStruct();
        fprintf('Initial transmission parameters: [%.3f, %.3f, ...]\n', ...
                AllPar.Values(1), AllPar.Values(2));
        fprintf('Initial position parameters: [%.3f, %.3f, ...]\n\n', ...
                Model.Tran2DObj.ParX(1), Model.Tran2DObj.ParX(2));
    end

    % ====================================================================
    % STEP 4: RUN OPTIMIZATION SEQUENCE
    % ====================================================================

    Results = struct([]);

    for IStage = 1:NumStages
        Stage = OptSequence(IStage);

        StageName = Stage.stagename;
        FreeParamsStage = Stage.freeparams;

        % FreeParamsStage should be a struct array with fields: .function, .parameter
        % Or empty [] for field correction

        SigmaClip = Stage.sigmaclip;
        SigmaThresh = Stage.sigmathresh;
        SigmaIter = Stage.sigmaiter;

        % Detect field correction stage (empty freeparams) - linear
        % Transmission parameter stages are nonlinear
        IsFieldCorrectionStage = isempty(FreeParamsStage);

        % The Tran2D.fitDesignMatrix method uses standard least squares

        % Determine method type (needed for Results struct)
        if IsFieldCorrectionStage
            Method = 'linear';
        else
            Method = 'nonlinear';
        end

        if Args.Verbose
            fprintf('--- Stage %d/%d: %s [%s] ---\n', IStage, NumStages, StageName, Method);
            fprintf('Description: %s\n', Stage.description);
        end

        if IsFieldCorrectionStage
            % =================================================================
            % FIELD CORRECTION STAGE (Linear optimization using Tran2D)
            % =================================================================
            if Args.Verbose
                fprintf('Field correction stage: optimizing Tran2D spatial parameters\n');
            end

            % Run field correction optimization using integrated Tran2D methods
            [Cost, Residuals, ClippedData] = optimizeFieldCorrection(...
                Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                Model, SigmaClip, SigmaThresh, SigmaIter, Args);

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
            % =================================================================
            % TRANSMISSION PARAMETER STAGE (Nonlinear optimization)
            % =================================================================

            % Set FitPar flags for this stage's parameters
            AllPar = Model.getAllParStruct();
            AllPar.FitPar(:) = false;  % Reset all to false

            % Set FitPar for parameters specified in this stage
            for I = 1:length(FreeParamsStage)
                FunctionName = FreeParamsStage(I).function;
                ParameterName = FreeParamsStage(I).parameter;
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

            % Run transmission parameter optimization using integrated approach
            [Cost, Residuals, ClippedData] = optimizeTransmission(...
                Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, CurrentX, CurrentY, ...
                Model, FreeParamIndices, SigmaClip, SigmaThresh, SigmaIter, Args);

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
        Results(IStage).StageName = StageName;
        Results(IStage).Method = Method;
        Results(IStage).Cost = Cost;
        Results(IStage).RMS = RMS;
        Results(IStage).Residuals = Residuals;
        Results(IStage).NumCalibrators = length(Residuals);
        Results(IStage).IsFieldCorrection = IsFieldCorrectionStage;

        if Args.Verbose
            fprintf('Stage complete: Cost=%.4e, RMS=%.4f mag, Calibrators=%d\n\n', ...
                    Cost, RMS, length(Residuals));
        end
    end

    % ====================================================================
    % STEP 5: FINALIZE OUTPUT
    % ====================================================================

    if Args.Verbose
        fprintf('=== OPTIMIZATION COMPLETE ===\n\n');
        fprintf('Final transmission parameters:\n');
        AllPar = Model.getAllParStruct();
        for I = 1:length(AllPar.Values)
            fprintf('  [%d] %s: %.6f\n', I, AllPar.Names{I}, AllPar.Values(I));
        end
        fprintf('\nFinal Tran2D position correction parameters:\n');
        FieldParamNames = {'kx0', 'kx', 'kx2', 'kx3', 'kx4', 'ky', 'ky2', 'ky3', 'ky4', 'kxy'};
        for I = 1:10
            fprintf('  [%d] %s: %.12f\n', I, FieldParamNames{I}, Model.Tran2DObj.ParX(I));
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
%  HELPER FUNCTION: Optimize Field Correction (using integrated Tran2D)
%  ========================================================================

function [Cost, Residuals, ClippedData] = optimizeFieldCorrection(...
    Lambda, Spec, SpecErr, Flux, FluxErr, X, Y, Model, ...
    SigmaClip, SigmaThresh, SigmaIter, Args)
    % Optimize Tran2D position correction parameters using integrated methods.
    % Uses Model.fitPositionPolynomial() for linear optimization (standard least squares).

    % Initialize current data
    CurrentSpec = Spec;
    CurrentSpecErr = SpecErr;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;

    % Set number of iterations: 1 if no sigma clipping, SigmaIter otherwise
    NumIterations = 1;
    if SigmaClip
        NumIterations = SigmaIter;
    end

    % Optimization loop with optional sigma clipping
    for Iter = 1:NumIterations
        % Calculate magnitude residuals without position correction
        % (temporary set Tran2D parameters to zero)
        SavedParX = Model.Tran2DObj.ParX;
        Model.Tran2DObj.ParX = zeros(1, length(SavedParX));

        % Calculate base magnitudes using transmissionCostFun1
        [MagResid0, ~, ~] = imUtil.calib.transmissionCostFun1(...
            Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
            Model, 'X', CurrentX, 'Y', CurrentY, ...
            'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, ...
            'Verbose', false);

        % Restore saved parameters
        Model.Tran2DObj.ParX = SavedParX;

        % Fit position polynomial using integrated Tran2D method
       
        [FitResult, Model] = Model.fitPositionPolynomial(CurrentX, CurrentY, MagResid0, ...
            'Verbose', false);

        % Calculate residuals with optimized position correction
        [Residuals, ~, ~] = imUtil.calib.transmissionCostFun1(...
            Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
            Model, 'X', CurrentX, 'Y', CurrentY, ...
            'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, ...
            'Verbose', false);

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
    [Residuals, Cost, ~] = imUtil.calib.transmissionCostFun1(...
        Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
        Model, 'X', CurrentX, 'Y', CurrentY, ...
        'GaiaWavelength', Args.GaiaWavelength, ...
        'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
        'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, ...
        'Verbose', false);

    % Build final ClippedData structure with updated calibrator data
    ClippedData = struct();
    ClippedData.Spec = CurrentSpec;
    ClippedData.SpecErr = CurrentSpecErr;
    ClippedData.Flux = CurrentFlux;
    ClippedData.FluxErr = CurrentFluxErr;
    ClippedData.X = CurrentX;
    ClippedData.Y = CurrentY;
end

%% ========================================================================
%  HELPER FUNCTION: Optimize Transmission Parameters (using integrated Tran2D)
%  ========================================================================

function [Cost, Residuals, ClippedData] = optimizeTransmission(...
    Lambda, Spec, SpecErr, Flux, FluxErr, X, Y, Model, ...
    FreeParamIndices, SigmaClip, SigmaThresh, SigmaIter, Args)
    % Optimize transmission parameters using nonlinear least squares (lsqNonLinWithFixed).
    % Uses integrated Tran2D approach with transmissionCostFun1.

    % Optimization loop (with or without sigma clipping)
    CurrentSpec = Spec;
    CurrentSpecErr = SpecErr;
    CurrentFlux = Flux;
    CurrentFluxErr = FluxErr;
    CurrentX = X;
    CurrentY = Y;

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

        % Get bounds and current parameter values
        AllPar = Model.getAllParStruct();
        CurrentTransParams = AllPar.Values;

        % Setup FitMask for all parameters
        FitMask = false(size(CurrentTransParams));
        FitMask(FreeParamIndices) = true;

        % Model function for lsqNonLinWithFixed (returns residuals for all calibrators)
        % X_dummy is ignored, transmissionCostFun1 returns [Residuals, Cost, PredictedFlux]
        % P is the TransParams vector being optimized by lsqnonlin
        ModelFun = @(X_dummy, P) imUtil.calib.transmissionCostFun1(...
            Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
            Model, 'TransParams', P, 'X', CurrentX, 'Y', CurrentY, ...
            'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, ...
            'Verbose', false);

        % Dummy X (calibrator indices), Y = 0 (fit residuals to zero), uniform weights
        NumCalib = length(CurrentFlux);
        X_dummy = (1:NumCalib)';
        Y_target = zeros(NumCalib, 1);
        Sigma_weights = ones(NumCalib, 1);

        % Call lsqNonLinWithFixed
        [OptTransParams, ~, ~] = tools.math.fit.lsqNonLinWithFixed(...
            X_dummy, Y_target, Sigma_weights, ModelFun, ...
            'InitPar', CurrentTransParams, ...
            'FitPar', FitMask, ...
            'Lb', AllPar.Min, ...
            'Ub', AllPar.Max, ...
            'Opts', Opts);

        % Update Model with optimized parameters
        AllPar.Values = OptTransParams;
        Model.setAllParStruct(AllPar);

        % Calculate residuals with updated parameters
        [Residuals, ~, ~] = imUtil.calib.transmissionCostFun1(...
            Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
            Model, 'X', CurrentX, 'Y', CurrentY, ...
            'GaiaWavelength', Args.GaiaWavelength, ...
            'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
            'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, ...
            'Verbose', false);

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

    % Final cost and residuals
    [Residuals, Cost, ~] = imUtil.calib.transmissionCostFun1(...
        Lambda, CurrentSpec, CurrentSpecErr, CurrentFlux, CurrentFluxErr, ...
        Model, 'X', CurrentX, 'Y', CurrentY, ...
        'GaiaWavelength', Args.GaiaWavelength, ...
        'Airmass', Args.Airmass, 'Temperature', Args.Temperature, ...
        'ExpTime', Args.ExpTime, 'Aperture_area_m2', Args.Aperture_area_m2, ...
        'Verbose', false);

    % Build final ClippedData structure with updated calibrator data
    ClippedData = struct();
    ClippedData.Spec = CurrentSpec;
    ClippedData.SpecErr = CurrentSpecErr;
    ClippedData.Flux = CurrentFlux;
    ClippedData.FluxErr = CurrentFluxErr;
    ClippedData.X = CurrentX;
    ClippedData.Y = CurrentY;
end


%% ========================================================================
%  HELPER FUNCTION: Load Configuration
%  ========================================================================

function [TransFunList, OptSequence, Pressure_mbar] = loadConfiguration(Args)
    % Load transmission functions and optimization sequence from YAML or explicit args
    %
    % Input  : - Args - Arguments structure from main function
    % Output : - TransFunList - Struct array of transmission functions
    %          - OptSequence - Struct array of optimization stages
    %          - Pressure_mbar - Atmospheric pressure value

    % Determine source of configuration
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

        % Convert YAML cell arrays to struct arrays
        if iscell(YAMLConfig.TransmissionFunctions)
            % Convert cell array to struct array
            NumFunctions = length(YAMLConfig.TransmissionFunctions);
            TransFunList = struct([]);
            for I = 1:NumFunctions
                Func = YAMLConfig.TransmissionFunctions{I};
                TransFunList(I).name = Func.name;
                TransFunList(I).handle = Func.handle;
                TransFunList(I).handletype = Func.handletype;
                TransFunList(I).params = Func.params;
                TransFunList(I).paraminfo = Func.paraminfo;
            end
        else
            TransFunList = YAMLConfig.TransmissionFunctions;
        end

        if iscell(YAMLConfig.OptimizationSequence)
            % Convert cell array to struct array, handling inconsistent fields
            NumStages = length(YAMLConfig.OptimizationSequence);
            OptSequence = struct([]);
            for I = 1:NumStages
                Stage = YAMLConfig.OptimizationSequence{I};
                % Copy all fields from YAML stage
                OptSequence(I).stagename = Stage.stagename;
                OptSequence(I).method = Stage.method;
                OptSequence(I).freeparams = Stage.freeparams;
                OptSequence(I).sigmaclip = Stage.sigmaclip;
                OptSequence(I).sigmathresh = Stage.sigmathresh;
                OptSequence(I).sigmaiter = Stage.sigmaiter;
                OptSequence(I).description = Stage.description;
                % Add regularization if present, otherwise default to 0
                if isfield(Stage, 'regularization')
                    OptSequence(I).regularization = Stage.regularization;
                else
                    OptSequence(I).regularization = 0;
                end
            end
        else
            OptSequence = YAMLConfig.OptimizationSequence;
        end

        % Convert freeparams from YAML cell array format to struct array
        for I = 1:length(OptSequence)
            if isfield(OptSequence(I), 'freeparams') && iscell(OptSequence(I).freeparams)
                FreeParamsCell = OptSequence(I).freeparams;
                if ~isempty(FreeParamsCell)
                    % Convert cell array of pairs to struct array
                    FreeParamsStruct = struct([]);
                    for J = 1:length(FreeParamsCell)
                        ParamPair = FreeParamsCell{J};
                        FreeParamsStruct(J).function = ParamPair{1};
                        FreeParamsStruct(J).parameter = ParamPair{2};
                    end
                    OptSequence(I).freeparams = FreeParamsStruct;
                else
                    % Empty cell array becomes empty struct array
                    OptSequence(I).freeparams = [];
                end
            end
        end

        if isfield(YAMLConfig, 'DefaultPressure_mbar')
            Pressure_mbar = YAMLConfig.DefaultPressure_mbar;
        else
            Pressure_mbar = Args.DefaultPressure_mbar;
        end
    end
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