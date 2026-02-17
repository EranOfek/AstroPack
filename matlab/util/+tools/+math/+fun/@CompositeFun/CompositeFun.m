classdef CompositeFun < handle
    % This class provides a unified interface for combining multiple functions 
    % (and, possibly, Tran2D object for position-dependent component and the sequence of optimization for transmission parameters)
    % with parameter mapping and optimization support. 
    %
    % Author : D. Kovaleva (Oct 2025 - Dec 2025)
    %
    % Example - Basic Setup:
    %   % Create composite function
    %{   
        Model = tools.math.fun.CompositeFun();
    
       % Adding functions
       % Method 1: Auto-extract ArgNames from function handle(recommended)
       Model.addFun('Ozone transmission', @astro.transmission.ozoneTransmission, [], 'Par', [30, 300], 'FitPar', [false, false]);
       Model.addFun('Aerosol transmission', @astro.transmission.aerosolTransmission, [], 'Par', [30, 0.05, 1.2], 'FitPar', [false, true, false]);
       
       % Method 2: Explicit extractArgFuns helper function
       OzoneArgNames = Model.extractArgFuns(@astro.transmission.ozoneTransmission);
       Model.addFun('Ozone transmission', @astro.transmission.ozoneTransmission, OzoneArgNames, ...
                    'Par', [45, 300], 'FitPar', [false, true]);
    
       % Method 3: Manual ArgNames construction
       AerosolArgNames = struct('Name', {1, 2, 3}, ...
                               'Description', {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'}, ...
                               'Min', {0, 0.01, 0.5}, 'Max', {90, 0.5, 2.0});
       Model.addFun('Aerosol transmission', @astro.transmission.aerosolTransmission, AerosolArgNames, ...
                    'Par', [30, 0.05, 1.2], 'FitPar', [false, true, false]);
    
       % Method 4: Direct function call for ArgNames
       OzoneArgNames = astro.transmission.ozoneTransmission('GetArgNames', true);
    
       % Adding simple mathematical functions
       % Sin function: y = A * sin(x + B)
       SinArgNames = struct('Name', {1, 2}, 'Description', {'SinAmplitude', 'Phase'}, 'Min', {0, -pi}, 'Max', {10, pi});
       Model.addFun('Sine function', @(x, par) par(1) * sin(x + par(2)), SinArgNames, 'Par', [1, 0], 'FitPar', [true, false]);
    
       % Cos function: y = C * cos(D * x)
       CosArgNames = struct('Name', {1, 2}, 'Description', {'CosAmplitude', 'Frequency'}, 'Min', {0, 0}, 'Max', {5, 10});
       Model.addFun('Cosine function', @(x, par) par(1) * cos(par(2) * x), CosArgNames, 'Par', [2, 1], 'FitPar', [false, true]);
    
       % After functions are added, can also get ArgNames from model
       ArgNames1 = Model.Funs(1).ArgNames;     % Same as OzoneArgNames
       ArgNames2 = Model.Funs(2).ArgNames;     % Same as AerosolArgNames
    
       % It is possible to add function(s) without setting parameter values (NaN by default),
       % but fixed parameters (FitPar=false) must be set before calculations:
       Model.addFun('Aerosol transmission', @astro.transmission.aerosolTransmission, [], 'Par', [], 'FitPar', [false, true, false]);
       AllFunPar = Model.getAllFunPar();  % Get parameter structure
       % AllFunPar.Name shows parameter names and their global indices
       AllFunPar.Val(1) = 30;   % Set ZenithAngle_deg (fixed parameter)
       AllFunPar.Val(3) = 1.2;  % Set AngstromExponent (fixed parameter)
       % Parameter 2 (TauAod500) will be fitted, so can remain NaN initially
       Model.setAllFunPar(AllFunPar);  % Apply the values 
    
    % Example - Information Getters:
       % Get function summary
       FunsNames = Model.namesFuns();
       fprintf('Added %d functions\n', size(FunsNames, 1));
    
       % Get parameter information
       fprintf('Total parameters: %d\n', Model.numAllFunPar());
       fprintf('Fitted parameters: %d\n', Model.numFittedPar());
    
       AllNames = Model.namesAllFunPar();
       AllValues = Model.valuesAllFunPar();
       FittedNames = Model.namesFittedPar();
       FittedInfo = Model.getFittedPar();
    
       % Get detailed function information
       AllFuns = Model.allFunsStruct();    % Complete structure with all fields
       FunsNames = Model.namesFuns();      % Cell array: {Name, Description}
    
     % Example - Dynamic Parameter Management:
       % Get current parameter structure
       AllFunPar = Model.getAllFunPar();
    
       % Modify parameters and fit flags for optimization
       AllFunPar.Val(2) = 350;      % Change ozone value
       AllFunPar.FitPar(1) = false;    % Fix zenith angle
       AllFunPar.FitPar(3) = true;     % Fit aerosol parameter
       Model.setAllFunPar(AllFunPar);  % Handle class - modifies in place
    
      % Example - Pre-calculation and Evaluation:
       % Define wavelength grid for evaluation
       Lambda = (3000:20:11000)';  % Transmission wavelength grid [Angstrom], 401 points

       % Pre-calculate functions with fixed parameters (after setting fit flags)
       Model.preCalc(Lambda);
    
       % Method 1: Evaluate with all parameter values (direct input)
       NewAllValues = [45, 280, 0.08, 0.6];  % All parameters
       Transmission = Model.evaluateAllFunParInput(Lambda, NewAllValues);
    
       % Method 2: Evaluate with only fitted parameters (fixed parameters pre-set)
       % First set all fixed parameters using setAllFunParStruct (if not
       % set already)
       AllFunPar = Model.getAllFunPar();
       AllFunPar(1).Value = 45;    % Set zenith angle (fixed)
       AllFunPar(1).FitPar = false;
       AllFunPar(2).Value = 280;   % Set ozone value (fixed)
       AllFunPar(2).FitPar = false;
       AllFunPar(3).FitPar = true; % Fit aerosol AOD
       AllFunPar(4).FitPar = true; % Fit Angstrom exponent
       Model.setAllFunPar(AllFunPar);
       % Now evaluate with only fitted parameters
       FittedValues = [0.08, 0.6];  % Only fitted parameters
       Transmission = Model.evaluate(Lambda, FittedValues);
    
    % Example - Position-Dependent Corrections with Tran2D:
       % Step 1: Build base wavelength transmission model
       Model = tools.math.fun.CompositeFun();
       Model.addFun('Ozone', @astro.transmission.ozoneTransmission, [], ...
                    'Par', [30, 300], 'FitPar', [false, true]);
       Model.addFun('Aerosol', @astro.transmission.aerosolTransmission, [], ...
                    'Par', [30, 0.05, 1.2], 'FitPar', [false, true, false]);
    
       % Step 2: Create Tran2D object for position-dependent correction
       % Position correction is additive in magnitude space
       T2D = Tran2D('cheby1_4_xt');  % LAST field correction (10 parameters)
       % Set normalization for Chebyshev polynomials: [0, 1726] → [-1, +1]
       T2D.ParNX = [863, 863];  % (x - 863) / 863 maps center to 0, edges to ±1 for LAST 1726*1726 pix subimage
       T2D.ParNY = [863, 863];  % (y - 863) / 863 maps center to 0, edges to ±1
       % Initialize polynomial coefficients (will be fitted later)
       T2D.ParX = zeros(1, 10);  % [kx0, kx, kx2, kx3, kx4, ky, ky2, ky3, ky4, kxy]
       T2D.ParY = zeros(1, 10);  % Not used for photometry, but required by Tran2D
    
       % Step 3: Add Tran2D class object to the model with reference position
       X_ref = 863;  % Field center in pixel coordinates
       Y_ref = 863;  % (maps to (0,0) in normalized Chebyshev coordinates)
       Model.addTran2D(T2D, 'X_ref', X_ref, 'Y_ref', Y_ref, 'Verbose', true);
    
       % Step 4: Evaluate transmission at specific positions and wavelengths
       Lambda = linspace(4000, 9000, 100)';  % Wavelength grid [Angstrom]
       X = [200; 863; 1500];               % Source X positions [pixels]
       Y = [200; 863; 1500];               % Source Y positions [pixels]
    
       % Get transmission matrix [N_sources x N_lambda]
       Trans = Model.evaluateWithPosition(Lambda, X, Y);
       % Trans(i,j) = transmission for source i at wavelength j
    
       % Step 5: Update position coefficients (e.g., after optimization)
       PosParams_new = [0.01, 0.0001, -0.0002, 0.00015,-0.001, 0.1,-0.01, 0.001,-0.002,0.01];  % Fitted values
       Model.setTran2DParams(PosParams_new);
    
       % Step 7: Get combined parameter structure (wavelength + position)
       AllFunParams = Model.getAllFunParWithTran2D();
       fprintf('Total parameters: %d\n', length(AllFunParams.Val));
       fprintf('  Wavelength transmission: %d\n', Model.numAllFunPar());
       fprintf('  Position polynomial: %d\n', length(Model.Tran2DObj.ParX));
    
    % Example - Model Building with CompositeFun.model function:
       % Define function specification list (e.g., from YAML or manual)
       FunList(1).name = 'Ozone';
       FunList(1).handle = '@astro.transmission.ozoneTransmission';
       FunList(1).handletype = 'named';
       FunList(1).params = [30, 300];
       FunList(1).paraminfo(1).name = 'ZenithAngle_deg';
       FunList(1).paraminfo(1).min = 0;
       FunList(1).paraminfo(1).max = 90;
       FunList(1).paraminfo(2).name = 'DobsonUnits';
       FunList(1).paraminfo(2).min = 200;
       FunList(1).paraminfo(2).max = 400;
    
       FunList(2).name = 'Aerosol';
       FunList(2).handle = '@astro.transmission.aerosolTransmission';
       FunList(2).handletype = 'named';
       FunList(2).params = [30, 0.05, 1.2];
       FunList(2).paraminfo(1).name = 'ZenithAngle_deg';
       FunList(2).paraminfo(1).min = 0;
       FunList(2).paraminfo(1).max = 90;
       FunList(2).paraminfo(2).name = 'TauAod500';
       FunList(2).paraminfo(2).min = 0.0;
       FunList(2).paraminfo(2).max = 0.5;
       FunList(2).paraminfo(3).name = 'Alpha';
       FunList(2).paraminfo(3).min = 0.5;
       FunList(2).paraminfo(3).max = 2.5;
    
       % Build transmission model with Tran2D position corrections and metadata injection
       Model = tools.math.fun.CompositeFun.model(FunList, ...
           'MetadataValues', {'ZenithAngle_deg', acosd(1.0/1.2), ...
                              'Pressure_mbar', 965, 'Temperature_C', 15}, ...
           'UseTran2D', true, 'Tran2DType', 'cheby1_4_xt', ...
           'XPixel', 1726, 'YPixel', 1726, 'Verbose', true);
       % Or build model without position corrections
       Model = tools.math.fun.CompositeFun.model(FunList, ...
           'MetadataValues', {'ZenithAngle_deg', acosd(1.0/1.2)}, ...
           'UseTran2D', false);
    
    % Example - Cost Function Evaluation with costFun:
       % Simple direct comparison (Ninput == NCalUsed)
       Lambda = linspace(4000, 9000, 100)';
       ObservedValues = randn(100, 1);  % Simulated observations
       [Residuals, Cost, Predicted] = Model.costFun(Lambda, ObservedValues);
    
       % With position corrections (requires X, Y coordinates)
       X = [200; 863; 1500];  % 3 sources
       Y = [200; 863; 1500];
       ObservedValues = randn(3, 1);
       [Residuals, Cost, Predicted] = Model.costFun(Lambda, ObservedValues, ...
           'X', X, 'Y', Y);
    
       % Transmission mode with spectra (for photometric calibration)
       Lambda = (3000:20:11000)';   % Transmission wavelength grid [Angstrom], 401 points
       SpecWvl = (3360:20:10200)';  % Calibrator spectra wavelength grid [Angstrom], 343 points (e.g., Gaia DR3 XP)
       Spec = randn(343, 3);  % Simulated calibrator spectra [N_CalibWvl x N_calib]
       ObsFlux = [1e5; 2e5; 1.5e5];  % Observed photon counts
       X = [500; 1000; 1500];  % Pixel coordinates
       Y = [500; 1000; 1500];
       [Residuals, Cost, PredFlux] = Model.costFun(Lambda, ObsFlux, ...
           'WeightMatrix', Spec, 'TransmissionMode', true, ...
           'CalibWavelength', SpecWvl, 'X', X, 'Y', Y, ...
           'ExpTime', 20, 'Aperture_area_m2', pi*0.1397^2);
       % Residuals are magnitude differences: 2.5*log10(Predicted/Observed)
    
    % Example - Parameter Fitting with fitPar:
       % Setup: Mark parameters for fitting
       AllFunPar = Model.getAllFunPar();
       AllFunPar.FitPar(1) = false;  % Fix ZenithAngle_deg
       AllFunPar.FitPar(2) = false;   % Fix DobsonUnits
       AllFunPar.FitPar(4) = true;   % Fit TauAod500
       Model.setAllFunPar(AllFunPar);
    
       % Fit wavelength parameters only (no position corrections)
       Lambda = linspace(4000, 9000, 100)';
       ObservedFlux = randn(20, 1);  % 20 observations
       X = 200 + 1300 * rand(20, 1);  % Random positions
       Y = 200 + 1300 * rand(20, 1);
       [Model, Result] = Model.fitPar(Lambda, ObservedFlux, ...
           'FitTransmission', true, 'FitPosition', false, 'Verbose', true);
       fprintf('Final RMS: %.4f\n', Result.RMS);

       % Fit both wavelength and position parameters with sigma clipping
       [Model, Result] = Model.fitPar(Lambda, ObservedFlux, ...
           'X', X, 'Y', Y, 'FitTransmission', true, 'FitPosition', true, ...
           'SigmaClip', true, 'SigmaThresh', 3.0, 'SigmaIter', 5, 'Verbose', true);
       fprintf('Final RMS: %.4f, Clipped: %d outliers\n', ...
               Result.RMS, Result.NumClipped);
    
     Example - Transmission Mode for Photometric Calibration:
       % Build transmission model with Tran2D
       Model = tools.math.fun.CompositeFun();
       Model.addFun('Ozone', @astro.transmission.ozoneTransmission, [], ...
           'Par', [30, 300], 'FitPar', [false, false]);
       Model.addFun('Aerosol', @astro.transmission.aerosolTransmission, [], ...
           'Par', [30, 0.05, 1.2], 'FitPar', [false, true, false]);
       % Add Tran2D for field-dependent corrections
       T2D = Tran2D('cheby1_4_xt');
       T2D.ParNX = [863, 863]; T2D.ParNY = [863, 863];
       T2D.ParX = zeros(1, 10); T2D.ParY = zeros(1, 10);
       Model.addTran2D(T2D, 'X_ref', 863, 'Y_ref', 863);
    
       % Prepare calibration data
       Lambda = (3000:20:11000)';   % Transmission wavelength grid [Angstrom], 401 points
       SpecWvl = (3360:20:10200)';  % Calibrator spectra wavelength grid [Angstrom], 343 points (e.g., Gaia DR3 XP)
       N_calib = 20;
       % Generate synthetic spectra with varying spectral indices
        Spec = zeros(343, N_calib);
        for i = 1:N_calib
            alpha = -2 + 3.5 * (i-1)/(N_calib-1);  % -2 (blue) to +1.5 (red)
            Spec(:, i) = (3e-17) ./ (SpecWvl / 500).^alpha;
        end
       ObsFlux = 8e4 + 4e4 * rand(N_calib, 1);  % Observed photons [80k-120k]
       X = rand(N_calib, 1) * 1726;  % Source X positions [pixels]
       Y = rand(N_calib, 1) * 1726;  % Source Y positions [pixels]
    
       % Setup CostArgs for TransmissionMode
       % WeightMatrix = calibrator spectra (default: Gaia DR3 XP, or synthetic/model spectra)
       CostArgs = {'WeightMatrix', Spec, 'TransmissionMode', true, ...
           'CalibWavelength', SpecWvl, 'ExpTime', 20, ...
           'Aperture_area_m2', pi * (0.1397)^2};
    
       % Fit transmission + position with sigma clipping
       % NOTE: FitPosition requires TransmissionMode (magnitude residuals)
       [Model, Result] = Model.fitPar(Lambda, ObsFlux, ...
           'CostArgs', CostArgs, 'X', X, 'Y', Y, ...
           'FitTransmission', true, 'FitPosition', true, ...
           'SigmaClip', true, 'SigmaThresh', 3.0, 'Verbose', true);
       fprintf('Final RMS: %.4f mag, Calibrators: %d/%d\n', ...
               Result.RMS, Result.NCalUsed, length(ObsFlux));
    
    % Example - Multi-Stage Optimization with OptimizationSequence:
       % Build transmission model with Tran2D (as above)
       Model = tools.math.fun.CompositeFun.model(FunList, ...
           'UseTran2D', true, 'Tran2DType', 'cheby1_4_xt', ...
           'XPixel', 1726, 'YPixel', 1726);
    
       % Define 2-stage optimization sequence
       % Stage 1: Fit aerosol optical depth with aggressive sigma clipping
       OptSeq(1).StageName = 'AerosolOpt';
       OptSeq(1).FreeParams(1).Function = 'Aerosol';
       OptSeq(1).FreeParams(1).Parameter = 'TauAod500';
       OptSeq(1).SigmaClip = true;
       OptSeq(1).SigmaThresh = 3.0;
       OptSeq(1).SigmaIter = 3;
       OptSeq(1).Description = 'Optimize aerosol optical depth';

       % Stage 2: Fit position-dependent field correction (linear fit)
       OptSeq(2).StageName = 'FieldCorr';
       OptSeq(2).FreeParams = [];  % Empty for field correction stage
       OptSeq(2).SigmaClip = true;
       OptSeq(2).SigmaThresh = 2.0;
       OptSeq(2).SigmaIter = 2;
       OptSeq(2).Description = 'Position-dependent field correction';
    
       % Prepare calibration data (same as above)
       Lambda = (300:2:1100)';   % Transmission wavelength grid [nm], 401 points
       SpecWvl = (336:2:1020)';  % Calibrator spectra wavelength grid [nm], 343 points (e.g., Gaia DR3 XP)
       N_calib = 20;
       Spec = zeros(343, N_calib);
       for i = 1:N_calib
           alpha = -2 + 3.5 * (i-1)/(N_calib-1);
           Spec(:, i) = (3e-17) ./ (SpecWvl / 500).^alpha;
       end
       ObsFlux = 8e4 + 4e4 * rand(N_calib, 1);
       X = rand(N_calib, 1) * 1726;
       Y = rand(N_calib, 1) * 1726;
    
       % Setup CostArgs for TransmissionMode
       CostArgs = {'WeightMatrix', Spec, 'TransmissionMode', true, ...
           'CalibWavelength', SpecWvl, 'ExpTime', 20, ...
           'Aperture_area_m2', pi * (0.1397)^2};
    
       % Run multi-stage optimization
       [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
           'CostArgs', CostArgs, 'X', X, 'Y', Y, ...
           'OptimizationSequence', OptSeq, 'Verbose', true);
    
       % Access per-stage results
       fprintf('Stage 1 (Aerosol):     RMS=%.4f mag, NCalUsed=%d\n', ...
               FitResult(1).RMS, FitResult(1).NCalUsed);
       fprintf('Stage 2 (Field Corr):  RMS=%.4f mag, NCalUsed=%d\n', ...
               FitResult(2).RMS, FitResult(2).NCalUsed);
    
       % Get fitted position correction parameters
       PosParams = Model.Tran2DObj.ParX;  % [kx0, kx, kx2, ..., kxy]
       fprintf('Field correction parameters: [%.6f, %.6f, ...]\n', ...
               PosParams(1), PosParams(2));
    
       % Evaluate transmission at new positions
       X_test = [863; 500; 1400];
       Y_test = [863; 500; 1400];
       Trans = Model.evaluateWithPosition(Lambda, X_test, Y_test);
    %}
    
    % Methods:
    %   Constructor: CompositeFun() - Create composite function object
    %   addFun() - Add transmission function with parameters
    %   preCalc() - Pre-calculate functions with fixed parameters
    %   evaluate() - Evaluate composite function
    %   evaluateAllFunParInput() - Evaluate composite function with all-
    %                             parameters input
    %   checkOverlappingParamConsistency() - Validate parameter consistency across functions
    %
    % High-Level Methods (Model Building and Optimization):
    %   model() - Static method to build CompositeFun from specification list
    %   costFun() - General cost function for optimization (residuals, cost, predictions)
    %   fitPar() - Fit parameters with sigma clipping and optional multi-stage optimization
    %                          Supports single-stage (default) or multi-stage via OptimizationSequence
    %   fitMultiStage() - Execute multi-stage optimization sequence
    %   fitMCMC() - MCMC sampling for parameter uncertainty estimation (uses mcmcstat)
    %
    % MCMC Support Methods:
    %   getFreeParamVector() - Extract free parameters as vector for MCMC
    %   setFreeParamVector() - Set free parameters from vector
    %   buildMCMCParams() - Build mcmcstat-compatible parameter cell array
    %   buildMCMCModel() - Build mcmcstat model structure with ssfun
    %
    % Methods using Tran2D class object (Position-Dependent Corrections):
    %   addTran2D() - Add Tran2D object for spatial corrections
    %   resetTran2DParams() - Reset Tran2D parameters to zeros
    %   evaluateWithPosition() - Evaluate transmission with position corrections
    %   fitPositionPolynomial() - Fit position polynomial using Tran2D (wraps fitDesignMatrix)
    %   normalizePositionPolynomial() - Normalize position polynomial at reference
    %
    % Setters:
    %   setAllFunPar() - Update Par (Model.Funs.Par, Model.Funs.FitPar)
    %   setTran2DParams() - Update Tran2D parameters from vector
    %
    % Getters:
    %   numAllFunPar() - Count of all parameters (fitted + fixed)
    %   namesAllFunPar() - Names of all parameters, cell array
    %   valuesAllFunPar() - Values of all parameters, vector
    %   numFittedPar() - Count of fitted parameters only
    %   namesFittedPar() - Names of fitted parameters only, cell array
    %   namesFuns() - Names and descriptions of added functions as cell array
    %   allFunsStruct() - Complete Funs structure array
    %   getFittedPar() - Comprehensive fitted parameter details,
    %                            structure array
    %   getAllFunPar() - Get Par structure array
    %   getTran2DPar() - Get Tran2D parameters in standard format
    %   getAllFunParWithTran2D() - Get combined Funs + Tran2D parameters
    %
    % Status Logging Utilities:
    %   addStatus() - Add a status entry (error/warning/info) to StatusLog
    %   getStatus() - Get status log entries, optionally filtered by level
    %   clearStatus() - Clear all status log entries
    %   hasErrors() - Check if any error-level status entries exist
    %   hasWarnings() - Check if any warning-level status entries exist
    %
    % Internal methods:
    %   extractArgFuns() - Extract argument information from function handles
    %   argMapping() - Map global parameters for newly added functions (builds from Funs structure)
    %   calculateChi2DOF() - Calculate chi-squared and degrees of freedom from residuals
    %


    properties
        Funs        = struct('Name',[], 'Desc','', 'Handle',[], 'Par',[], 'FitPar',[], 'OptionalArgs',{}, ...
                              'ArgNames',[], 'ArgMapping',[], 'PreCalc',[]);
                            % Name - Function name
                            % Desc - Function description
                            % Handle - Function handle to function of the
                            %       form: Y=F(X, Par, OptionaArgs{:})
                            %       X and Y can be arrays of any dim.
                            %       Par is a vector of parameters of the
                            %       function.
                            %       OptionalArgs is a cell array of
                            %       arbitrary number of additional
                            %       arguments.
                            % Par - A vector of parameters.
                            % FitPar - a vector of logical (size is like Par)
                            %       that contains true for a parameter to
                            %       fit and false to parameter to hold
                            %       constant.
                            % OptionalArgs - A cell array of
                            %       arbitrary number of additional
                            %       arguments.
                            % ArgsNames - Structure array of Par information returned by ArgsNames=Fun()
                            %       Contains .Name = number of the parameter in the calling for a given function
                            %                .Description = name of the parameter
                            %                .Min
                            %                .Max
                            % ArgMapping - A vector with size equal to
                            %       size(Par) containing in each entry the
                            %       index of this parameter in the global
                            %       evaluation parameters order.
                            % PreCalc - Pre calculated Y - relevant when
                            %       all(FitPar==false).

        FunOperator = '*'

        OptSeq = []             % Optimization sequence (struct array defining multi-stage fitting strategy)
                                % Each stage is a struct with fields:
                                %   .StageName - Name of the optimization stage
                                %   .FreeParams - Struct array with .Function and .Parameter fields (empty for field correction)
                                %   .SigmaClip - Enable sigma clipping (true/false)
                                %   .SigmaThresh - Sigma threshold for outlier rejection
                                %   .SigmaIter - Number of sigma clipping iterations
                                %   .Description - Description of the stage
                                % Can be set directly or passed to fitPar via 'OptimizationSequence' argument

        % Position-dependent correction (Tran2D integration)
        Tran2DObj = []           % Tran2D object for spatial corrections
        UseTran2D logical = false  % Flag to enable Tran2D evaluation
        NameTran2D = ''          % Name/type of Tran2D transformation (e.g., 'cheby1_3', 'poly_2')

        % Fit quality metrics (set by fitPar)
        RMS = NaN                % RMS of residuals from last fit
        Chi2 = NaN               % Chi-squared value from last fit
        DOF = NaN                % Degrees of freedom from last fit

        % Status log for error/warning tracking (accumulated across method calls)
        StatusLog = struct('Function', {}, 'Level', {}, 'Message', {}, 'Identifier', {}, 'Timestamp', {})
                                 % Struct array with fields:
                                 %   .Function   - Method name that generated the status
                                 %   .Level      - 'error', 'warning', or 'info'
                                 %   .Message    - Status message text
                                 %   .Identifier - Error identifier (from ME.identifier)
                                 %   .Timestamp  - Time of occurrence (datestr)
    end

    methods % Constructor
        function Obj = CompositeFun()
            % Constructor for CompositeFun
            % Input  : None
            % Output : - CompositeFun object.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model = tools.math.fun.CompositeFun();

            % Initialize properties
            Obj.FunOperator = '*';
            Obj.Funs = [];
        end
    end

    methods % Status logging utilities
        function Obj = addStatus(Obj, FunctionName, Level, Message, Identifier)
            % Add a status entry to the log
            % Input  : - Obj - CompositeFun object
            %          - FunctionName - Name of the method generating the status
            %          - Level - 'error', 'warning', or 'info'
            %          - Message - Status message text
            %          - Identifier - (optional) Error identifier. Default is ''.
            % Output : - Obj - Updated object (for chaining)
            % Author : D. Kovaleva (Jan 2026)
            % Example: Model = Model.addStatus('fitPar', 'warning', 'Convergence issue', '');

            arguments
                Obj
                FunctionName char
                Level char
                Message char
                Identifier char = ''
            end

            NewEntry.Function = FunctionName;
            NewEntry.Level = Level;
            NewEntry.Message = Message;
            NewEntry.Identifier = Identifier;
            NewEntry.Timestamp = datestr(now, 'yyyy-mm-dd HH:MM:SS');

            if isempty(Obj.StatusLog) || isempty(fieldnames(Obj.StatusLog))
                Obj.StatusLog = NewEntry;
            else
                Obj.StatusLog(end+1) = NewEntry;
            end
        end

        function Log = getStatus(Obj, Level)
            % Get status log entries, optionally filtered by level
            % Input  : - Obj - CompositeFun object
            %          - Level - (optional) Filter: 'error', 'warning', 'info', or 'all'
            %                    Default is 'all'.
            % Output : - Log - Struct array of status entries
            % Author : D. Kovaleva (Jan 2026)
            % Example: Errors = Model.getStatus('error');

            arguments
                Obj
                Level char = 'all'
            end

            if isempty(Obj.StatusLog) || isempty(fieldnames(Obj.StatusLog))
                Log = struct('Function', {}, 'Level', {}, 'Message', {}, 'Identifier', {}, 'Timestamp', {});
                return;
            end

            if strcmp(Level, 'all')
                Log = Obj.StatusLog;
            else
                Mask = strcmp({Obj.StatusLog.Level}, Level);
                Log = Obj.StatusLog(Mask);
            end
        end

        function Obj = clearStatus(Obj)
            % Clear all status log entries
            % Input  : - Obj - CompositeFun object
            % Output : - Obj - Updated object (for chaining)
            % Author : D. Kovaleva (Jan 2026)
            % Example: Model = Model.clearStatus();

            Obj.StatusLog = struct('Function', {}, 'Level', {}, 'Message', {}, 'Identifier', {}, 'Timestamp', {});
        end

        function Result = hasErrors(Obj)
            % Check if any error-level status entries exist
            % Input  : - Obj - CompositeFun object
            % Output : - Result - Logical, true if errors present
            % Author : D. Kovaleva (Jan 2026)
            % Example: if Model.hasErrors(), disp('Errors occurred'); end

            if isempty(Obj.StatusLog) || isempty(fieldnames(Obj.StatusLog))
                Result = false;
            else
                Result = any(strcmp({Obj.StatusLog.Level}, 'error'));
            end
        end

        function Result = hasWarnings(Obj)
            % Check if any warning-level status entries exist
            % Input  : - Obj - CompositeFun object
            % Output : - Result - Logical, true if warnings present
            % Author : D. Kovaleva (Jan 2026)
            % Example: if Model.hasWarnings(), disp('Warnings occurred'); end

            if isempty(Obj.StatusLog) || isempty(fieldnames(Obj.StatusLog))
                Result = false;
            else
                Result = any(strcmp({Obj.StatusLog.Level}, 'warning'));
            end
        end
    end

    methods (Static) % Factory methods
        function Obj = model(FunList, Args)
            % Build CompositeFun model from function specification list
            % Input  : - FunList - Struct array of function specifications
            %                   Each element is a struct with fields:
            %                   .name - Function name (string)
            %                   .handle - Function handle string (e.g., '@func.name')
            %                   .handletype - 'named' or 'anonymous'
            %                   .params - Parameter values (numeric array)
            %                   .paraminfo - Struct array with fields: .name, .min, .max
            %          * ...,key,val,...
            %            'MetadataValues' - Cell array with metadata name-value pairs to inject
            %                   Format: {'Name1', Value1, 'Name2', Value2, ...}
            %                   Default is {}.
            %            'OptimizationSequence' - Optimization sequence struct array defining multi-stage fitting.
            %                   Default is [] (no optimization sequence set).
            %            'UseTran2D' - Enable position-dependent corrections using Tran2D
            %                   Default is false.
            %            'Tran2DType' - Tran2D transformation type (e.g., 'cheby1_4_xt')
            %            'XPixel' - Detector X dimension [pixels]. Default is 1726.
            %            'YPixel' - Detector Y dimension [pixels]. Default is 1726.
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - Obj - CompositeFun object with all functions added
            % Author : D. Kovaleva (Dec 2025)
            % Example: FunList(1).Name = 'Func1';
            %          FunList(1).Handle = '@mypackage.myfunction';
            %          FunList(1).HandleType = 'named';
            %          FunList(1).Params = [1.0, 2.0];
            %          FunList(1).FitPar = [true, false];
            %          FunList(1).ParamInfo(1).Name = 'param1';
            %          FunList(1).ParamInfo(1).Min = 0;
            %          FunList(1).ParamInfo(1).Max = 10;
            %          FunList(1).ParamInfo(2).Name = 'param2';
            %          FunList(1).ParamInfo(2).Min = 0;
            %          FunList(1).ParamInfo(2).Max = 5;
            %          Metadata = {'param1', 1.5};
            %          Model = tools.math.fun.CompositeFun.model(FunList, ...
            %              'MetadataValues', Metadata, 'UseTran2D', true);

            arguments
                FunList struct
                Args.MetadataValues cell = {}
                Args.OptimizationSequence = []
                Args.UseTran2D logical = false
                Args.Tran2DType = 'cheby1_4_xt'
                Args.XPixel = 1726
                Args.YPixel = 1726
                Args.Verbose logical = false
            end

            % Create CompositeFun object
            Obj = tools.math.fun.CompositeFun();

            NumFunctions = length(FunList);

            if Args.Verbose
                fprintf('=== BUILDING COMPOSITEFUN MODEL ===\n');
                fprintf('Number of functions: %d\n', NumFunctions);
            end

            % Add all functions
            for I = 1:NumFunctions
                FunDef = FunList(I);

                % Extract function definition
                FunName = FunDef.Name;
                HandleStr = FunDef.Handle;
                HandleType = FunDef.HandleType;
                Params = FunDef.Params;
                ParamInfo = FunDef.ParamInfo;

                % Convert params from cell array to numeric array if needed (from YAML)
                if iscell(Params)
                    Params = cell2mat(Params);
                end

                % Convert paraminfo from cell array to struct array if needed (from YAML)
                if iscell(ParamInfo)
                    ParamInfo = [ParamInfo{:}];
                end

                % Convert to row vector
                if iscolumn(Params)
                    Params = Params';
                end

                % Extract FitPar array (or default to all false)
                NumParams = length(Params);
                if isfield(FunDef, 'FitPar')
                    FitPar = FunDef.FitPar;
                    % Ensure row vector
                    if iscolumn(FitPar)
                        FitPar = FitPar';
                    end
                else
                    FitPar = false(1, NumParams);
                end

                % Build ArgNames structure
                if NumParams == 0
                    % No parameters - create empty struct array with proper fields
                    ArgNames = struct('Name', {}, 'Description', {}, 'Min', {}, 'Max', {});
                else
                    ArgNames = struct([]);
                    for J = 1:NumParams
                        PInfo = ParamInfo(J);
                        ArgNames(J).Name = PInfo.Name;
                        ArgNames(J).Description = PInfo.Name;
                        ArgNames(J).Min = PInfo.Min;
                        ArgNames(J).Max = PInfo.Max;
                    end
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
                Obj.addFun(FunName, FunHandle, ArgNames, 'Par', Params, 'FitPar', FitPar);

                if Args.Verbose
                    fprintf('  [%d/%d] Added: %s (%d params)\n', I, NumFunctions, FunName, NumParams);
                end
            end

            % Inject metadata values if provided
            % MetadataValues is a cell array: {'Name1', Value1, 'Name2', Value2, ...}
            if ~isempty(Args.MetadataValues)
                if Args.Verbose
                    fprintf('\nInjecting metadata values:\n');
                end

                % Get all parameters structure from CompositeFun
                AllFunPar = Obj.getAllFunPar();

                % Iterate through name-value pairs
                for I = 1:2:length(Args.MetadataValues)
                    MetaName = Args.MetadataValues{I};
                    MetaValue = Args.MetadataValues{I+1};

                    % Find parameter with matching name
                    Idx = find(strcmp(AllFunPar.Name, MetaName), 1);
                    if ~isempty(Idx)
                        AllFunPar.Val(Idx) = MetaValue;
                        if Args.Verbose
                            fprintf('  Injected %s = %.3f\n', MetaName, MetaValue);
                        end
                    elseif Args.Verbose
                        fprintf('  Warning: Metadata "%s" not found in model parameters\n', MetaName);
                    end
                end

                % Apply updated parameters back to CompositeFun
                Obj.setAllFunPar(AllFunPar);
            end

            % Add Tran2D position-dependent corrections if requested
            if Args.UseTran2D
                if Args.Verbose
                    fprintf('\n=== ADDING TRAN2D POSITION CORRECTIONS ===\n');
                end

                % Calculate field center coordinates
                X_center = Args.XPixel / 2;
                Y_center = Args.YPixel / 2;

                % Create Tran2D object with specified transformation
                T2D = Tran2D(Args.Tran2DType);

                % Set normalization for coordinate transformation
                T2D.ParNX = [X_center, X_center];
                T2D.ParNY = [Y_center, Y_center];

                % Initialize polynomial coefficients to zero
                Nparams = length(T2D.FunX);
                T2D.ParX = zeros(1, Nparams);
                T2D.ParY = zeros(1, Nparams);

                % Add Tran2D to CompositeFun model without normalization
                % (normalization not needed for iterative optimization)
                Obj.addTran2D(T2D, 'Verbose', Args.Verbose);

                % Store Tran2D transformation name
                Obj.NameTran2D = Args.Tran2DType;

                if Args.Verbose
                    fprintf('Tran2D added: %s (%d parameters)\n', Args.Tran2DType, Nparams);
                    fprintf('Field center: (%.1f, %.1f)\n', X_center, Y_center);
                end
            end

            % Set optimization sequence if provided
            if ~isempty(Args.OptimizationSequence)
                Obj.OptSeq = Args.OptimizationSequence;
                if Args.Verbose
                    fprintf('Optimization sequence set: %d stages\n', length(Args.OptimizationSequence));
                end
            end

            if Args.Verbose
                fprintf('\n=== COMPOSITEFUN MODEL COMPLETE ===\n');
                fprintf('Total functions: %d, Total parameters: %d\n', ...
                        NumFunctions, Obj.numAllFunPar());
                if Args.UseTran2D
                    fprintf('Tran2D: %s, %d parameters\n', Args.Tran2DType, length(Obj.Tran2DObj.ParX));
                end
            end
        end
    end

    methods % setter/getters

        % All parameters (fitted + fixed)
        function NumParams = numAllFunPar(Obj)
            % Get total number of all global parameters (fitted + fixed)
            % Input  : - Obj - CompositeFun object.
            % Output : - NumParams - Number of all global parameters.
            % Author : D. Kovaleva (Nov 2025)

            if isempty(Obj.Funs)
                NumParams = 0;
            else
                NumParams = max([Obj.Funs.ArgMapping]);
            end
        end

        function ParamNames = namesAllFunPar(Obj)
            % Get list of all global parameter names (fitted + fixed)
            % Input  : - Obj - CompositeFun object.
            % Output : - ParamNames - Cell array of all parameter names.
            % Author : D. Kovaleva (Nov 2025)

            NumParams = Obj.numAllFunPar();
            ParamNames = cell(NumParams, 1);

            % Build names from Funs.ArgNames
            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    GlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                    if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                        ParamNames{GlobalIndex} = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                    else
                        ParamNames{GlobalIndex} = sprintf('Param_%d', GlobalIndex);
                    end
                end
            end
        end

        function ParamValues = valuesAllFunPar(Obj)
            % Get current parameter values for all parameters (fitted + fixed)
            % Input  : - Obj - CompositeFun object.
            % Output : - ParamValues - Column vector of all parameter values.
            % Author : D. Kovaleva (Nov 2025)

            AllFunPar = getAllFunPar(Obj);
            ParamValues = AllFunPar.Val;
        end

        % Fitted parameters only
        function NumFittedPars = numFittedPar(Obj)
            % Get total number of fitted parameters only
            % Input  : - Obj - CompositeFun object.
            % Output : - NumFittedPars - Number of parameters marked for fitting.
            % Author : D. Kovaleva (Nov 2025)

            if isempty(Obj.Funs)
                NumFittedPars = 0;
                return;
            end

            % Vectorized 
            NumFittedPars = sum(arrayfun(@(f) sum(f.FitPar), Obj.Funs));
        end

        function FittedNames = namesFittedPar(Obj)
            % Get list of fitted parameter names only
            % Input  : - Obj - CompositeFun object.
            % Output : - FittedNames - Cell array of fitted parameter names.
            % Author : D. Kovaleva (Nov 2025)

            NumAllFunPar = Obj.numAllFunPar();
            if NumAllFunPar == 0
                FittedNames = {};
                return;
            end

            % Get all parameter names
            AllNames = Obj.namesAllFunPar();

            % Create a logical mask for fitted parameters across all global parameters
            IsFitted = false(NumAllFunPar, 1);

            % Single loop through all functions to mark fitted parameters
            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    if Obj.Funs(Ifun).FitPar(Ipar)
                        AllIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                        if AllIndex > 0 && AllIndex <= NumAllFunPar
                            IsFitted(AllIndex) = true;
                        end
                    end
                end
            end

            % Extract fitted parameter names 
            FittedNames = AllNames(IsFitted);
        end

        % Function information
        function FunsNames = namesFuns(Obj)
            % Get names and descriptions of added functions as cell array
            % Input  : - Obj - CompositeFun object.
            % Output : - FunsNames - Cell array with columns: {Name, Description}.
            % Author : D. Kovaleva (Nov 2025)

            if isempty(Obj.Funs)
                FunsNames = {};
                return;
            end

            FunsNames = cell(numel(Obj.Funs), 2);
            for Ifun = 1:numel(Obj.Funs)
                FunsNames{Ifun, 1} = Obj.Funs(Ifun).Name;
                FunsNames{Ifun, 2} = Obj.Funs(Ifun).Desc;
            end
        end

        function FunsStruct = allFunsStruct(Obj)
            % Get complete Funs structure: Name, Desc, Handle, Par, FitPar,
            %                              OptionalArgs, ArgNames, ArgMapping, Precalc
            % Input  : - Obj - CompositeFun object.
            % Output : - FunsStruct - Complete Funs structure array.
            % Author : D. Kovaleva (Nov 2025)

            FunsStruct = Obj.Funs;
        end

        function FittedInfo = getFittedPar(Obj)
            % Get comprehensive information about fitted parameters
            % Input  : - Obj - CompositeFun object.
            % Output : - FittedInfo - Structure with TotalFitted, FittedNames, FunctionMapping.
            % Author : D. Kovaleva (Nov 2025)

            FittedInfo.TotalFitted = numFittedPar(Obj);
            FittedInfo.FittedNames = namesFittedPar(Obj);

            % Map which global parameter indices each function uses for fitted params
            FittedInfo.FunctionMapping = {};
            for Ifun = 1:numel(Obj.Funs)
                FittedIndices = Obj.Funs(Ifun).ArgMapping(Obj.Funs(Ifun).FitPar);
                FittedInfo.FunctionMapping{Ifun} = FittedIndices(:)';  % Row vector
            end
        end

        function AllFunPar = getAllFunPar(Obj)
            % Get complete parameter structure for optimization
            % Input  : - Obj - CompositeFun object.
            % Output : - AllFunPar - Structure with Name, Val, FitPar, Min, Max.
            % Author : D. Kovaleva (Nov 2025)
            % Example: AllFunPar = Model.getAllFunPar();
            %   % Modify parameter values and fit flags as needed
            %   AllFunPar.Val(2) = 350;  % Change parameter value
            %   AllFunPar.FitPar(3) = true; % Mark parameter for fitting
            %   % Update the model
            %   Model.setAllFunPar(AllFunPar);

            AllFunPar = struct();

            % Initialize arrays
            NumAllFunPar = Obj.numAllFunPar();
            AllFunPar.Name = Obj.namesAllFunPar();
            AllFunPar.Val = zeros(NumAllFunPar, 1);
            AllFunPar.FitPar = false(NumAllFunPar, 1);
            AllFunPar.Min = -inf(NumAllFunPar, 1);  % Default: no lower bound
            AllFunPar.Max = inf(NumAllFunPar, 1);   % Default: no upper bound

            % Fill values, FitPar flags, and bounds by looking at all functions
            for Ifun = 1:numel(Obj.Funs)
                % Vectorized assignment for values and FitPar
                AllIndices = Obj.Funs(Ifun).ArgMapping;
                AllFunPar.Val(AllIndices) = Obj.Funs(Ifun).Par;
                AllFunPar.FitPar(AllIndices) = Obj.Funs(Ifun).FitPar;

                % Extract bounds from ArgNames
                if ~isempty(Obj.Funs(Ifun).ArgNames)
                    for Ipar = 1:min(length(Obj.Funs(Ifun).Par), length(Obj.Funs(Ifun).ArgNames))
                        ArgInfo = Obj.Funs(Ifun).ArgNames(Ipar);
                        AllIndex = AllIndices(Ipar);
                        if isfield(ArgInfo, 'Min') && ~isempty(ArgInfo.Min)
                            AllFunPar.Min(AllIndex) = ArgInfo.Min;
                        end
                        if isfield(ArgInfo, 'Max') && ~isempty(ArgInfo.Max)
                            AllFunPar.Max(AllIndex) = ArgInfo.Max;
                        end
                    end
                end
            end

            % Add metadata
            AllFunPar.TotalParams = NumAllFunPar;
            AllFunPar.NumFitted = sum(AllFunPar.FitPar);
            AllFunPar.NumFixed = sum(~AllFunPar.FitPar);
        end

        function setAllFunPar(Obj, AllFunPar)
            % Update all parameter values, FitPar flags, and bounds if provided
            % Input  : - Obj - CompositeFun object.
            %          - AllFunPar - Structure with Val and FitPar fields.
            %                            Val: vector of parameter values
            %                            FitPar: logical vector of fit flags
            %                            Min: (optional) vector of lower bounds
            %                            Max: (optional) vector of upper bounds
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Nov 2025)
            % Example:
            %   AllFunPar = Model.getAllFunPar();
            %   AllFunPar.Val(2) = 350;  % Change parameter value
            %   AllFunPar.FitPar(3) = true; % Mark parameter for fitting
            %   AllFunPar.Min(2) = 200;     % Set lower bound
            %   AllFunPar.Max(2) = 500;     % Set upper bound
            %   Model.setAllFunPar(AllFunPar);

            NumAllFunPar = Obj.numAllFunPar();

            % Check if bounds should be updated
            UpdateBounds = isfield(AllFunPar, 'Min') && isfield(AllFunPar, 'Max');

            % Update all functions with new values, FitPar flags, and bounds
            for Ifun = 1:numel(Obj.Funs)
                % Vectorized assignment for values and FitPar
                AllIndices = Obj.Funs(Ifun).ArgMapping;
                Obj.Funs(Ifun).Par = AllFunPar.Val(AllIndices);
                Obj.Funs(Ifun).FitPar = AllFunPar.FitPar(AllIndices);

                % Update bounds in ArgNames if provided (still need loop for structure access)
                if UpdateBounds && ~isempty(Obj.Funs(Ifun).ArgNames)
                    for Ipar = 1:min(length(Obj.Funs(Ifun).Par), length(Obj.Funs(Ifun).ArgNames))
                        AllIndex = AllIndices(Ipar);
                        Obj.Funs(Ifun).ArgNames(Ipar).Min = AllFunPar.Min(AllIndex);
                        Obj.Funs(Ifun).ArgNames(Ipar).Max = AllFunPar.Max(AllIndex);
                    end
                end
            end

            % Clear any pre-calculated values since parameters may have changed
            if ~isempty(Obj.Funs)
                [Obj.Funs.PreCalc] = deal([]);  % Vectorized assignment
            end
        end

        % ================================================================
        % MCMC SUPPORT METHODS
        % ================================================================

        function [ParamVec, ParamInfo] = getFreeParamVector(Obj, Args)
            % Get free parameter values as a vector for MCMC sampling
            % Description: Extracts all parameters marked with FitPar=true
            %              into a single vector, optionally including Tran2D
            %              position parameters.
            % Input  : - Obj - CompositeFun object.
            %          * ...,key,val,...
            %            'IncludeTran2D' - Include Tran2D position parameters.
            %                   Default is false (transmission params only).
            % Output : - ParamVec - Vector of free parameter values [N_free x 1].
            %          - ParamInfo - Structure with fields:
            %                   .Names - Cell array of parameter names
            %                   .Indices - Global indices of free parameters
            %                   .Min - Lower bounds
            %                   .Max - Upper bounds
            %                   .NumTrans - Number of transmission parameters
            %                   .NumPos - Number of position parameters (0 if not included)
            % Author : D. Kovaleva (Jan 2026)
            % Example: [ParVec, Info] = Model.getFreeParamVector();
            %          [ParVec, Info] = Model.getFreeParamVector('IncludeTran2D', true);

            arguments
                Obj
                Args.IncludeTran2D logical = false
            end

            % Get transmission parameters
            AllFunPar = Obj.getAllFunPar();
            FreeIndicesTrans = find(AllFunPar.FitPar);
            ParamVecTrans = AllFunPar.Val(FreeIndicesTrans);
            NamesTrans = AllFunPar.Name(FreeIndicesTrans);
            MinTrans = AllFunPar.Min(FreeIndicesTrans);
            MaxTrans = AllFunPar.Max(FreeIndicesTrans);

            % Initialize output
            ParamVec = ParamVecTrans(:);
            ParamInfo.Names = NamesTrans;
            ParamInfo.Indices = FreeIndicesTrans(:);
            ParamInfo.Min = MinTrans(:);
            ParamInfo.Max = MaxTrans(:);
            ParamInfo.NumTrans = length(ParamVecTrans);
            ParamInfo.NumPos = 0;

            % Add Tran2D parameters if requested
            if Args.IncludeTran2D && Obj.UseTran2D && ~isempty(Obj.Tran2DObj)
                PosPar = Obj.getTran2DPar();
                FreeIndicesPos = find(PosPar.FitPar);
                ParamVecPos = PosPar.Val(FreeIndicesPos);
                NamesPos = PosPar.Name(FreeIndicesPos);

                % Position parameters have physical bounds [-10, 10] mag
                % (typical field corrections are <0.1 mag, 10 is very conservative)
                NumPos = length(FreeIndicesPos);
                MinPos = -10 * ones(NumPos, 1);
                MaxPos = 10 * ones(NumPos, 1);

                % Concatenate
                ParamVec = [ParamVec; ParamVecPos(:)];
                ParamInfo.Names = [ParamInfo.Names; NamesPos];
                ParamInfo.Indices = [ParamInfo.Indices; ...
                    FreeIndicesPos(:) + length(AllFunPar.Val)];  % Offset for Tran2D
                ParamInfo.Min = [ParamInfo.Min; MinPos];
                ParamInfo.Max = [ParamInfo.Max; MaxPos];
                ParamInfo.NumPos = NumPos;
            end
        end

        function Obj = setFreeParamVector(Obj, ParamVec, Args)
            % Set free parameter values from a vector
            % Description: Updates parameters marked with FitPar=true from
            %              a vector, optionally including Tran2D parameters.
            % Input  : - Obj - CompositeFun object.
            %          - ParamVec - Vector of free parameter values.
            %          * ...,key,val,...
            %            'IncludeTran2D' - ParamVec includes Tran2D parameters.
            %                   Default is false.
            % Output : - Obj - Updated CompositeFun object.
            % Author : D. Kovaleva (Jan 2026)
            % Example: Model = Model.setFreeParamVector(NewParams);

            arguments
                Obj
                ParamVec
                Args.IncludeTran2D logical = false
            end

            ParamVec = ParamVec(:);

            % Get current parameter structure
            AllFunPar = Obj.getAllFunPar();
            FreeIndicesTrans = find(AllFunPar.FitPar);
            NumTrans = length(FreeIndicesTrans);

            % Update transmission parameters
            if length(ParamVec) >= NumTrans
                AllFunPar.Val(FreeIndicesTrans) = ParamVec(1:NumTrans);
                Obj.setAllFunPar(AllFunPar);
            else
                error('CompositeFun:setFreeParamVector:DimensionMismatch', ...
                    'ParamVec length (%d) is less than number of free transmission parameters (%d)', ...
                    length(ParamVec), NumTrans);
            end

            % Update Tran2D parameters if included
            if Args.IncludeTran2D && Obj.UseTran2D && ~isempty(Obj.Tran2DObj)
                PosPar = Obj.getTran2DPar();
                FreeIndicesPos = find(PosPar.FitPar);
                NumPos = length(FreeIndicesPos);

                if length(ParamVec) >= NumTrans + NumPos
                    PosParams = ParamVec(NumTrans+1:NumTrans+NumPos);
                    Obj.Tran2DObj.ParX(FreeIndicesPos) = PosParams;
                else
                    error('CompositeFun:setFreeParamVector:DimensionMismatch', ...
                        'ParamVec length (%d) is less than total free parameters (%d)', ...
                        length(ParamVec), NumTrans + NumPos);
                end
            end
        end

        function Params = buildMCMCParams(Obj, Args)
            % Build mcmcstat-compatible parameter cell array
            % Description: Creates the params structure for mcmcrun from
            %              the model's free parameters.
            % Input  : - Obj - CompositeFun object.
            %          * ...,key,val,...
            %            'IncludeTran2D' - Include Tran2D parameters. Default is false.
            %            'PriorType' - Prior type for all parameters:
            %                   'uniform' - Uniform prior (min, max bounds)
            %                   'none' - No prior (improper uniform)
            %                   Default is 'uniform'.
            % Output : - Params - Cell array for mcmcrun:
            %                   {{name, init, min, max, pri_mu, pri_sig}, ...}
            % Author : D. Kovaleva (Jan 2026)
            % Reference: mcmcstat toolbox by Marko Laine
            % Example: Params = Model.buildMCMCParams();

            arguments
                Obj
                Args.IncludeTran2D logical = false
                Args.PriorType = 'uniform'
            end

            [ParamVec, ParamInfo] = Obj.getFreeParamVector('IncludeTran2D', Args.IncludeTran2D);
            NumParams = length(ParamVec);

            Params = cell(1, NumParams);
            for I = 1:NumParams
                Name = ParamInfo.Names{I};
                Init = ParamVec(I);
                MinVal = ParamInfo.Min(I);
                MaxVal = ParamInfo.Max(I);

                % Set prior parameters based on type
                switch lower(Args.PriorType)
                    case 'uniform'
                        % Uniform prior: pri_mu and pri_sig not used by mcmcstat
                        % when bounds are finite
                        PriMu = NaN;
                        PriSig = Inf;
                    case 'none'
                        % Improper uniform (no bounds enforced by prior)
                        PriMu = NaN;
                        PriSig = Inf;
                    otherwise
                        PriMu = NaN;
                        PriSig = Inf;
                end

                % mcmcstat format: {name, initial, min, max, pri_mu, pri_sig}
                Params{I} = {Name, Init, MinVal, MaxVal, PriMu, PriSig};
            end
        end

        function Model = buildMCMCModel(Obj, InputValues, ObservedValues, Args)
            % Build mcmcstat-compatible model structure
            % Description: Creates the model structure for mcmcrun with
            %              ssfun wrapping the costFun method.
            % Input  : - Obj - CompositeFun object.
            %          - InputValues - Wavelength grid for transmission.
            %          - ObservedValues - Observed flux values.
            %          * ...,key,val,...
            %            'CostArgs' - Cell array of additional costFun arguments.
            %                   Default is {}.
            %            'X' - X coordinates for position correction. Default is [].
            %            'Y' - Y coordinates for position correction. Default is [].
            %            'IncludeTran2D' - Include Tran2D in parameter vector.
            %                   Default is false.
            % Output : - Model - Structure for mcmcrun with fields:
            %                   .ssfun - Sum-of-squares function handle
            %                   .N - Number of observations
            %                   (only mcmcstat-valid fields)
            % Author : D. Kovaleva (Jan 2026)
            % Example: Model = Obj.buildMCMCModel(Lambda, ObsFlux, 'CostArgs', CostArgs);

            arguments
                Obj
                InputValues
                ObservedValues
                Args.CostArgs cell = {}
                Args.X = []
                Args.Y = []
                Args.IncludeTran2D logical = false
            end

            % Capture data in closure (not in Model struct - mcmcstat rejects unknown fields)
            CapturedInputValues = InputValues;
            CapturedObservedValues = ObservedValues;
            CapturedCostArgs = Args.CostArgs;
            CapturedX = Args.X;
            CapturedY = Args.Y;
            CapturedIncludeTran2D = Args.IncludeTran2D;
            CapturedObj = Obj;  % Handle class - reference is captured

            % Build sum-of-squares function using closure
            % mcmcstat calls: ss = ssfun(par, data)
            % par = parameter vector, data = passed but we use closure instead
            Model.ssfun = @mcmcSSFun;
            Model.N = length(ObservedValues);

            % Nested function captures all data via closure
            function SS = mcmcSSFun(ParVec, ~)
                % Update model parameters (handle class - modifies in place)
                CapturedObj.setFreeParamVector(ParVec, 'IncludeTran2D', CapturedIncludeTran2D);

                % Call costFun to get residuals
                if ~isempty(CapturedX)
                    [~, ~, ~, Residuals] = CapturedObj.costFun(CapturedInputValues, CapturedObservedValues, ...
                        CapturedCostArgs{:}, 'X', CapturedX, 'Y', CapturedY);
                else
                    [~, ~, ~, Residuals] = CapturedObj.costFun(CapturedInputValues, CapturedObservedValues, ...
                        CapturedCostArgs{:});
                end

                % Sum of squared residuals (magnitude residuals)
                SS = sum(Residuals.^2);
            end
        end

    end

    methods % low level utilities

        function checkOverlappingParamConsistency(Obj)
            % Check for parameter value inconsistencies across functions and NaN fixed parameters
            % Input  : - Obj - CompositeFun object.
            % Output : - None (throws error if inconsistencies found, shows reminder for NaN fixed params).
            % Author : D. Kovaleva (Nov 2025)
            % Example: Model.checkOverlappingParamConsistency();  % Optional validation before calculations

            if isempty(Obj.Funs)
                return;  % Nothing to check
            end

            % Build mapping of global parameter index to {function, local parameter index, value}
            GlobalParamMap = containers.Map('KeyType', 'int32', 'ValueType', 'any');

            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    GlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                    ParamValue = Obj.Funs(Ifun).Par(Ipar);
                    if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                        ParamName = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                    else
                        ParamName = sprintf('Param_%d', GlobalIndex);
                    end

                    if isKey(GlobalParamMap, GlobalIndex)
                        % Parameter already seen - check for consistency
                        ExistingEntries = GlobalParamMap(GlobalIndex);

                        % Check if any existing value differs from current value
                        for Ientry = 1:length(ExistingEntries)
                            ExistingValue = ExistingEntries{Ientry}{3};
                            % Check for NaN inconsistencies or numerical differences
                            if isnan(ParamValue) ~= isnan(ExistingValue) || ...
                               (~isnan(ParamValue) && ~isnan(ExistingValue) && abs(ParamValue - ExistingValue) > 1e-12)
                                % Found inconsistency - display detailed information using fprintf
                                fprintf('\n=== PARAMETER INCONSISTENCY DETECTED ===\n');
                                fprintf('Global parameter %d (%s) has different values:\n', GlobalIndex, ParamName);

                                % Display existing conflicting values
                                for Jentry = 1:length(ExistingEntries)
                                    ExistFun = ExistingEntries{Jentry}{1};
                                    ExistVal = ExistingEntries{Jentry}{3};
                                    if isnan(ExistVal)
                                        fprintf('  Function %d (%s): NaN\n', ExistFun, Obj.Funs(ExistFun).Desc);
                                    else
                                        fprintf('  Function %d (%s): %.6g\n', ExistFun, Obj.Funs(ExistFun).Desc, ExistVal);
                                    end
                                end

                                % Display current conflicting value
                                if isnan(ParamValue)
                                    fprintf('  Function %d (%s): NaN\n', Ifun, Obj.Funs(Ifun).Desc);
                                else
                                    fprintf('  Function %d (%s): %.6g\n', Ifun, Obj.Funs(Ifun).Desc, ParamValue);
                                end

                                % Display recipe to fix - prefer non-NaN value
                                SuggestedValue = ExistingValue;
                                if isnan(ExistingValue) && ~isnan(ParamValue)
                                    SuggestedValue = ParamValue;
                                end

                                fprintf('\nRecipe to fix:\n');
                                fprintf('  AllFunPar = Model.getAllFunPar();\n');
                                if isnan(SuggestedValue)
                                    fprintf('  AllFunPar.Val(%d) = 45;  %% Set meaningful value (example)\n', GlobalIndex);
                                else
                                    fprintf('  AllFunPar.Val(%d) = %.6g;  %% Set consistent value\n', GlobalIndex, SuggestedValue);
                                end
                                fprintf('  Model.setAllFunPar(AllFunPar);\n');
                                fprintf('==========================================\n\n');

                                % Throw a concise error
                                error('CompositeFun:checkOverlappingParamConsistency:Inconsistency', ...
                                      'Parameter inconsistency detected for "%s". See details above.', ParamName);
                            end
                        end

                        % Add current entry to existing list
                        NewEntry = {Ifun, Ipar, ParamValue};
                        ExistingEntries{end+1} = NewEntry;  %#ok<AGROW>
                        GlobalParamMap(GlobalIndex) = ExistingEntries;
                    else
                        % First time seeing this global parameter
                        GlobalParamMap(GlobalIndex) = {{Ifun, Ipar, ParamValue}};
                    end
                end
            end

            % Check for NaN fixed parameters (informational warning, not error)
            % Pre-allocate cell array to avoid size-changing in loop
            TotalParams = sum(arrayfun(@(f) length(f.Par), Obj.Funs));
            NaNFixedParamsCell = cell(1, TotalParams);
            NaNCount = 0;
            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    if ~Obj.Funs(Ifun).FitPar(Ipar) && isnan(Obj.Funs(Ifun).Par(Ipar))
                        GlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                        if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                            ParamName = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                        else
                            ParamName = sprintf('Param_%d', GlobalIndex);
                        end
                        NaNCount = NaNCount + 1;
                        NaNFixedParamsCell{NaNCount} = {GlobalIndex, Ifun, Ipar, ParamName, Obj.Funs(Ifun).Desc};
                    end
                end
            end
            % Trim to actual size
            NaNFixedParamsCell = NaNFixedParamsCell(1:NaNCount);

            % Convert to matrix if there are any NaN fixed params
            if ~isempty(NaNFixedParamsCell)
                NaNFixedParams = zeros(length(NaNFixedParamsCell), 3);
                NaNFixedParamsNames = cell(length(NaNFixedParamsCell), 2);
                for i = 1:length(NaNFixedParamsCell)
                    NaNFixedParams(i,:) = [NaNFixedParamsCell{i}{1:3}];
                    NaNFixedParamsNames{i,1} = NaNFixedParamsCell{i}{4};
                    NaNFixedParamsNames{i,2} = NaNFixedParamsCell{i}{5};
                end
            else
                NaNFixedParams = [];
                NaNFixedParamsNames = {};
            end

            if ~isempty(NaNFixedParams)
                fprintf('\n=== REMINDER: NaN FIXED PARAMETERS DETECTED ===\n');
                fprintf('The following fixed parameters (FitPar=false) have NaN values:\n');
                for i = 1:size(NaNFixedParams, 1)
                    GlobalIndex = NaNFixedParams(i,1);
                    FunIndex = NaNFixedParams(i,2);
                    ParamName = NaNFixedParamsNames{i,1};
                    FunDesc = NaNFixedParamsNames{i,2};
                    fprintf('  Global parameter %d (%s) in function %d (%s)\n', GlobalIndex, ParamName, FunIndex, FunDesc);
                end

                fprintf('\nFixed parameters with NaN values cannot be used in calculations.\n');
                fprintf('Recipe to set values:\n');
                fprintf('  AllFunPar = Model.getAllFunPar();\n');

                % Show unique global indices to avoid duplicate settings
                UniqueGlobalIndices = unique(NaNFixedParams(:,1));
                for i = 1:length(UniqueGlobalIndices)
                    GlobalIndex = UniqueGlobalIndices(i);
                    % Find corresponding parameter name
                    ParamRow = find(NaNFixedParams(:,1) == GlobalIndex, 1);
                    ParamName = NaNFixedParamsNames{ParamRow,1};
                    fprintf('  AllFunPar.Val(%d) = 45;  %% Set meaningful value for %s (example)\n', GlobalIndex, ParamName);
                end

                fprintf('  Model.setAllFunPar(AllFunPar);\n');
                fprintf('===============================================\n\n');
            end

            % If we reach here, no inconsistencies found
        end

        function ArgNames = extractArgFuns(~, FunctionHandle)
            % Auto-extract ArgNames structure from function handle
            % Input  : - ~ - CompositeFun object (not used, static-like method).
            %          - FunctionHandle - Function handle to transmission function that supports GetArgNames flag.
            % Output : - ArgNames - Structure array with Name, Description, Min, Max fields.
            % Author : D. Kovaleva (Nov 2025)
            % Example: ArgNames = Model.extractArgFuns(@astro.transmission.ozoneTransmission);

            try
                % Call function with GetArgNames flag to get parameter info
                ArgNames = FunctionHandle('GetArgNames', true);
            catch ME
                error('CompositeFun:extractArgFuns:Failed', ...
                      'Cannot extract ArgNames from function handle: %s', ME.message);
            end
        end

    end

    methods % utilities

        function addFun(Obj, Desc, Handle, ArgNames, varargin)
            % Add a function component to Funs
            % Input  : - Obj - CompositeFun object.
            %          - Desc - Description string (obligatory).
            %          - Handle - Function handle (obligatory).
            %          - ArgNames - Argument names structure array (optional) for ParamMatrix elements.
            %                     For each ParamMatrix element: .Name - consecutive number in ParamMatrix,
            %                                                 .Description - parameter name,
            %                                                 .Min - lower bound,
            %                                                 .Max - upper bound.
            %                     If not submitted, ArgNames will be uploaded from the function.
            %          * ...,key,val,...
            %            'Par' - Parameter values for ParamMatrix (default from Handle).
            %                   Vector corresponding to one row of transmission function's ParamMatrix.
            %            'FitPar' - Logical vector for fitting ParamMatrix elements (default all false).
            %            'OptionalArgs' - Cell array for transmission function's optional arguments.
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Nov 2025)
            % Example: Model.addFun('Ozone', @astro.transmission.ozoneTransmission, [], 'Par', [45, 300]);
            %          ArgNames = Model.extractArgFuns(@astro.transmission.ozoneTransmission); % Explicit generation

            % Check obligatory inputs
            if nargin < 3
                error('CompositeFun:addFun:MissingInputs', 'Desc and Handle are obligatory');
            end

            % Auto-extract ArgNames if not provided
            if nargin < 4 || isempty(ArgNames)
                ArgNames = extractArgFuns(Obj, Handle);
            end

            % Parse optional key-value pairs
            Par = [];
            FitPar = logical([]);
            OptionalArgs = {};

            for I = 1:2:length(varargin)
                if I+1 <= length(varargin)
                    switch varargin{I}
                        case 'Par'
                            Par = varargin{I+1};
                        case 'FitPar'
                            FitPar = varargin{I+1};
                        case 'OptionalArgs'
                            OptionalArgs = varargin{I+1};
                    end
                end
            end

            % Handle empty Par - parameters can be provided later
            if isempty(Par)
                % Get number of parameters from ArgNames to set up structure
                NumParams = length(ArgNames);
                Par = NaN(1, NumParams);    % Initialize with NaN as placeholder
            end

            % Set FitPar default if not provided
            if isempty(FitPar)
                FitPar = false(size(Par));  % Already returns logical
            end

            % Create an entry for new function 
            NewFun = struct();

            % Name is the function number in order of adding
            if isempty(Obj.Funs)
                NewFun.Name = 1;
            else
                NewFun.Name = numel(Obj.Funs) + 1;
            end

            NewFun.Desc = Desc;
            NewFun.Handle = Handle;
            NewFun.Par = Par(:)';
            NewFun.FitPar = FitPar(:);  
            NewFun.OptionalArgs = OptionalArgs;
            NewFun.ArgNames = ArgNames;  % Store ArgNames structure directly
            NewFun.ArgMapping = [];  % Will be filled by argMapping
            NewFun.PreCalc = [];  % Placeholder - will come from function cache if available

            % Add to Funs array 
            if isempty(Obj.Funs)
                Obj.Funs = NewFun;
            else
                Obj.Funs(end+1) = NewFun;
            end

            % Update ArgMapping for the new function
            argMapping(Obj);
        end

        function argMapping(Obj)
            % Map parameters of the last added function to global parameter indices
            % Input  : - Obj - CompositeFun object.
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Nov 2025)

            if isempty(Obj.Funs)
                return;
            end

            % Process the last added function
            CurrentFun = Obj.Funs(end);
            NumParams = length(CurrentFun.Par);

            % Build current global parameter name list from existing functions
            % Find maximum global index first to pre-allocate
            MaxGlobalIndex = 0;
            for Ifun = 1:(numel(Obj.Funs)-1)
                if ~isempty(Obj.Funs(Ifun).ArgMapping)
                    MaxGlobalIndex = max(MaxGlobalIndex, max(Obj.Funs(Ifun).ArgMapping));
                end
            end

            % Pre-allocate GlobalParamNames
            GlobalParamNames = cell(1, MaxGlobalIndex);
            NextGlobalIndex = MaxGlobalIndex + 1;

            % First, process all existing functions (except the last one) to build current state
            for Ifun = 1:(numel(Obj.Funs)-1)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    ExistingGlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                    % Build parameter name from ArgNames
                    if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                        ParamName = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                        GlobalParamNames{ExistingGlobalIndex} = ParamName;
                    end
                end
            end

            % Now process the last function's parameters
            for Ipar = 1:NumParams
                % Get parameter name (stored in Description field)
                ParamName = CurrentFun.ArgNames(Ipar).Description;

                % Check if parameter exists in global list
                ExistingIndex = find(strcmp(GlobalParamNames, ParamName), 1);

                if isempty(ExistingIndex)
                    % New parameter, assign next available index
                    GlobalIndex = NextGlobalIndex;
                    NextGlobalIndex = NextGlobalIndex + 1;
                else
                    % Parameter exists, use existing index
                    GlobalIndex = ExistingIndex;
                end

                % Store mapping for this function
                Obj.Funs(end).ArgMapping(Ipar) = GlobalIndex;
            end
        end

        function preCalc(Obj, X)
            % Pre-calculate function values for functions with all fixed parameters
            % Input  : - Obj - CompositeFun object.
            %          - X - Input values (e.g., wavelengths).
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Nov 2025)

            if nargin < 2 || isempty(X)
                return;
            end

            Nfun = numel(Obj.Funs);
            for Ifun = 1:Nfun
                if all(~Obj.Funs(Ifun).FitPar)
                    % Check for NaN parameters before pre-calculation
                    if any(isnan(Obj.Funs(Ifun).Par))
                        error('CompositeFun:preCalc:NaNParameters', ...
                              'Cannot pre-calculate function %d (%s): contains NaN parameter values. Use setAllFunPar() to set parameter values first.', ...
                              Ifun, Obj.Funs(Ifun).Desc);
                    end

                    % All parameters are fixed - calculate once and store result
                    try
                        % Calculate result once
                        Obj.Funs(Ifun).PreCalc = Obj.Funs(Ifun).Handle(X, Obj.Funs(Ifun).Par, Obj.Funs(Ifun).OptionalArgs{:});

                        % Future calls to evaluate() will use this PreCalc value via 'Return' argument
                        % instead of recalculating

                    catch ME
                        % Configuration error check
                        error('CompositeFun:preCalc:CalculationFailed', ...
                              'Pre-calculation failed for function %d (%s): %s', ...
                              Ifun, Obj.Funs(Ifun).Desc, ME.message);
                    end
                else
                    % Some parameters will be fitted - clear any existing PreCalc
                    Obj.Funs(Ifun).PreCalc = [];
                end
            end
        end
    end

    methods % evaluation
        function Y=evaluateAllFunParInput(Obj, X, AllFunPar)
            % Evaluate the composite function
            % Input  : - Obj - CompositeFun object.
            %          - X - Input values (e.g., wavelengths), a vector.
            %          - AllFunPars - Full parameter matrix (optional).
            %                       If vector: single parameter set.
            %                       If matrix: each row is a parameter set.
            %                       If not provided, uses stored parameter values.
            % Output : - Y - Output values matrix (wavelengths × parameter_sets).
            % Author : D. Kovaleva (Nov 2025)

            if nargin < 3
                AllFunPar = [];
            end

            % Determine number of parameter sets
            if ~isempty(AllFunPar)
                NumParamSets = size(AllFunPar, 1);
            else
                NumParamSets = 1;
            end

            Nfun = numel(Obj.Funs);

            switch Obj.FunOperator                       % there may be more FunOperator options
                case '*'

                    Y = ones(length(X), NumParamSets);
                    for Ifun=1:1:Nfun
                        % Check if all parameters for this function are fixed and pre-calculated
                        if ~isempty(Obj.Funs(Ifun).PreCalc) && all(~Obj.Funs(Ifun).FitPar)
                            % Use pre-calculated values - replicate for all parameter sets
                            Y = Y .* repmat(Obj.Funs(Ifun).PreCalc(:), 1, NumParamSets);
                        else
                            % Build parameter matrix for this function
                            if ~isempty(AllFunPar)
                                % Extract parameters for this function from AllFunPar matrix
                                ParMatrix = zeros(NumParamSets, length(Obj.Funs(Ifun).Par));
                                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                                    AllIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                                    ParMatrix(:, Ipar) = AllFunPar(:, AllIndex);
                                end
                            else
                                % Use stored parameter values - single parameter set
                                ParMatrix = Obj.Funs(Ifun).Par(:)';
                            end

                            % Check for NaN parameters
                            if any(isnan(ParMatrix(:)))
                                Obj.addStatus('evaluateAllFunParInput', 'error', ...
                                    'Cannot evaluate: some parameters contain NaN values. Use setAllFunParsStruct() to set parameter values first.', ...
                                    'CompositeFun:evaluate:NaNParameters');
                                Y = nan(length(X), NumParamSets);
                                return;
                            end

                            FunResult = Obj.Funs(Ifun).Handle(X, ParMatrix, Obj.Funs(Ifun).OptionalArgs{:});
                            Y = Y .* FunResult;
                        end
                    end
                otherwise
                    error('Composite function Operator: %s - not supported yet',Obj.FunOperator);
            end

        end

        function Y=evaluate(Obj, X, FittedPars)
            % Evaluate the composite function using only fitted parameters
            % Input  : - Obj - CompositeFun object.
            %          - X - Input values (e.g., wavelengths), a vector.
            %          - FittedPars - Fitted parameter matrix only.
            %                          If vector: single parameter set.
            %                          If matrix: each row is a parameter set.
            %                          Fixed parameters are taken from stored Obj.Funs.Par values.
            % Output : - Y - Output values matrix (wavelengths × parameter_sets).
            % Author : D. Kovaleva (Nov 2025)

            % Validate FittedPars size
            NumFittedPars = Obj.numFittedPar();
            if size(FittedPars, 2) ~= NumFittedPars
                error('CompositeFun:evaluate:FittedParsSizeMismatch', ...
                      'FittedPars has %d columns but %d fitted parameters expected', ...
                      size(FittedPars, 2), NumFittedPars);
            end

            NumParamSets = size(FittedPars, 1);
            Nfun = numel(Obj.Funs);

            % Track fitted parameter usage across all functions
            FittedParIndex = 0;

            switch Obj.FunOperator
                case '*'
                    Y = ones(length(X), NumParamSets);
                    for Ifun=1:1:Nfun
                        % Check if all parameters for this function are fixed and pre-calculated
                        if ~isempty(Obj.Funs(Ifun).PreCalc) && all(~Obj.Funs(Ifun).FitPar)
                            % Use pre-calculated values - replicate for all parameter sets
                            Y = Y .* repmat(Obj.Funs(Ifun).PreCalc(:), 1, NumParamSets);
                        else
                            % Build parameter matrix for this function
                            NumFunParams = length(Obj.Funs(Ifun).Par);
                            ParMatrix = zeros(NumParamSets, NumFunParams);

                            for Ipar = 1:NumFunParams
                                if Obj.Funs(Ifun).FitPar(Ipar)
                                    % This is a fitted parameter - take from FittedPars input
                                    FittedParIndex = FittedParIndex + 1;
                                    ParMatrix(:, Ipar) = FittedPars(:, FittedParIndex);
                                else
                                    % This is a fixed parameter - take from stored Obj.Funs.Par
                                    FixedValue = Obj.Funs(Ifun).Par(Ipar);
                                    if isnan(FixedValue)
                                        error('CompositeFun:evaluate:NaNFixedParameter', ...
                                              'Fixed parameter %d in function %d (%s) has NaN value. Use setAllFunPar() to set fixed parameters first.', ...
                                              Ipar, Ifun, Obj.Funs(Ifun).Name);
                                    end
                                    ParMatrix(:, Ipar) = FixedValue;
                                end
                            end

                            FunResult = Obj.Funs(Ifun).Handle(X, ParMatrix, Obj.Funs(Ifun).OptionalArgs{:});
                            Y = Y .* FunResult;
                        end
                    end
                otherwise
                    error('Composite function Operator: %s - not supported yet',Obj.FunOperator);
            end
        end

    end

    % ===================================================================
    % Tran2D Integration Methods
    % ===================================================================

    methods
        function addTran2D(Obj, Tran2DObj, Args)
            % Add position-dependent correction using Tran2D object
            % Input  : - Tran2DObj: Tran2D object for spatial corrections
            %          * ...,key,val,...
            %            'X_ref' - Reference X coordinate for normalization.
            %                   If provided with Y_ref, normalizes polynomial immediately.
            %                   Default is [] (no normalization).
            %            'Y_ref' - Reference Y coordinate for normalization.
            %                   If provided with X_ref, normalizes polynomial immediately.
            %                   Default is [] (no normalization).
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : None (modifies Obj in place)
            % Author : D. Kovaleva (Nov 2025)
            % Example: Model.addTran2D(Tran2DObj, 'Verbose', true);
            %          Model.addTran2D(Tran2DObj, 'X_ref', 1000, 'Y_ref', 1000);

            arguments
                Obj
                Tran2DObj
                Args.X_ref = []
                Args.Y_ref = []
                Args.Verbose logical = false
            end

            % Validate input is a Tran2D object
            if ~isa(Tran2DObj, 'Tran2D')
                error('CompositeFun:addTran2D:InvalidInput', ...
                      'Input must be a Tran2D object');
            end

            % Store the Tran2D object and enable flag
            Obj.Tran2DObj = Tran2DObj;
            Obj.UseTran2D = true;

            if Args.Verbose
                fprintf('Tran2D position correction added to CompositeFun\n');
                fprintf('  Number of ParX parameters: %d\n', length(Tran2DObj.ParX));
                fprintf('  Number of ParY parameters: %d\n', length(Tran2DObj.ParY));
            end

            % Optionally normalize at reference position
            if ~isempty(Args.X_ref) && ~isempty(Args.Y_ref)
                Obj.normalizePositionPolynomial(Args.X_ref, Args.Y_ref, 'Verbose', Args.Verbose);
            elseif ~isempty(Args.X_ref) || ~isempty(Args.Y_ref)
                warning('CompositeFun:addTran2D:IncompleteReference', ...
                        'Both X_ref and Y_ref must be provided for normalization. Skipping normalization.');
            end
        end

        function resetTran2DParams(Obj, Args)
            % Reset Tran2D position polynomial parameters to zeros
            % Input  : * ...,key,val,...
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : None (modifies Obj in place)
            % Author : D. Kovaleva (Nov 2025)
            % Example: Model.resetTran2DParams('Verbose', true);

            arguments
                Obj
                Args.Verbose logical = false
            end

            if ~Obj.UseTran2D || isempty(Obj.Tran2DObj)
                warning('CompositeFun:resetTran2DParams:NoTran2D', ...
                        'Tran2D not initialized. Nothing to reset.');
                return;
            end

            % Reset ParX and ParY to zeros
            Obj.Tran2DObj.ParX = zeros(size(Obj.Tran2DObj.ParX));
            Obj.Tran2DObj.ParY = zeros(size(Obj.Tran2DObj.ParY));

            if Args.Verbose
                fprintf('Tran2D parameters reset to zeros\n');
                fprintf('  ParX: %d parameters\n', length(Obj.Tran2DObj.ParX));
                fprintf('  ParY: %d parameters\n', length(Obj.Tran2DObj.ParY));
            end
        end

        function Transmission = evaluateWithPosition(Obj, Lambda, X, Y, Args)
            % Evaluate transmission including position-dependent corrections
            % Input  : - Lambda: Wavelength grid [N_lambda x 1]
            %          - X: X coordinates [N_sources x 1]
            %          - Y: Y coordinates [N_sources x 1]
            %          * ...,key,val,...
            %            'TransParams' - Wavelength-dependent transmission parameters.
            %                   If empty, uses current values from Obj. Default is [].
            %            'PosParams' - Position-dependent polynomial parameters (ParX).
            %                   If empty, uses current values from Tran2DObj. Default is [].
            %            'ValInp' - Boolean flag for validation of inputs. Default is true.
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - Transmission: [N_sources x N_lambda] matrix
            %                   Transmission(i,j) = transmission for source i at wavelength j
            % Author : D. Kovaleva (Nov 2025)
            % Example: Trans = Model.evaluateWithPosition(Lambda, X, Y);

            arguments
                Obj
                Lambda
                X
                Y
                Args.TransParams = []
                Args.PosParams = []
                Args.ValInp logical = true
                Args.Verbose logical = false
            end

            % Ensure column vectors
            Lambda = Lambda(:);
            X = X(:);
            Y = Y(:);

            N_lambda = length(Lambda);
            N_sources = length(X);

            if Args.ValInp
                if length(Y) ~= N_sources
                    error('CompositeFun:evaluateWithPosition:DimensionMismatch', ...
                          'X and Y must have same length (X: %d, Y: %d)', N_sources, length(Y));
                end
            end

            % Step 1: Evaluate base transmission (wavelength-dependent)
            if isempty(Args.TransParams)
                Transmission_base = Obj.evaluateAllFunParInput(Lambda);
            else
                Transmission_base = Obj.evaluateAllFunParInput(Lambda, Args.TransParams);
            end
            Transmission_base = Transmission_base(:);  % Ensure column vector

            % Step 2: Check if Tran2D is enabled
            if ~Obj.UseTran2D || isempty(Obj.Tran2DObj)
                % No position correction - replicate base transmission for all sources
                Transmission = repmat(Transmission_base', N_sources, 1);

                if Args.Verbose
                    fprintf('Position correction disabled - returning replicated base transmission\n');
                end
                return;
            end

            % Step 3: Update Tran2D parameters if provided
            if ~isempty(Args.PosParams)
                Obj.Tran2DObj.ParX = Args.PosParams(:)';  % Ensure row vector
            end

            % Step 4: Calculate position-dependent correction in magnitude space
            % Use Tran2D's forward() method - it returns [Xi, Yi] where Xi is the magnitude correction
            Coo = [X, Y];
            [FieldCorrectionMag, ~] = Obj.Tran2DObj.forward(X, Y, false);
            FieldCorrectionMag = FieldCorrectionMag(:);  % [N_sources x 1]

            % Check for invalid values from Tran2D
            if any(isnan(FieldCorrectionMag)) || any(isinf(FieldCorrectionMag))
                error('CompositeFun:evaluateWithPosition:InvalidTran2D', ...
                      'Tran2D.forward() produced NaN or Inf values.\n  ParX range: %.4e - %.4e\n  Coordinates: X=[%.2f - %.2f], Y=[%.2f - %.2f]', ...
                      min(Obj.Tran2DObj.ParX), max(Obj.Tran2DObj.ParX), min(X), max(X), min(Y), max(Y));
            end

            % Step 5: Convert magnitude correction to transmission space
            % Polynomial was fitted to residuals: MagResid = Observed - Predicted (positive when model too bright)
            % At each position, FieldCorrectionMag = Hx*ParX where ParX was fitted to MagResid directly
            % Since we multiply in transmission space,
            % we use: T_corrected = T_base × 10^(-0.4 × FieldCorrectionMag)
         
            FieldCorrectionTransmission = 10.^(-0.4 * FieldCorrectionMag);  % [N_sources x 1]

            % Step 6: Build 2D transmission matrix [N_sources x N_lambda]
            Transmission_base_replicated = repmat(Transmission_base', N_sources, 1);
            FieldCorrectionTransmission_replicated = repmat(FieldCorrectionTransmission, 1, N_lambda);
            Transmission = Transmission_base_replicated .* FieldCorrectionTransmission_replicated;

            % Check for unphysical transmission values
            if any(Transmission(:) > 1)
                warning('CompositeFun:evaluateWithPosition:UnphysicalTransmission', ...
                        ['Transmission exceeds 1.0 (max=%.4f). \n' ...
                         '  Base transmission range: %.4f - %.4f\n' ...
                         '  Field correction mag range: %.4f - %.4f mag\n' ...
                         '  Field correction transmission range: %.4f - %.4f\n' ...
                         '  Final transmission range: %.4f - %.4f'], ...
                        max(Transmission(:)), min(Transmission_base), max(Transmission_base), ...
                        min(FieldCorrectionMag), max(FieldCorrectionMag), ...
                        min(FieldCorrectionTransmission), max(FieldCorrectionTransmission), ...
                        min(Transmission(:)), max(Transmission(:)));
            end

            if Args.Verbose
                fprintf('Position-dependent transmission calculated\n');
                fprintf('  Sources: %d, Wavelengths: %d\n', N_sources, N_lambda);
                fprintf('  Base transmission range: %.4f - %.4f\n', ...
                        min(Transmission_base), max(Transmission_base));
                fprintf('  Field correction range: %.4f - %.4f mag\n', ...
                        min(FieldCorrectionMag), max(FieldCorrectionMag));
                fprintf('  Field correction transmission range: %.4f - %.4f\n', ...
                        min(FieldCorrectionTransmission), max(FieldCorrectionTransmission));
                fprintf('  Total transmission range: %.4f - %.4f\n', ...
                        min(Transmission(:)), max(Transmission(:)));
            end
        end

        function normalizePositionPolynomial(Obj, X_ref, Y_ref, Args)
            % Normalize position polynomial so P(X_ref, Y_ref) = 1
            % This prevents degeneracy with wavelength transmission normalization
            %
            % NOTE: Normalization is OPTIONAL and not needed for iterative optimization
            % where position corrections are fitted separately from wavelength transmission.
            % Use only if you need to enforce a specific reference point constraint.
            %
            % Input  : - X_ref: Reference X coordinate (typically field center)
            %          - Y_ref: Reference Y coordinate (typically field center)
            %          * ...,key,val,...
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : None (modifies Obj.Tran2DObj.ParX in place)
            % Author : D. Kovaleva (Nov 2025)
            % Example: Model.normalizePositionPolynomial(863, 863);  % For 1726-pixel detector

            arguments
                Obj
                X_ref
                Y_ref
                Args.Verbose logical = false
            end

            if ~Obj.UseTran2D || isempty(Obj.Tran2DObj)
                error('CompositeFun:normalizePositionPolynomial:NoTran2D', ...
                      'Tran2D not initialized. Call addTran2D() first.');
            end

            % Evaluate correction at reference position
            Coo_ref = [X_ref, Y_ref];
            [P_ref, ~] = Obj.Tran2DObj.forward(Coo_ref, false);

            if Args.Verbose
                fprintf('Normalizing position polynomial\n');
                fprintf('  Reference position: (%.1f, %.1f)\n', X_ref, Y_ref);
                fprintf('  Correction at reference before normalization: %.6f mag\n', P_ref);
            end

            % Normalize: subtract offset so P(X_ref, Y_ref) = 0 in magnitude space
            % This means transmission correction = 10^(-0.4 * 0) = 1 at reference
            % We do this by adjusting all parameters to shift the polynomial
            % Since P = sum(c_i * f_i(x,y)), we need to subtract P_ref from the result
            % This is typically done by adjusting the constant term

            % For Tran2D, ParX(1) is usually the constant term
            % Adjust it to make P(X_ref, Y_ref) = 0
            if ~isempty(Obj.Tran2DObj.ParX) && length(Obj.Tran2DObj.ParX) >= 1
                Obj.Tran2DObj.ParX(1) = Obj.Tran2DObj.ParX(1) - P_ref;

                % Verify normalization
                [P_ref_new, ~] = Obj.Tran2DObj.forward(Coo_ref, false);

                if Args.Verbose
                    fprintf('  Correction at reference after normalization: %.6f mag\n', P_ref_new);
                    fprintf('  Transmission factor at reference: %.6f\n', 10^(-0.4 * P_ref_new));
                end
            else
                warning('CompositeFun:normalizePositionPolynomial:NoConstantTerm', ...
                        'Cannot normalize - ParX is empty or has no constant term');
            end
        end

        function Par = getTran2DPar(Obj)
            % Get Tran2D parameters in standard format
            % Compatible with getAllFunPar format
            % Output : - Par: Structure with fields:
            %                   .Name - Parameter names
            %                   .Val - Parameter values
            %                   .FitPar - Fitted parameter flags
            % Author : D. Kovaleva (Nov 2025)
            % Example: PosPar = Model.getTran2DPar();

            if ~Obj.UseTran2D || isempty(Obj.Tran2DObj)
                % Return empty structure
                Par.Name = {};
                Par.Val = [];
                Par.FitPar = [];
                return;
            end

            N_params = length(Obj.Tran2DObj.ParX);

            % Build parameter names (column cell array for consistency)
            Par.Name = cell(N_params, 1);
            for I = 1:N_params
                Par.Name{I} = sprintf('PosCoeff_%d', I);
            end

            % Get current parameter values 
            Par.Val = Obj.Tran2DObj.ParX(:);

            % By default, all position parameters are fittable (column vector)
            Par.FitPar = true(N_params, 1);
        end

        function setTran2DParams(Obj, ParValues, Args)
            % Set Tran2D parameters from vector
            % Input  : - ParValues: Vector of position polynomial coefficients
            %          * ...,key,val,...
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : None (modifies Obj.Tran2DObj.ParX in place)
            % Author : D. Kovaleva (Nov 2025)
            % Example: Model.setTran2DParams([0.1, 0.2, 0.3]);

            arguments
                Obj
                ParValues
                Args.Verbose logical = false
            end

            if ~Obj.UseTran2D || isempty(Obj.Tran2DObj)
                error('CompositeFun:setTran2DParams:NoTran2D', ...
                      'Tran2D not initialized. Call addTran2D() first.');
            end

            % Ensure row vector
            ParValues = ParValues(:)';

            % Validate size
            N_expected = length(Obj.Tran2DObj.ParX);
            if length(ParValues) ~= N_expected
                error('CompositeFun:setTran2DParams:SizeMismatch', ...
                      'Expected %d parameters, got %d', N_expected, length(ParValues));
            end

            % Set parameters
            Obj.Tran2DObj.ParX = ParValues;

            if Args.Verbose
                fprintf('Tran2D parameters updated: %d coefficients\n', length(ParValues));
            end
        end

        function AllFunPar = getAllFunParWithTran2D(Obj)
            % Get all parameters including both function and Tran2D parameters
            % Output : - AllFunPar: Structure with fields:
            %                   .Name - Combined parameter names
            %                   .Val - Combined parameter values
            %                   .FitPar - Combined fitted parameter flags
            % Author : D. Kovaleva (Nov 2025)
            % Example: AllParams = Model.getAllFunParWithTran2D();

            % Get function parameters
            FunPar = Obj.getAllFunPar();

            % Get Tran2D parameters
            PosPar = Obj.getTran2DPar();

            % Combine them (vertical concatenation for column vectors)
            AllFunPar.Name = [FunPar.Name; PosPar.Name];
            AllFunPar.Val = [FunPar.Val; PosPar.Val];
            AllFunPar.FitPar = [FunPar.FitPar; PosPar.FitPar];
        end

        function [FitPosResult, Obj] = fitPositionPolynomial(Obj, X, Y, MagResid, Args)
            % Fit position polynomial using Tran2D's fitDesignMatrix method
            % Input  : - X: X coordinates [N_sources x 1]
            %          - Y: Y coordinates [N_sources x 1]
            %          - MagResid: Magnitude residuals [N_sources x 1]
            %                   (mag_obs - mag_model_base)
            %          * ...,key,val,...
            %            'Method' - Fitting method: '\' or 'lscov'. Default is '\'.
            %            'ErrMag' - Magnitude errors for lscov method. Default is [].
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - FitPosResult: Structure with fit results from Tran2D.fitDesignMatrix
            %          - Obj: Updated CompositeFun object with fitted PosParams
            % Author : D. Kovaleva (Dec 2025)
            % Example: [FitPosResult, Model] = Model.fitPositionPolynomial(X, Y, MagResid);
            %
            % Note: This method wraps Tran2D's fitDesignMatrix for convenience.
            %       For position-dependent photometric corrections:
            %       - ParX contains the magnitude correction polynomial
            %       - ParY is not used (can be set to zeros or ignored)

            arguments
                Obj
                X
                Y
                MagResid
                Args.Method = '\';
                Args.ErrMag = [];
                Args.Verbose logical = false
            end

            if ~Obj.UseTran2D || isempty(Obj.Tran2DObj)
                error('CompositeFun:fitPositionPolynomial:NoTran2D', ...
                      'Tran2D not initialized. Call addTran2D() first.');
            end

            % Ensure column vectors
            X = X(:);
            Y = Y(:);
            MagResid = MagResid(:);

            % Get design matrix using Tran2D
            Coo = [X, Y];
            [Hx, Hy] = Obj.Tran2DObj.design_matrix(Coo);

            % For photometric corrections, we only use ParX (magnitude correction)
            % ParY can be set to zeros since we don't need Y-coordinate transformation
            DummyY = zeros(size(MagResid));

            % Use Tran2D's fitDesignMatrix method
            % MagResid follows convention: Mag_obs - Mag_pred (positive when model too bright in flux)
            % ParX fitted to MagResid directly, so:
            %   - FieldCorrectionMag = Hx * ParX ≈ MagResid
            %   - T_correction = 10^(-0.4 * FieldCorrectionMag)
            %   - If model too bright (MagResid > 0), T_correction < 1, reducing transmission
            [FitPosResult, Obj.Tran2DObj] = Obj.Tran2DObj.fitDesignMatrix(Hx, Hy, MagResid, DummyY, ...
                                                                     'Method', Args.Method, ...
                                                                     'ErrX', Args.ErrMag, ...
                                                                     'ErrY', []);

            if Args.Verbose
                fprintf('Position polynomial fitted using Tran2D.fitDesignMatrix\n');
                fprintf('  Number of sources: %d\n', length(X));
                fprintf('  Number of parameters: %d\n', length(FitPosResult.ParX));
                fprintf('  RMS residual: %.4f mag\n', FitPosResult.RmsX);
                fprintf('  ParX range: [%.6f, %.6f]\n', min(FitPosResult.ParX), max(FitPosResult.ParX));
            end
        end

        function [Residuals, Cost, PredictedValues, UnweightedResiduals, MagErr] = costFun(Obj, InputValues, ObservedValues, Args)
            % Cost function for CompositeFun optimization with optional Tran2D
            % Input  : - Obj - CompositeFun object.
            %          - InputValues - Wavelength grid [N_input x 1].
            %          - ObservedValues - Observed flux [N_obs x 1].
            %          * ...,key,val,...
            %            'TransParams' - Parameter override vector. Default is [].
            %            'X' - Source X coordinates [N_obs x 1]. Default is [].
            %            'Y' - Source Y coordinates [N_obs x 1]. Default is [].
            %            'WeightMatrix' - Calibrator spectra [N_wvl x N_obs]. Default is [].
            %            'PrecomputedMagErr' - Pre-computed magnitude errors [N_obs x 1].
            %                   Must be computed before optimization via PhotCalibTrans.propagateCalibratorMagErr.
            %                   Default is [].
            %            'PrecomputedSpecFluxMatrix' - Pre-computed interpolated spectra [N_input x N_obs].
            %                   Calibrator spectra interpolated onto transmission wavelength grid.
            %                   Must be computed before optimization via PhotCalibTrans.resampleCalibratorSpectra.
            %                   Avoids repeated interpolation on every costFun call. Default is [].
            %            'IntegrationDim' - Integration dimension (1 or 2). Default is 2.
            %            'TransmissionMode' - Enable transmission mode. Default is false.
            %            'CalibWavelength' - Calibrator wavelength grid [Angstrom]. Default is SpecWvl.
            %            'ExpTime' - Exposure time [s]. Default is 20.
            %            'Aperture_area_m2' - Aperture area [m^2]. Default is LAST.
            %            'CostType' - 'sse', 'mae', or 'rmse'. Default is 'sse'.
            %            'ValInp' - Validate inputs. Default is true.
            %            'Verbose' - Verbose output. Default is false.
            % Output : - Residuals - Weighted residuals [N_obs x 1] (r/σ if MagErr available).
            %          - Cost - Scalar cost value.
            %          - PredictedValues - Model-predicted flux [N_obs x 1] (in TransmissionMode).
            %          - UnweightedResiduals - Unweighted magnitude residuals [N_obs x 1].
            %          - MagErr - Magnitude errors [N_obs x 1] (from PrecomputedMagErr).
            % Author : D. Kovaleva (Dec 2025)

            arguments
                Obj
                InputValues                           % Input values [N_input x 1]
                ObservedValues                        % Observed values [N_obs x 1]
                Args.TransParams = []                 % Parameter override
                Args.X = []                           % X coordinates [N_obs x 1]
                Args.Y = []                           % Y coordinates [N_obs x 1]
                Args.WeightMatrix = []                % Calibrator spectra [N_wvl x N_obs]
                Args.PrecomputedMagErr = []           % Pre-computed magnitude errors [N_obs x 1]
                Args.PrecomputedSpecFluxMatrix = []   % Pre-computed interpolated spectra [N_input x N_obs]
                Args.IntegrationDim = 2               % Integration dimension
                Args.TransmissionMode logical = false % Enable transmission-specific mode
                Args.CalibWavelength = CompositeFun.SpecWvl  % Calibrator wavelength grid [Angstrom]
                Args.ExpTime = 20                     % Exposure time [s]
                Args.Aperture_area_m2 = pi * (0.1397)^2  % LAST aperture [m^2]
                Args.CostType = 'sse'                 % Cost function type
                Args.ValInp logical = true
                Args.Verbose logical = false
            end

            % Initialize outputs for early return on validation error
            Residuals = [];
            Cost = Inf;
            PredictedValues = [];
            UnweightedResiduals = [];
            MagErr = [];

          %      H = constant.h('SI');      % Planck constant [J·s]
                H = 6.62607015e-34;         % SI 2019 Plank constant
                C = constant.c('SI');      % Speed of light [m/s]

            % ====================================================================
            % STEP 1: VALIDATE INPUTS
            % ====================================================================

            if Args.ValInp
                if ~isa(Obj, 'tools.math.fun.CompositeFun')
                    Obj.addStatus('costFun', 'error', 'Obj must be a tools.math.fun.CompositeFun object', 'CompositeFun:InvalidObject');
                    Residuals = NaN;
                    PredictedValues = NaN;
                    UnweightedResiduals = NaN;
                    return;
                end
            end

            % Extract or validate parameters
            if isempty(Args.TransParams)
                TransParams = Obj.valuesAllFunPar();
                if Args.Verbose
                    fprintf('Extracted %d parameters from CompositeFun\n', length(TransParams));
                end
            else
                TransParams = Args.TransParams;
                if Args.ValInp
                    ExpectedNumParams = Obj.numAllFunPar();
                    if length(TransParams) ~= ExpectedNumParams
                        Obj.addStatus('costFun', 'error', sprintf('TransParams size (%d) does not match Obj parameter count (%d)', length(TransParams), ExpectedNumParams), 'CompositeFun:ParamCountMismatch');
                        Residuals = NaN;
                        PredictedValues = NaN;
                        UnweightedResiduals = NaN;
                        return;
                    end
                end
                if Args.Verbose
                    fprintf('Using provided TransParams (%d parameters)\n', length(TransParams));
                end
            end

            if Args.ValInp
                if any(isnan(TransParams))
                    Obj.addStatus('costFun', 'error', 'TransParams contains NaN', 'CompositeFun:NaNParams');
                    Residuals = NaN;
                    PredictedValues = NaN;
                    UnweightedResiduals = NaN;
                    return;
                end
            end

            % Ensure column vectors
            InputValues = InputValues(:);
            ObservedValues = ObservedValues(:);

            NCalUsed = length(ObservedValues);
            Ninput = length(InputValues);

            % Initialize outputs for all paths
            UnweightedResiduals = [];
            MagErr = [];

            if Args.ValInp
                if NCalUsed == 0
                    Obj.addStatus('costFun', 'error', 'ObservedValues is empty', 'CompositeFun:EmptyObserved');
                    Residuals = NaN;
                    PredictedValues = NaN;
                    UnweightedResiduals = NaN;
                    return;
                end
                if Ninput == 0
                    Obj.addStatus('costFun', 'error', 'InputValues is empty', 'CompositeFun:EmptyInput');
                    Residuals = NaN;
                    PredictedValues = NaN;
                    UnweightedResiduals = NaN;
                    return;
                end

                % Validate X, Y if using Tran2D and coordinates are provided
                if Obj.UseTran2D && ~isempty(Args.X) && ~isempty(Args.Y)
                    if length(Args.X) ~= NCalUsed
                        Obj.addStatus('costFun', 'error', sprintf('X size (%d) does not match number of observations (%d)', length(Args.X), NCalUsed), 'CompositeFun:XSizeMismatch');
                        Residuals = nan(NCalUsed, 1);
                        PredictedValues = nan(NCalUsed, 1);
                        UnweightedResiduals = nan(NCalUsed, 1);
                        return;
                    end
                    if length(Args.Y) ~= NCalUsed
                        Obj.addStatus('costFun', 'error', sprintf('Y size (%d) does not match number of observations (%d)', length(Args.Y), NCalUsed), 'CompositeFun:YSizeMismatch');
                        Residuals = nan(NCalUsed, 1);
                        PredictedValues = nan(NCalUsed, 1);
                        UnweightedResiduals = nan(NCalUsed, 1);
                        return;
                    end
                end

                % Validate WeightMatrix if provided (skip for TransmissionMode - different dimensions)
                if ~isempty(Args.WeightMatrix) && ~Args.TransmissionMode
                    [WRows, WCols] = size(Args.WeightMatrix);
                    if WRows ~= NCalUsed || WCols ~= Ninput
                        Obj.addStatus('costFun', 'error', sprintf('WeightMatrix size [%d x %d] must match [NCalUsed=%d x Ninput=%d]', WRows, WCols, NCalUsed, Ninput), 'CompositeFun:WeightMatrixSize');
                        Residuals = nan(NCalUsed, 1);
                        PredictedValues = nan(NCalUsed, 1);
                        UnweightedResiduals = nan(NCalUsed, 1);
                        return;
                    end
                end
            end

            if Args.Verbose
                fprintf('=== COMPOSITEFUN COST FUNCTION ===\n');
                fprintf('Number of observations: %d\n', NCalUsed);
                fprintf('Number of input points: %d\n', Ninput);
                fprintf('Input range: %.3f - %.3f\n', min(InputValues), max(InputValues));
            end

            % ====================================================================
            % STEP 2: CALCULATE PREDICTED VALUES AND RESIDUALS
            % ====================================================================

            UsePositionCorrections = Obj.UseTran2D && ~isempty(Args.X) && ~isempty(Args.Y);

            if Args.TransmissionMode
                % ============================================================
                % TRANSMISSION-SPECIFIC MODE
                % ============================================================
                if isempty(Args.WeightMatrix)
                    Obj.addStatus('costFun', 'error', 'TransmissionMode requires WeightMatrix (spectra)', 'CompositeFun:MissingWeightMatrix');
                    Residuals = nan(NCalUsed, 1);
                    PredictedValues = nan(NCalUsed, 1);
                    UnweightedResiduals = nan(NCalUsed, 1);
                    return;
                end

                % Evaluate model
                if UsePositionCorrections
                    % Evaluate with position corrections: [N_obs x N_input]
                    ModelOutput = Obj.evaluateWithPosition(InputValues, Args.X, Args.Y, ...
                        'TransParams', TransParams(:)');
                    if Args.Verbose
                        fprintf('Evaluated model with Tran2D position corrections\n');
                    end
                else
                    % Evaluate without position corrections: [N_input x 1]
                    ModelOutput = Obj.evaluateAllFunParInput(InputValues, TransParams(:)');
                    if Args.Verbose
                        fprintf('Evaluated model without position corrections\n');
                    end
                end

                % Calibrator spectra [N_SpecWvl x N_obs] and wavelength grid [Angstrom]
                Spec = Args.WeightMatrix;
                SpecWvl = Args.CalibWavelength(:);

                % Use pre-computed interpolated spectra if provided, otherwise compute
                if ~isempty(Args.PrecomputedSpecFluxMatrix)
                    % Use pre-computed interpolated spectra matrix
                    SpecFluxMatrix = Args.PrecomputedSpecFluxMatrix;
                    if Args.Verbose
                        fprintf('Using pre-computed SpecFluxMatrix [%d x %d]\n', size(SpecFluxMatrix, 1), size(SpecFluxMatrix, 2));
                    end
                else
                    % Compute interpolated spectra (expensive, should be pre-computed for optimization)
                    % Calibrator spectral boundaries (e.g., Gaia XP: 3360-10200 Angstrom)
                    SpecWvlMin = min(SpecWvl);
                    SpecWvlMax = max(SpecWvl);

                    % Wavelength region masks for extrapolation
                    MaskGaia = (InputValues >= SpecWvlMin) & (InputValues <= SpecWvlMax);
                    MaskUV = (InputValues < SpecWvlMin);
                    MaskIR = (InputValues > SpecWvlMax);
                    WvlGaiaRegion = InputValues(MaskGaia);

                    % Interpolate calibrator spectra onto transmission grid (vectorized)
                    SpecFluxMatrix = zeros(Ninput, NCalUsed);
                    SpecFluxMatrix(MaskGaia, :) = interp1(SpecWvl, Spec, WvlGaiaRegion, 'linear');

                    % UV/IR extrapolation: constant boundary values
                    if any(MaskUV)
                        EdgeValuesUV = interp1(SpecWvl, Spec, SpecWvlMin, 'linear');
                        SpecFluxMatrix(MaskUV, :) = repmat(EdgeValuesUV, sum(MaskUV), 1);
                    end
                    if any(MaskIR)
                        EdgeValuesIR = interp1(SpecWvl, Spec, SpecWvlMax, 'linear');
                        SpecFluxMatrix(MaskIR, :) = repmat(EdgeValuesIR, sum(MaskIR), 1);
                    end
                end

                % Apply transmission to spectra
                % ModelOutput: [NCalUsed x Ninput] with position corrections, [Ninput x 1] without
                if UsePositionCorrections
                    TransmittedSpectra = SpecFluxMatrix .* ModelOutput';  % [Ninput x NCalUsed]
                else
                    TransmittedSpectra = SpecFluxMatrix .* ModelOutput;   % [Ninput x NCalUsed] via broadcast
                end

                % Integrate: Int[Flux(Lambda) * T(Lambda) * Lambda] dLambda
                % Gaia flux is W/m^2/nm, so Lambda in nm for dimensional consistency
                LambdaNm = InputValues / 10;  % Angstrom to nm
                Integrand = TransmittedSpectra' .* LambdaNm(:)';  % [NCalUsed x Ninput]
                Avector = tools.math.integral.trapzmat(InputValues(:)', Integrand, 2);
                Avector = Avector(:);

                % Convert to photon counts
                B = H * C * 1e10;  % h*c with Angstrom-to-m conversion
                PredictedFlux = Args.ExpTime * Args.Aperture_area_m2 * Avector / B;

                % Magnitude errors: must be pre-computed and stored in SourceData
                % (computed once by PhotCalibTrans.propagateCalibratorMagErr before optimization)
                UseWeighting = ~isempty(Args.PrecomputedMagErr);
                if UseWeighting
                    MagErr = Args.PrecomputedMagErr(:);
                    if length(MagErr) ~= NCalUsed
                        warning('CompositeFun:MagErrMismatch', ...
                            'PrecomputedMagErr length (%d) != NCalUsed (%d). Using unweighted.', ...
                            length(MagErr), NCalUsed);
                        UseWeighting = false;
                        MagErr = [];
                    end
                else
                    MagErr = [];
                end

                % Magnitude residuals: DiffMag = 2.5*log10(Pred/Obs) = Mag_obs - Mag_pred
                % Positive when model too bright in flux space
                DiffMag = 2.5 * log10(PredictedFlux ./ ObservedValues);
                DiffMag(isnan(DiffMag) | isinf(DiffMag)) = 0;

                % Apply weights if available (for lsqnonlin: r_weighted = r/sigma)
                if UseWeighting
                    Residuals = DiffMag ./ MagErr;
                else
                    Residuals = DiffMag;
                end
                UnweightedResiduals = DiffMag;
                PredictedValues = PredictedFlux;

                if Args.Verbose
                    fprintf('Transmission mode: %d wavelength points, flux range %.2e - %.2e\n', ...
                        Ninput, min(PredictedFlux), max(PredictedFlux));
                end

            elseif ~isempty(Args.WeightMatrix)
                % ============================================================
                % GENERAL WEIGHTED INTEGRATION MODE
                % ============================================================

                % Evaluate model
                if UsePositionCorrections
                    % Evaluate with position corrections: [N_obs x N_input]
                    ModelOutput = Obj.evaluateWithPosition(InputValues, Args.X, Args.Y, ...
                        'TransParams', TransParams(:)');
                    % WeightMatrix is [N_obs x N_input]
                    % Element-wise multiply then integrate
                    Integrand = ModelOutput .* Args.WeightMatrix;
                else
                    % Evaluate without position corrections: [N_input x 1]
                    ModelOutput = Obj.evaluateAllFunParInput(InputValues, TransParams(:)');
                    % WeightMatrix is [N_obs x N_input]
                    % Broadcast multiply
                    Integrand = Args.WeightMatrix .* ModelOutput';  % [N_obs x N_input]
                end

                % Integrate along specified dimension
                PredictedValues = sum(Integrand, Args.IntegrationDim);
                PredictedValues = PredictedValues(:);  % Ensure column vector

                % Calculate residuals for general weighted mode
                Residuals = PredictedValues - ObservedValues;

                if Args.Verbose
                    fprintf('Applied WeightMatrix integration (dim=%d)\n', Args.IntegrationDim);
                end

            else
                % ============================================================
                % DIRECT COMPARISON MODE
                % ============================================================

                % Evaluate model
                if UsePositionCorrections
                    % Evaluate with position corrections: [N_obs x N_input]
                    ModelOutput = Obj.evaluateWithPosition(InputValues, Args.X, Args.Y, ...
                        'TransParams', TransParams(:)');
                    % Need to reduce to [N_obs x 1] - use mean across input dimension
                    PredictedValues = mean(ModelOutput, 2);
                    if Args.Verbose
                        fprintf('Direct comparison: averaged position-dependent output\n');
                    end
                else
                    % Evaluate without position corrections: [N_input x 1]
                    ModelOutput = Obj.evaluateAllFunParInput(InputValues, TransParams(:)');
                    % Need to match with [N_obs x 1]
                    if Ninput == NCalUsed
                        PredictedValues = ModelOutput;
                    else
                        Obj.addStatus('costFun', 'error', sprintf('Direct comparison requires Ninput (%d) == NCalUsed (%d) or use WeightMatrix', Ninput, NCalUsed), 'CompositeFun:InputObsMismatch');
                        Residuals = nan(NCalUsed, 1);
                        PredictedValues = nan(NCalUsed, 1);
                        UnweightedResiduals = nan(NCalUsed, 1);
                        return;
                    end
                end

                % Calculate residuals for direct mode
                Residuals = PredictedValues - ObservedValues;
            end

            % ====================================================================
            % STEP 3: CALCULATE COST
            % ====================================================================

            switch lower(Args.CostType)
                case 'sse'
                    Cost = sum(Residuals.^2);
                case 'mae'
                    Cost = mean(abs(Residuals));
                case 'rmse'
                    Cost = sqrt(mean(Residuals.^2));
                otherwise
                    Obj.addStatus('costFun', 'error', sprintf('Unknown CostType: %s', Args.CostType), 'CompositeFun:UnknownCostType');
                    % Cost already initialized to Inf, Residuals already set
                    return;
            end

            % Set default UnweightedResiduals and MagErr for non-TransmissionMode paths
            if ~exist('UnweightedResiduals', 'var')
                UnweightedResiduals = Residuals;
            end
            if ~exist('MagErr', 'var')
                MagErr = [];
            end

            if Args.Verbose
                fprintf('Residuals: mean=%.4e, std=%.4e\n', mean(UnweightedResiduals), std(UnweightedResiduals));
                fprintf('Cost (%s): %.4e\n', Args.CostType, Cost);
                fprintf('=== COMPOSITEFUN COST FUNCTION COMPLETE ===\n\n');
            end
        end

        function [Obj, FitResult] = fitPar(Obj, InputValues, ObservedValues, Args)
            % General parameter fitting for CompositeFun with optional Tran2D and sigma clipping.
            % Fits free parameters by minimizing residuals between model predictions and observations.
            % Supports alternating optimization between base function parameters (nonlinear via
            % lsqNonLinWithFixed) and position parameters (linear via Tran2D.fitDesignMatrix).
            %
            % Input  : - Obj - CompositeFun object (modified in place)
            %          - InputValues - Independent variable grid where the CompositeFun is evaluated (e.g., wavelength grid)
            %                   Vector [N_input x 1]
            %          - ObservedValues - Observed values to be compared to
            %          the model [N_obs x 1] (e.g. observed flux)
            %          * ...,key,val,...
            %            'CostArgs' - Cell array with additional arguments to pass to Obj.costFun (key-value pairs)
            %                   Default is {}.
            %            'X' - Source X coordinates [N_obs x 1] for Tran2D corrections
            %                   Required if UseTran2D is true. Default is [].
            %            'Y' - Source Y coordinates [N_obs x 1] for Tran2D corrections
            %                   Required if UseTran2D is true. Default is [].
            %            'FitTransmission' - Fit base function parameters (nonlinear)
            %                   Default is true.
            %            'FitPosition' - Fit Tran2D position parameters (linear)
            %                   Only used if UseTran2D is true. Default is true.
            %            'FreeParamIndices' - Indices of parameters to fit
            %                   If empty, fits all parameters with FitPar=true.
            %                   Default is [].
            %            'SigmaClip' - Enable sigma clipping outlier rejection
            %                   Default is false.
            %            'SigmaThresh' - Threshold for sigma clipping [sigma units]
            %                   Default is 3.0.
            %            'SigmaIter' - Maximum sigma clipping iterations
            %                   Default is 5.
            %            'SigmaClipMethod' - Sigma clipping method:
            %                   'median' - Astropy-style iterative clipping on abs(residuals)
            %                              using median center and std scale (default)
            %                   'weighted' - Threshold on error-normalized residuals |r/σ| > N
            %            'MinCalibrators' - Minimum calibrators to keep during clipping.
            %                   If clipping would leave fewer, clipping stops. Default is 0 (no limit).
            %            'OptimOptions' - Options structure for lsqnonlin
            %                   Passed via tools.math.fit.lsqNonLinWithFixed wrapper.
            %                   Default is optimoptions('lsqnonlin', 'Display', 'off').
            %            'OptimizationSequence' - Multi-stage optimization sequence (struct array)
            %                   If provided, enables multi-stage sequence of optimization, describing which parameters are optimized at the current step.
            %                   When provided, it is stored in Obj.OptSeq for future use. If not provided, uses stored Obj.OptSeq if available.
            %                   Description of each stage is a struct with:
            %                   .StageName - Name of the stage
            %                   .FreeParams - Struct array with .Function and .Parameter fields
            %                                Empty [] for field correction stage (linear fit)
            %                   .SigmaClip - Enable sigma clipping for this stage
            %                   .SigmaThresh - Threshold for sigma clipping [sigma units]
            %                   .SigmaIter - Number of sigma clipping iterations
            %                   .SigmaClipMethod - Clipping method (optional, defaults to Args.SigmaClipMethod)
            %                   .MinCalibrators - Min calibrators to keep (optional, defaults to Args.MinCalibrators)
            %                   .Description - Description of the stage
            %                   Default is [] (single-stage mode if Obj.OptSeq is also empty).
            %            'ValInp' - Boolean flag for validation of inputs and setup. Default is true.
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - Obj - Updated CompositeFun object with fitted parameters
            %                   Also sets Obj.Chi2 and Obj.DOF from the final fit.
            %          - FitResult - Structure with fields:
            %                   Single-stage mode:
            %                     .Cost - Final cost value
            %                     .RMS - RMS of residuals
            %                     .Residuals - Final residuals [N_obs x 1]
            %                     .WeightedResiduals - Weighted residuals (r/σ)
            %                     .NCalUsed - Number of observations after clipping
            %                     .NumClipped - Number of clipped outliers
            %                     .KeepMask - Logical mask of surviving observations
            %                     .ConvergedSigmaClip - True if sigma clipping converged
            %                     .Chi2 - Chi-squared value
            %                     .DOF - Degrees of freedom
            %                     .MagErr - Magnitude errors
            %                     .PredictedFlux - Model-predicted flux values
            %                   Multi-stage mode: Array of structs with per-stage results
            %                     FitResult(i).StageName, .Method, .Cost, .RMS, .Residuals,
            %                     .NCalUsed, .NumClipped, .IsFieldCorrection, .Chi2, .DOF,
            %                     .MagErr, .PredictedFlux
            % Author : D. Kovaleva (Dec 2025)
            % Example: % Example 1: Simple single-stage fit
            %          Model = tools.math.fun.CompositeFun.model(FunList);
            %          [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
            %              'FitTransmission', true, 'FitPosition', false);
            %
            %          % Example 2: Single-stage with position and sigma clipping
            %          Model = tools.math.fun.CompositeFun.model(FunList, 'UseTran2D', true);
            %          [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
            %              'X', X, 'Y', Y, 'SigmaClip', true, 'SigmaThresh', 3.0);
            %
            %          % Example 3: Multi-stage optimization sequence
            %          % Define 2-stage optimization: (1) fit aerosol, (2) fit position
            %          OptSeq(1).StageName = 'AerosolOpt';
            %          OptSeq(1).FreeParams(1).Function = 'Aerosol';
            %          OptSeq(1).FreeParams(1).Parameter = 'TauAod500';
            %          OptSeq(1).SigmaClip = true;
            %          OptSeq(1).SigmaThresh = 3.0;
            %          OptSeq(1).SigmaIter = 3;
            %          OptSeq(1).Description = 'Optimize aerosol optical depth';
            %          OptSeq(2).StageName = 'FieldCorr';
            %          OptSeq(2).FreeParams = [];  % Empty for field correction stage
            %          OptSeq(2).SigmaClip = true;
            %          OptSeq(2).SigmaThresh = 2.0;
            %          OptSeq(2).SigmaIter = 2;
            %          OptSeq(2).Description = 'Position-dependent field correction';
            %          % Build model with Tran2D
            %          Model = tools.math.fun.CompositeFun.model(FunList, 'UseTran2D', true);
            %          CostArgs = {'WeightMatrix', CalibSpec, 'TransmissionMode', true, ...
            %                      'CalibWavelength', SpecWvl, 'ExpTime', 20, 'Aperture_area_m2', pi*0.1397^2};
            %          [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
            %              'CostArgs', CostArgs, 'X', X, 'Y', Y, ...
            %              'OptimizationSequence', OptSeq, 'Verbose', true);
            %          % FitResult is an array: FitResult(1) for Stage 1, FitResult(2) for Stage 2
            %          fprintf('Stage 1 RMS: %.4f mag\n', FitResult(1).RMS);
            %          fprintf('Stage 2 RMS: %.4f mag\n', FitResult(2).RMS);

            arguments
                Obj
                InputValues
                ObservedValues
                Args.CostArgs cell = {}
                Args.X = []
                Args.Y = []
                Args.FitTransmission logical = true
                Args.FitPosition logical = true
                Args.FreeParamIndices = []
                Args.SigmaClip logical = false
                Args.SigmaThresh = 3.0
                Args.SigmaIter = 5
                Args.SigmaClipMethod = 'median'  % 'median' (astropy-style) or 'weighted' (|r/σ| > N)
                Args.MinCalibrators = 0  % Minimum calibrators to keep (0 = no limit)
                Args.OptimOptions = []
                Args.OptimizationSequence = []  % Multi-stage optimization sequence
                Args.ValInp logical = true
                Args.Verbose logical = false
            end

            % Initialize FitResult with failure values for early return on validation error
            FitResult = struct();
            FitResult.Cost = Inf;
            FitResult.RMS = NaN;
            FitResult.Residuals = [];
            FitResult.WeightedResiduals = [];
            FitResult.NCalUsed = 0;
            FitResult.NumClipped = 0;
            FitResult.KeepMask = [];
            FitResult.ConvergedSigmaClip = false;
            FitResult.Chi2 = NaN;
            FitResult.DOF = NaN;
            FitResult.MagErr = [];
            FitResult.PredictedFlux = [];

            % ====================================================================
            % STEP 0: MULTI-STAGE OPTIMIZATION (if OptimizationSequence provided)
            % ====================================================================

            % Use provided OptimizationSequence or fall back to stored Obj.OptSeq
            if ~isempty(Args.OptimizationSequence)
                % Only store if it's a multi-stage sequence (external call)
                % Don't overwrite during recursive single-stage calls from fitMultiStage
                if length(Args.OptimizationSequence) > 1
                    Obj.OptSeq = Args.OptimizationSequence;  % Store for future reference
                end
            elseif isempty(Args.OptimizationSequence) && ~isempty(Obj.OptSeq)
                Args.OptimizationSequence = Obj.OptSeq;  % Use stored sequence
            end

            % Only call fitMultiStage if there are multiple stages (>1)
            % Single-stage sequences run as regular single-stage fits
            if ~isempty(Args.OptimizationSequence) && length(Args.OptimizationSequence) > 1
                % Multi-stage optimization mode
                [Obj, FitResult] = fitMultiStage(Obj, InputValues, ObservedValues, Args);
                return;
            end

            % ====================================================================
            % STEP 1: VALIDATE INPUTS AND SETUP (Single-stage mode)
            % ====================================================================

            % Ensure column vectors
            InputValues = InputValues(:);
            ObservedValues = ObservedValues(:);

            NCalUsedInitial = length(ObservedValues);

            if Args.ValInp
                if Obj.UseTran2D && Args.FitPosition
                    if isempty(Args.X) || isempty(Args.Y)
                        Obj.addStatus('fitPar', 'error', 'X and Y coordinates required when fitting position parameters', 'CompositeFun:MissingCoordinates');
                        FitResult.KeepMask = false(NCalUsedInitial, 1);
                        return;
                    end
                    if length(Args.X) ~= NCalUsedInitial || length(Args.Y) ~= NCalUsedInitial
                        Obj.addStatus('fitPar', 'error', sprintf('X, Y size must match number of observations (%d)', NCalUsedInitial), 'CompositeFun:CoordinateSizeMismatch');
                        FitResult.KeepMask = false(NCalUsedInitial, 1);
                        return;
                    end
                end

                % Check for non-zero Tran2D parameters that might cause issues
                if Obj.UseTran2D && ~isempty(Obj.Tran2DObj)
                    MaxParX = max(abs(Obj.Tran2DObj.ParX));
                    if MaxParX > 100
                        warning('CompositeFun:fitPar:LargeTran2DParams', ...
                                'Tran2D ParX contains large values (max abs: %.2e). This may cause Inf during evaluation.\n  Consider calling Model.resetTran2DParams() before fitting.', ...
                                MaxParX);
                        if Args.Verbose
                            fprintf('  Current ParX: '); disp(Obj.Tran2DObj.ParX);
                        end
                    end
                end
            end

            % Setup optimization options for nonlinear solver
            if isempty(Args.OptimOptions)
                if Args.Verbose
                    DisplayOpt = 'iter';
                else
                    DisplayOpt = 'off';
                end
                OptimOpts = optimoptions('lsqnonlin', 'Display', DisplayOpt, ...
                    'MaxIterations', 1000, 'FunctionTolerance', 1e-8);
            else
                OptimOpts = Args.OptimOptions;
            end

            if Args.Verbose
                fprintf('=== COMPOSITEFUN PARAMETER FITTING ===\n');
                fprintf('Initial observations: %d\n', NCalUsedInitial);
                fprintf('Fit transmission parameters: %d\n', Args.FitTransmission);
                fprintf('Fit position parameters: %d\n', Args.FitPosition && Obj.UseTran2D);
                fprintf('Sigma clipping: %d (thresh=%.1f, max_iter=%d)\n\n', ...
                        Args.SigmaClip, Args.SigmaThresh, Args.SigmaIter);
            end

            % ====================================================================
            % STEP 2: DETERMINE FREE PARAMETERS
            % ====================================================================

            if Args.FitTransmission
                if isempty(Args.FreeParamIndices)
                    % Use FitPar flags from model
                    AllFunPar = Obj.getAllFunPar();
                    FreeParamIndices = find(AllFunPar.FitPar);
                    if isempty(FreeParamIndices)
                        warning('No parameters marked with FitPar=true, fitting all parameters');
                        FreeParamIndices = 1:length(AllFunPar.Val);
                    end
                else
                    FreeParamIndices = Args.FreeParamIndices;
                end

                if Args.Verbose
                    fprintf('Free parameters: %d\n', length(FreeParamIndices));
                end
            else
                FreeParamIndices = [];
            end

            % ====================================================================
            % STEP 3: SIGMA CLIPPING LOOP
            % ====================================================================

            CurrentObs = ObservedValues;
            CurrentX = Args.X;
            CurrentY = Args.Y;

            % Initialize KeepMask to track which original observations survive clipping
            NCalUsedInitial = length(ObservedValues);
            KeepMask = true(NCalUsedInitial, 1);
            CurrentIndices = (1:NCalUsedInitial)';  % Maps current obs to original indices

            % Loop structure: initial fit, then [clip → refit] × SigmaIter
            % Matches Python fit_transmission: 1 initial fit + N clip-refit cycles
            NumIterations = 1 + Args.SigmaClip * Args.SigmaIter;
            ConvergedSigmaClip = false;

            for Iter = 1:NumIterations
                if ~ConvergedSigmaClip

                    % =============================================================
                    % SIGMA CLIPPING (skip first iteration — no residuals yet)
                    % =============================================================

                    if Args.SigmaClip && Iter > 1
                        % Skip clipping if already below MinCalibrators
                        if Args.MinCalibrators > 0 && length(CurrentObs) <= Args.MinCalibrators
                            ConvergedSigmaClip = true;
                            if Args.Verbose
                                fprintf('--- Sigma clipping skipped: %d calibrators <= %d minimum ---\n', ...
                                    length(CurrentObs), Args.MinCalibrators);
                            end
                        end
                    end

                    if Args.SigmaClip && Iter > 1 && ~ConvergedSigmaClip
                        if Args.Verbose
                            fprintf('--- Sigma clipping iteration %d/%d ---\n', Iter-1, Args.SigmaIter);
                        end

                        % Sigma clipping via helper function
                        [OutlierMask, ClipInfo] = tools.math.stat.sigmaClip(...
                            UnweightedResiduals, Args.SigmaThresh, ...
                            'Method', Args.SigmaClipMethod, 'Errors', MagErr);

                        if ~ClipInfo.Success
                            ConvergedSigmaClip = true;
                            Obj.addStatus('fitPar', 'warning', ...
                                sprintf('Sigma clipping failed: %s', ClipInfo.ErrorMsg), ...
                                'CompositeFun:SigmaClipFailed');
                            if Args.Verbose
                                fprintf('Sigma clipping failed: %s; skipping\n', ClipInfo.ErrorMsg);
                            end
                        end
                    end

                    if Args.SigmaClip && Iter > 1 && ~ConvergedSigmaClip && ClipInfo.Success
                        NumOutliers = ClipInfo.NumOutliers;

                        if Args.Verbose
                            fprintf('Sigma clipping (%s): threshold=%.1f, outliers=%d\n', ...
                                Args.SigmaClipMethod, Args.SigmaThresh, NumOutliers);
                        end

                        % MinCalibrators safeguard: stop clipping if too few would remain
                        NRemaining = sum(~OutlierMask);
                        SafeguardTriggered = NumOutliers > 0 && Args.MinCalibrators > 0 ...
                            && NRemaining < Args.MinCalibrators;

                        if SafeguardTriggered
                            ConvergedSigmaClip = true;
                            if Args.Verbose
                                fprintf('Sigma clipping stopped: would leave %d < %d calibrators\n', ...
                                    NRemaining, Args.MinCalibrators);
                            end
                        elseif NumOutliers == 0
                            ConvergedSigmaClip = true;
                            if Args.Verbose
                                fprintf('Sigma clipping converged (no outliers)\n');
                            end
                        else
                            % Remove outliers
                            IterKeepMask = ~OutlierMask;

                            % Update global KeepMask at original indices
                            KeepMask(CurrentIndices(OutlierMask)) = false;
                            CurrentIndices = CurrentIndices(IterKeepMask);

                            CurrentObs = CurrentObs(IterKeepMask);
                            if ~isempty(CurrentX)
                                CurrentX = CurrentX(IterKeepMask);
                                CurrentY = CurrentY(IterKeepMask);
                            end

                            % Subset CostArgs arrays for remaining observations
                            Idx = find(strcmp(Args.CostArgs(1:2:end), 'WeightMatrix'));
                            if ~isempty(Idx)
                                Args.CostArgs{2*Idx} = Args.CostArgs{2*Idx}(:, IterKeepMask);
                            end
                            Idx = find(strcmp(Args.CostArgs(1:2:end), 'PrecomputedMagErr'));
                            if ~isempty(Idx) && ~isempty(Args.CostArgs{2*Idx})
                                Args.CostArgs{2*Idx} = Args.CostArgs{2*Idx}(IterKeepMask);
                            end
                            Idx = find(strcmp(Args.CostArgs(1:2:end), 'PrecomputedSpecFluxMatrix'));
                            if ~isempty(Idx) && ~isempty(Args.CostArgs{2*Idx})
                                Args.CostArgs{2*Idx} = Args.CostArgs{2*Idx}(:, IterKeepMask);
                            end

                            if Args.Verbose
                                fprintf('Removed %d outliers, %d observations remaining\n', ...
                                        NumOutliers, length(CurrentObs));
                            end
                        end
                    end

                    % =============================================================
                    % FIT PARAMETERS AND COMPUTE RESIDUALS
                    % (skip if sigma clipping just converged — previous residuals valid)
                    % =============================================================

                    if ~ConvergedSigmaClip

                        % FIT TRANSMISSION PARAMETERS (if requested)
                        if Args.FitTransmission && ~isempty(FreeParamIndices)
                            if Args.Verbose
                                fprintf('Fitting transmission parameters (nonlinear)...\n');
                            end

                            AllFunPar = Obj.getAllFunPar();
                            CurrentTransParams = AllFunPar.Val;

                            FitMask = false(size(CurrentTransParams));
                            FitMask(FreeParamIndices) = true;

                            if ~isempty(CurrentX)
                                ModelFun = @(X_dummy, P) Obj.costFun(InputValues, CurrentObs, ...
                                    Args.CostArgs{:}, 'TransParams', P, 'X', CurrentX, 'Y', CurrentY);
                            else
                                ModelFun = @(X_dummy, P) Obj.costFun(InputValues, CurrentObs, ...
                                    Args.CostArgs{:}, 'TransParams', P);
                            end

                            NumCurrent = length(CurrentObs);
                            X_dummy = (1:NumCurrent)';
                            Y_target = zeros(NumCurrent, 1);
                            Sigma_weights = ones(NumCurrent, 1);

                            [OptTransParams, ~, MinimizerInfo] = tools.math.fit.lsqNonLinWithFixed(...
                                X_dummy, Y_target, Sigma_weights, ModelFun, ...
                                'InitPar', CurrentTransParams, ...
                                'FitPar', FitMask, ...
                                'Lb', AllFunPar.Min, ...
                                'Ub', AllFunPar.Max, ...
                                'Opts', OptimOpts);

                            AllFunPar.Val = OptTransParams;
                            Obj.setAllFunPar(AllFunPar);
                        end

                        % FIT POSITION PARAMETERS (if requested and Tran2D enabled)
                        if Args.FitPosition && Obj.UseTran2D
                            if Args.Verbose
                                fprintf('Fitting position parameters (linear)...\n');
                            end

                            Obj.Tran2DObj.ParX = zeros(1, length(Obj.Tran2DObj.ParX));

                            if ~isempty(CurrentX)
                                [~, ~, ~, BaseResiduals, BaseMagErr] = Obj.costFun(InputValues, CurrentObs, ...
                                    Args.CostArgs{:}, 'X', CurrentX, 'Y', CurrentY);
                            else
                                [~, ~, ~, BaseResiduals, BaseMagErr] = Obj.costFun(InputValues, CurrentObs, Args.CostArgs{:});
                            end

                            if ~isempty(BaseMagErr) && all(BaseMagErr > 0)
                                [~, Obj] = Obj.fitPositionPolynomial(CurrentX, CurrentY, BaseResiduals, ...
                                    'Method', 'lscov', 'ErrMag', BaseMagErr, 'Verbose', false);
                            else
                                warning('CompositeFun:UnweightedPositionFit', ...
                                    'MagErr unavailable or invalid, using unweighted position polynomial fit.');
                                [~, Obj] = Obj.fitPositionPolynomial(CurrentX, CurrentY, BaseResiduals, ...
                                    'Verbose', false);
                            end
                        end

                        % CALCULATE RESIDUALS with all fitted parameters
                        if ~isempty(CurrentX)
                            [WeightedResiduals, Cost, PredictedFlux, UnweightedResiduals, MagErr] = Obj.costFun(InputValues, CurrentObs, ...
                                Args.CostArgs{:}, 'X', CurrentX, 'Y', CurrentY);
                        else
                            [WeightedResiduals, Cost, PredictedFlux, UnweightedResiduals, MagErr] = Obj.costFun(InputValues, CurrentObs, Args.CostArgs{:});
                        end

                        StageRMS = sqrt(mean(UnweightedResiduals.^2));

                        if Args.Verbose
                            fprintf('Current RMS: %.4f, NCalUsed: %d\n', StageRMS, length(UnweightedResiduals));
                        end
                    end
                end
            end

            % ====================================================================
            % STEP 4: FINALIZE RESULTS
            % ====================================================================

            NumClipped = NCalUsedInitial - length(CurrentObs);

            % Get quality metrics from minimizer if available
            % Note: MinimizerInfo.Resid contains weighted residuals, so we use
            % UnweightedResiduals for RMS calculation (calculated in last sigma clip iteration)
            if exist('MinimizerInfo', 'var')
                StageChi2 = MinimizerInfo.Chi2;
                StageDOF = MinimizerInfo.Dof;
                % RMS from unweighted residuals (StageRMS already set in sigma clipping loop)
            else
                StageChi2 = NaN;
                StageDOF = NaN;
                % StageRMS already calculated from costFun residuals
            end

            % Store fit quality metrics in object
            Obj.RMS = StageRMS;
            Obj.Chi2 = StageChi2;
            Obj.DOF = StageDOF;

            FitResult = struct();
            FitResult.Cost = Cost;
            FitResult.RMS = StageRMS;
            FitResult.Residuals = UnweightedResiduals;  % Unweighted for diagnostics
            FitResult.WeightedResiduals = WeightedResiduals;  % Weighted for reference
            FitResult.NCalUsed = length(CurrentObs);
            FitResult.NumClipped = NumClipped;
            FitResult.KeepMask = KeepMask;  % Logical mask of which original observations survived
            FitResult.ConvergedSigmaClip = ConvergedSigmaClip;
            FitResult.Chi2 = StageChi2;
            FitResult.DOF = StageDOF;
            FitResult.MagErr = MagErr;  % Magnitude errors from error propagation
            FitResult.PredictedFlux = PredictedFlux;  % Model-predicted flux for calibrators

            if Args.Verbose
                fprintf('\nTransmission optimization complete\n');
                fprintf('  Final observations: %d (clipped: %d)\n', length(CurrentObs), NumClipped);
                fprintf('  RMS: %.4f\n', StageRMS);
            end
        end

        function [Obj, MCMCResult] = fitMCMC(Obj, InputValues, ObservedValues, Args)
            % MCMC sampling for parameter uncertainty estimation
            % Description: Uses mcmcstat package to sample the posterior
            %              distribution of transmission model parameters.
            % Input  : - Obj - CompositeFun object with model setup.
            %          - InputValues - Wavelength grid [N_wvl x 1].
            %          - ObservedValues - Observed flux values [N_obs x 1].
            %          * ...,key,val,...
            %            'CostArgs' - Cell array of costFun arguments. Default is {}.
            %            'X' - X coordinates for position correction. Default is [].
            %            'Y' - Y coordinates for position correction. Default is [].
            %            'IncludeTran2D' - Include Tran2D in sampling. Default is false.
            %            'Nsimu' - Number of MCMC samples. Default is 10000.
            %            'BurnIn' - Burn-in samples to discard. Default is [] (20%).
            %            'Method' - MCMC method: 'dram', 'am', 'dr', 'mh', 'ram'.
            %                   Default is 'dram' (Delayed Rejection Adaptive Metropolis).
            %            'InitFromFit' - Initialize from lsqnonlin fit. Default is true.
            %            'AdaptInt' - Adaptation interval. Default is 100.
            %            'Verbosity' - mcmcstat verbosity (0-2). Default is 1.
            %            'WaitBar' - Show progress waitbar. Default is false.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - Obj - Updated CompositeFun with median posterior parameters.
            %          - MCMCResult - Structure with fields:
            %                   .Chain - MCMC chain [Nsimu x Nparams]
            %                   .Results - mcmcstat results structure
            %                   .ParamNames - Parameter names
            %                   .Median - Median values
            %                   .Std - Standard deviations
            %                   .CI95 - 95% credible intervals [Nparams x 2]
            %                   .AcceptRate - Acceptance rate
            %                   .Tau - Integrated autocorrelation times
            % Author : D. Kovaleva (Jan 2026)
            % Reference: mcmcstat toolbox by Marko Laine (2003)
            % Example: [Model, MCMCRes] = Model.fitMCMC(Lambda, ObsFlux, ...
            %              'CostArgs', CostArgs, 'X', X, 'Y', Y, 'Nsimu', 20000);

            arguments
                Obj
                InputValues
                ObservedValues
                Args.CostArgs cell = {}
                Args.X = []
                Args.Y = []
                Args.IncludeTran2D logical = false
                Args.Nsimu = 10000
                Args.BurnIn = []
                Args.Method = 'dram'
                Args.InitFromFit logical = true
                Args.AdaptInt = 100
                Args.Verbosity = 1
                Args.WaitBar logical = false
                Args.Verbose logical = true
            end

            if Args.Verbose
                fprintf('\n=== MCMC PARAMETER SAMPLING ===\n');
                fprintf('Method: %s, Samples: %d\n', Args.Method, Args.Nsimu);
            end

            % ================================================================
            % STEP 1: INITIALIZE FROM LEAST-SQUARES FIT (optional)
            % ================================================================

            if Args.InitFromFit
                if Args.Verbose
                    fprintf('Initializing from least-squares fit...\n');
                end

                % Run a quick fit to get good starting point
                [Obj, ~] = Obj.fitPar(InputValues, ObservedValues, ...
                    'CostArgs', Args.CostArgs, 'X', Args.X, 'Y', Args.Y, ...
                    'FitTransmission', true, 'FitPosition', false, ...
                    'SigmaClip', false, 'Verbose', false);
            end

            % ================================================================
            % STEP 2: BUILD MCMC STRUCTURES
            % ================================================================

            % Build parameter array for mcmcstat
            Params = Obj.buildMCMCParams('IncludeTran2D', Args.IncludeTran2D);

            if Args.Verbose
                fprintf('Free parameters: %d\n', length(Params));
                for I = 1:min(length(Params), 5)
                    fprintf('  %s: %.4g [%.4g, %.4g]\n', ...
                        Params{I}{1}, Params{I}{2}, Params{I}{3}, Params{I}{4});
                end
                if length(Params) > 5
                    fprintf('  ... and %d more\n', length(Params) - 5);
                end
            end

            % Build model structure
            Model = Obj.buildMCMCModel(InputValues, ObservedValues, ...
                'CostArgs', Args.CostArgs, 'X', Args.X, 'Y', Args.Y, ...
                'IncludeTran2D', Args.IncludeTran2D);

            % ================================================================
            % STEP 3: CONFIGURE MCMC OPTIONS
            % ================================================================

            Options.nsimu = Args.Nsimu;
            Options.method = Args.Method;
            Options.adaptint = Args.AdaptInt;
            Options.verbosity = Args.Verbosity;
            Options.waitbar = Args.WaitBar;

            % ================================================================
            % STEP 4: RUN MCMC
            % ================================================================

            if Args.Verbose
                fprintf('Running MCMC sampling...\n');
                TicStart = tic;
            end

            % Call mcmcstat
            % Model contains only valid mcmcstat fields (ssfun, N)
            % Data argument is ignored by our ssfun (uses closure), pass empty struct
            Data = struct();
            [Results, Chain, S2Chain, SSChain] = mcmcrun(Model, Data, Params, Options);

            if Args.Verbose
                ElapsedTime = toc(TicStart);
                fprintf('MCMC completed in %.1f seconds\n', ElapsedTime);
                fprintf('Acceptance rate: %.1f%%\n', (1 - Results.rejected) * 100);
            end

            % ================================================================
            % STEP 5: PROCESS RESULTS
            % ================================================================

            % Determine burn-in
            if isempty(Args.BurnIn)
                BurnIn = floor(Args.Nsimu / 5);  % Default: discard first 20%
            else
                BurnIn = Args.BurnIn;
            end

            % Trim chain
            ChainTrimmed = Chain(BurnIn+1:end, :);
            NumSamples = size(ChainTrimmed, 1);
            NumParams = size(ChainTrimmed, 2);

            % Compute statistics
            MedianVals = median(ChainTrimmed, 1)';
            StdVals = std(ChainTrimmed, 0, 1)';
            CI95 = prctile(ChainTrimmed, [2.5, 97.5], 1)';

            % Integrated autocorrelation time (if available in mcmcstat)
            try
                TauVals = iact(ChainTrimmed);
            catch
                TauVals = nan(NumParams, 1);
            end

            % ================================================================
            % STEP 6: UPDATE MODEL WITH MEDIAN PARAMETERS
            % ================================================================

            Obj.setFreeParamVector(MedianVals, 'IncludeTran2D', Args.IncludeTran2D);

            % ================================================================
            % STEP 7: BUILD OUTPUT STRUCTURE
            % ================================================================

            MCMCResult.Chain = ChainTrimmed;
            MCMCResult.FullChain = Chain;
            MCMCResult.S2Chain = S2Chain;
            MCMCResult.SSChain = SSChain;
            MCMCResult.Results = Results;
            MCMCResult.ParamNames = Results.names;
            MCMCResult.Median = MedianVals;
            MCMCResult.Std = StdVals;
            MCMCResult.CI95 = CI95;
            MCMCResult.AcceptRate = 1 - Results.rejected;
            MCMCResult.Tau = TauVals;
            MCMCResult.BurnIn = BurnIn;
            MCMCResult.Nsimu = Args.Nsimu;
            MCMCResult.NumSamples = NumSamples;

            % ================================================================
            % STEP 8: PRINT SUMMARY
            % ================================================================

            if Args.Verbose
                fprintf('\n--- MCMC Parameter Summary ---\n');
                fprintf('%-20s %12s %12s %12s %12s\n', 'Parameter', 'Median', 'Std', 'CI95_low', 'CI95_high');
                fprintf('%s\n', repmat('-', 1, 70));
                for I = 1:NumParams
                    fprintf('%-20s %12.4g %12.4g %12.4g %12.4g\n', ...
                        Results.names{I}, MedianVals(I), StdVals(I), CI95(I,1), CI95(I,2));
                end
                fprintf('%s\n', repmat('-', 1, 70));
                fprintf('Effective samples: %.0f (tau_mean=%.1f)\n', ...
                    NumSamples / nanmean(TauVals), nanmean(TauVals));
            end
        end

        function [Obj, FitResult] = fitMultiStage(Obj, InputValues, ObservedValues, Args)
            % Multi-stage optimization wrapper for fitPar
            % Loops through OptimizationSequence and calls single-stage fitting for each stage
            %
            % OptimizationSequence format (same as transmissionFit1):
            %   OptSeq(i).StageName - Name of the stage
            %   OptSeq(i).FreeParams - Struct array with .Function and .Parameter fields
            %                          Empty [] for field correction stage
            %   OptSeq(i).SigmaClip - Enable sigma clipping for this stage
            %   OptSeq(i).SigmaThresh - Threshold for sigma clipping
            %   OptSeq(i).SigmaIter - Number of sigma clipping iterations
            %   OptSeq(i).SigmaClipMethod - 'median' or 'weighted' (defaults to Args.SigmaClipMethod)
            %   OptSeq(i).MinCalibrators - Min calibrators to keep (defaults to Args.MinCalibrators)
            %   OptSeq(i).Description - Description of the stage
            % Author : D. Kovaleva (Nov 2025)

            % Use stored Obj.OptSeq directly (already set by fitPar)
            Stages = Obj.OptSeq;
            NumStages = length(Stages);

            % Initialize results array
            FitResult = struct('StageName', {}, 'Method', {}, 'Cost', {}, 'RMS', {}, ...
                           'Residuals', {}, 'NCalUsed', {}, 'NumClipped', {}, 'KeepMask', {}, ...
                           'IsFieldCorrection', {}, 'Chi2', {}, 'DOF', {});

            % Current data (will be updated after sigma clipping in each stage)
            CurrentObs = ObservedValues(:);
            CurrentX = Args.X(:);
            CurrentY = Args.Y(:);
            CurrentCostArgs = Args.CostArgs;

            % Track cumulative KeepMask across all stages (relative to original observations)
            NCalUsedInitial = length(ObservedValues);
            GlobalKeepMask = true(NCalUsedInitial, 1);
            CurrentIndices = (1:NCalUsedInitial)';

            % Setup optimization options once for all stages (avoid repeated optimoptions calls)
            if isempty(Args.OptimOptions)
                if Args.Verbose
                    DisplayOpt = 'iter';
                else
                    DisplayOpt = 'off';
                end
                OptimOpts = optimoptions('lsqnonlin', 'Display', DisplayOpt, ...
                    'MaxIterations', 1000, 'FunctionTolerance', 1e-8);
            else
                OptimOpts = Args.OptimOptions;
            end

            if Args.Verbose
                fprintf('\n=== MULTI-STAGE OPTIMIZATION ===\n');
                fprintf('Number of stages: %d\n', NumStages);
                fprintf('Initial observations: %d\n\n', length(CurrentObs));
            end

            % Loop through optimization stages
            for IStage = 1:NumStages
                Stage = Stages(IStage);
                StageName = Stage.StageName;
                FreeParamsStage = Stage.FreeParams;
                SigmaClip = Stage.SigmaClip;
                SigmaThresh = Stage.SigmaThresh;
                SigmaIter = Stage.SigmaIter;
                % SigmaClipMethod: use stage-specific value if set, otherwise use Args
                if isfield(Stage, 'SigmaClipMethod')
                    SigmaClipMethod = Stage.SigmaClipMethod;
                else
                    SigmaClipMethod = Args.SigmaClipMethod;
                end
                % MinCalibrators: use stage-specific value if set, otherwise use Args
                if isfield(Stage, 'MinCalibrators')
                    MinCalibrators = Stage.MinCalibrators;
                else
                    MinCalibrators = Args.MinCalibrators;
                end

                % Detect field correction stage (empty freeparams)
                IsFieldCorrectionStage = isempty(FreeParamsStage);

                % Detect Norm-only linear stage (analytical solution)
                IsNormOnlyLinear = false;
                if ~IsFieldCorrectionStage && isfield(Stage, 'Method') && strcmp(Stage.Method, 'linear')
                    % Check if only Norm parameter is being fitted
                    if length(FreeParamsStage) == 1 && strcmp(FreeParamsStage(1).Parameter, 'Norm')
                        IsNormOnlyLinear = true;
                    end
                end

                if IsFieldCorrectionStage || IsNormOnlyLinear
                    Method = 'linear';
                else
                    Method = 'nonlinear';
                end

                if Args.Verbose
                    fprintf('=== Stage %d/%d: %s [%s] ===\n', IStage, NumStages, StageName, Method);
                    fprintf('Description: %s\n', Stage.Description);
                end

                if IsNormOnlyLinear
                    % =============================================================
                    % NORM-ONLY LINEAR STAGE: Analytical solution
                    % =============================================================
                    % Norm_opt = 10^(-mean(residuals_with_Norm1 / 2.5))
                    % where residuals_with_Norm1 = 2.5 * log10(PredBase / Obs)

                    if Args.Verbose
                        fprintf('  Using analytical solution for Norm parameter\n');
                    end

                    % Get current Norm parameter index
                    AllFunPar = Obj.getAllFunPar();
                    NormIdx = find(strcmp(AllFunPar.Name, 'Norm'), 1);

                    % Sigma clipping loop for Norm-only stage
                    % Loop structure: initial fit, then [clip → refit] × SigmaIter
                    %   Residuals = Residuals_base - MeanResidual
                 
                    NumIterNorm = 1 + SigmaClip * SigmaIter;
                    CurrentObsNorm = CurrentObs;
                    CurrentXNorm = CurrentX;
                    CurrentYNorm = CurrentY;
                    CurrentCostArgsNorm = CurrentCostArgs;
                    KeepMaskNorm = true(length(CurrentObs), 1);
                    ConvergedNorm = false;

                    for IterNorm = 1:NumIterNorm
                        if ~ConvergedNorm

                            % SIGMA CLIPPING (skip first iteration — no residuals yet)
                            if SigmaClip && IterNorm > 1
                                % Skip clipping if already below MinCalibrators
                                if MinCalibrators > 0 && length(CurrentObsNorm) <= MinCalibrators
                                    ConvergedNorm = true;
                                    if Args.Verbose
                                        fprintf('  Sigma clipping skipped: %d calibrators <= %d minimum\n', ...
                                            length(CurrentObsNorm), MinCalibrators);
                                    end
                                end
                            end

                            if SigmaClip && IterNorm > 1 && ~ConvergedNorm
                                if Args.Verbose
                                    fprintf('  Norm sigma clipping iteration %d/%d\n', IterNorm-1, SigmaIter);
                                end

                                % Sigma clipping via helper function
                                [OutlierMask, ClipInfo] = tools.math.stat.sigmaClip(...
                                    Residuals, SigmaThresh, ...
                                    'Method', SigmaClipMethod, 'Errors', MagErr_base);

                                if ~ClipInfo.Success
                                    ConvergedNorm = true;
                                    Obj.addStatus('fitMultiStage', 'warning', ...
                                        sprintf('Norm sigma clipping failed: %s', ClipInfo.ErrorMsg), ...
                                        'CompositeFun:SigmaClipFailed');
                                    if Args.Verbose
                                        fprintf('  Sigma clipping failed: %s; skipping\n', ClipInfo.ErrorMsg);
                                    end
                                end
                            end

                            if SigmaClip && IterNorm > 1 && ~ConvergedNorm && ClipInfo.Success
                                NumOutliers = ClipInfo.NumOutliers;

                                if Args.Verbose
                                    fprintf('  Sigma clipping (%s): threshold=%.1f, outliers=%d\n', ...
                                        SigmaClipMethod, SigmaThresh, NumOutliers);
                                end

                                % MinCalibrators safeguard
                                NRemaining = sum(~OutlierMask);
                                SafeguardTriggered = NumOutliers > 0 && MinCalibrators > 0 ...
                                    && NRemaining < MinCalibrators;

                                if SafeguardTriggered
                                    ConvergedNorm = true;
                                    if Args.Verbose
                                        fprintf('  Sigma clipping stopped: would leave %d < %d calibrators\n', ...
                                            NRemaining, MinCalibrators);
                                    end
                                elseif NumOutliers == 0
                                    ConvergedNorm = true;
                                    if Args.Verbose
                                        fprintf('  Norm sigma clipping converged (no outliers)\n');
                                    end
                                else
                                    CurrentKeep = ~OutlierMask;
                                    KeepMaskNorm(KeepMaskNorm) = CurrentKeep;

                                    CurrentObsNorm = CurrentObsNorm(CurrentKeep);
                                    if ~isempty(CurrentXNorm)
                                        CurrentXNorm = CurrentXNorm(CurrentKeep);
                                        CurrentYNorm = CurrentYNorm(CurrentKeep);
                                    end

                                    Idx = find(strcmp(CurrentCostArgsNorm(1:2:end), 'WeightMatrix'));
                                    if ~isempty(Idx)
                                        CurrentCostArgsNorm{2*Idx} = CurrentCostArgsNorm{2*Idx}(:, CurrentKeep);
                                    end
                                    Idx = find(strcmp(CurrentCostArgsNorm(1:2:end), 'PrecomputedMagErr'));
                                    if ~isempty(Idx) && ~isempty(CurrentCostArgsNorm{2*Idx})
                                        CurrentCostArgsNorm{2*Idx} = CurrentCostArgsNorm{2*Idx}(CurrentKeep);
                                    end
                                    Idx = find(strcmp(CurrentCostArgsNorm(1:2:end), 'PrecomputedSpecFluxMatrix'));
                                    if ~isempty(Idx) && ~isempty(CurrentCostArgsNorm{2*Idx})
                                        CurrentCostArgsNorm{2*Idx} = CurrentCostArgsNorm{2*Idx}(:, CurrentKeep);
                                    end

                                    if Args.Verbose
                                        fprintf('  Clipped %d outliers (%.1f sigma)\n', NumOutliers, SigmaThresh);
                                    end
                                end
                            end

                            % FIT NORM AND COMPUTE RESIDUALS (skip if just converged)
                            if ~ConvergedNorm
                                % Set Norm=1 temporarily to get base residuals
                                AllFunPar.Val(NormIdx) = 1.0;
                                Obj.setAllFunPar(AllFunPar);

                                if ~isempty(CurrentXNorm)
                                    [~, ~, ~, Residuals_base, MagErr_base] = Obj.costFun(InputValues, CurrentObsNorm, ...
                                        CurrentCostArgsNorm{:}, 'X', CurrentXNorm, 'Y', CurrentYNorm);
                                else
                                    [~, ~, ~, Residuals_base, MagErr_base] = Obj.costFun(InputValues, CurrentObsNorm, ...
                                        CurrentCostArgsNorm{:});
                                end

                                % Analytical solution: weighted mean of base residuals
                                if ~isempty(MagErr_base) && all(MagErr_base > 0)
                                    Weights = 1 ./ (MagErr_base.^2);
                                    MeanResidual = sum(Residuals_base .* Weights) / sum(Weights);
                                else
                                    MeanResidual = mean(Residuals_base);
                                end
                                Norm_opt = 10^(-MeanResidual / 2.5);

                                % Set optimal Norm in model
                                AllFunPar.Val(NormIdx) = Norm_opt;
                                Obj.setAllFunPar(AllFunPar);

                                % Residuals with optimal Norm (analytical, exact):
                                % mag(Norm_opt) - mag(1) = -2.5*log10(Norm_opt) = MeanResidual
                                % so Residuals_final = Residuals_base - MeanResidual
                                Residuals = Residuals_base - MeanResidual;

                                if Args.Verbose
                                    fprintf('  Norm = %.6f (analytical, weighted)\n', Norm_opt);
                                end
                            end
                        end
                    end

                    % Build StageResult structure
                    StageRMS = std(Residuals);
                    NumClipped = sum(~KeepMaskNorm);

                    % Weighted residuals for diagnostics
                    if ~isempty(MagErr_base) && all(MagErr_base > 0)
                        WeightedRes = Residuals ./ MagErr_base;
                        StageChi2 = sum(WeightedRes.^2);
                    else
                        WeightedRes = [];
                        StageChi2 = sum(Residuals.^2);
                    end

                    StageResult = struct();
                    StageResult.Cost = sum(Residuals.^2);
                    StageResult.RMS = StageRMS;
                    StageResult.Residuals = Residuals;
                    StageResult.WeightedResiduals = WeightedRes;
                    StageResult.NCalUsed = length(CurrentObsNorm);
                    StageResult.NumClipped = NumClipped;
                    StageResult.KeepMask = KeepMaskNorm;
                    StageResult.ConvergedSigmaClip = ConvergedNorm;
                    StageResult.Chi2 = StageChi2;
                    StageResult.DOF = length(Residuals) - 1;  % 1 free parameter (Norm)
                    StageResult.MagErr = MagErr_base;
                    StageResult.PredictedFlux = [];  % Not computed (would require extra costFun call)

                    if Args.Verbose
                        fprintf('  RMS: %.4f mag, Observations: %d\n', StageRMS, length(CurrentObsNorm));
                    end

                elseif IsFieldCorrectionStage
                    % Field correction stage: fit position only
                    [Obj, StageResult] = Obj.fitPar(InputValues, CurrentObs, ...
                        'CostArgs', CurrentCostArgs, ...
                        'X', CurrentX, 'Y', CurrentY, ...
                        'FitTransmission', false, ...
                        'FitPosition', true, ...
                        'SigmaClip', SigmaClip, ...
                        'SigmaThresh', SigmaThresh, ...
                        'SigmaIter', SigmaIter, ...
                        'SigmaClipMethod', SigmaClipMethod, ...
                        'MinCalibrators', MinCalibrators, ...
                        'OptimizationSequence', Stages(IStage), ...
                        'OptimOptions', OptimOpts, ...
                        'Verbose', Args.Verbose);
                else
                    % Transmission parameter stage: set FitPar flags for specified parameters
                    AllFunPar = Obj.getAllFunPar();
                    AllFunPar.FitPar(:) = false;  % Reset all to false

                    % Set FitPar for parameters specified in this stage
                    StageHasError = false;
                    for I = 1:length(FreeParamsStage)
                        if ~StageHasError
                            FunctionName = FreeParamsStage(I).Function;
                            ParameterName = FreeParamsStage(I).Parameter;
                            Idx = find(strcmp(AllFunPar.Name, ParameterName), 1);
                            if isempty(Idx)
                                Obj.addStatus('fitMultiStage', 'error', sprintf('Parameter "%s" (from function "%s") not found in Model', ParameterName, FunctionName), 'CompositeFun:ParameterNotFound');
                                StageHasError = true;
                            end
                            if ~StageHasError
                                AllFunPar.FitPar(Idx) = true;
                            end
                        end
                    end

                    % If parameter lookup failed, create failure result; otherwise fit
                    if StageHasError
                        StageResult = struct();
                        StageResult.Cost = Inf;
                        StageResult.RMS = NaN;
                        StageResult.Residuals = [];
                        StageResult.WeightedResiduals = [];
                        StageResult.NCalUsed = length(CurrentObs);
                        StageResult.NumClipped = 0;
                        StageResult.KeepMask = true(length(CurrentObs), 1);
                        StageResult.ConvergedSigmaClip = false;
                        StageResult.Chi2 = NaN;
                        StageResult.DOF = NaN;
                        StageResult.MagErr = [];
                        StageResult.PredictedFlux = [];
                    else
                        % Apply FitPar flags
                        Obj.setAllFunPar(AllFunPar);

                        % Fit transmission parameters
                        [Obj, StageResult] = Obj.fitPar(InputValues, CurrentObs, ...
                            'CostArgs', CurrentCostArgs, ...
                            'X', CurrentX, 'Y', CurrentY, ...
                            'FitTransmission', true, ...
                            'FitPosition', false, ...
                            'SigmaClip', SigmaClip, ...
                            'SigmaThresh', SigmaThresh, ...
                            'SigmaIter', SigmaIter, ...
                            'SigmaClipMethod', SigmaClipMethod, ...
                            'MinCalibrators', MinCalibrators, ...
                            'OptimizationSequence', Stages(IStage), ...
                            'OptimOptions', OptimOpts, ...
                            'Verbose', Args.Verbose);
                    end
                end

                % Update current data after sigma clipping using KeepMask from fitPar
                if StageResult.NumClipped > 0
                    % Use KeepMask directly from fitPar result (no recomputation needed)
                    StageKeepMask = StageResult.KeepMask;

                    % Update GlobalKeepMask at original indices
                    ClippedInStage = ~StageKeepMask;
                    GlobalKeepMask(CurrentIndices(ClippedInStage)) = false;
                    CurrentIndices = CurrentIndices(StageKeepMask);

                    if Args.Verbose
                        fprintf('  Propagating clipped data: keeping %d/%d observations\n', ...
                                sum(StageKeepMask), length(StageKeepMask));
                    end

                    % Update datasets for next stage
                    CurrentObs = CurrentObs(StageKeepMask);
                    if ~isempty(CurrentX)
                        CurrentX = CurrentX(StageKeepMask);
                        CurrentY = CurrentY(StageKeepMask);
                    end

                    % Update WeightMatrix, PrecomputedMagErr, and PrecomputedSpecFluxMatrix if present (for TransmissionMode)
                    Idx = find(strcmp(CurrentCostArgs(1:2:end), 'WeightMatrix'));
                    if ~isempty(Idx)
                        CurrentCostArgs{2*Idx} = CurrentCostArgs{2*Idx}(:, StageKeepMask);
                    end
                    Idx = find(strcmp(CurrentCostArgs(1:2:end), 'PrecomputedMagErr'));
                    if ~isempty(Idx) && ~isempty(CurrentCostArgs{2*Idx})
                        CurrentCostArgs{2*Idx} = CurrentCostArgs{2*Idx}(StageKeepMask);
                    end
                    Idx = find(strcmp(CurrentCostArgs(1:2:end), 'PrecomputedSpecFluxMatrix'));
                    if ~isempty(Idx) && ~isempty(CurrentCostArgs{2*Idx})
                        CurrentCostArgs{2*Idx} = CurrentCostArgs{2*Idx}(:, StageKeepMask);
                    end
                end

                % Store stage results (after updating GlobalKeepMask)
                FitResult(IStage).StageName = StageName;
                FitResult(IStage).Method = Method;
                FitResult(IStage).Cost = StageResult.Cost;
                FitResult(IStage).RMS = StageResult.RMS;
                FitResult(IStage).Residuals = StageResult.Residuals;
                FitResult(IStage).WeightedResiduals = StageResult.WeightedResiduals;  % For weighted fitting reference
                FitResult(IStage).NCalUsed = StageResult.NCalUsed;
                FitResult(IStage).NumClipped = NCalUsedInitial - sum(GlobalKeepMask);  % Cumulative clipped
                FitResult(IStage).KeepMask = GlobalKeepMask;  % Cumulative mask relative to original
                FitResult(IStage).IsFieldCorrection = IsFieldCorrectionStage;
                FitResult(IStage).Chi2 = StageResult.Chi2;
                FitResult(IStage).DOF = StageResult.DOF;
                FitResult(IStage).MagErr = StageResult.MagErr;  % Magnitude errors from error propagation
                FitResult(IStage).PredictedFlux = StageResult.PredictedFlux;  % Model-predicted flux

                if Args.Verbose
                    fprintf('Stage complete: RMS=%.4f mag, NCalUsed=%d\n', ...
                            StageResult.RMS, StageResult.NCalUsed);
                    fprintf('\n');
                end
            end

            if Args.Verbose
                fprintf('Transmission optimization complete\n\n');
            end
        end
    end

end