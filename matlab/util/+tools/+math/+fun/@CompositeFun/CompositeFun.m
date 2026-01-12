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
       % Simple direct comparison (NumInput == NumObs)
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
               Result.RMS, Result.NumObs, length(ObsFlux));
    
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
       fprintf('Stage 1 (Aerosol):     RMS=%.4f mag, NumObs=%d\n', ...
               FitResult(1).RMS, FitResult(1).NumObs);
       fprintf('Stage 2 (Field Corr):  RMS=%.4f mag, NumObs=%d\n', ...
               FitResult(2).RMS, FitResult(2).NumObs);
    
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

        % Fit quality metrics (set by fitPar)
        RMS = NaN                % RMS of residuals from last fit
        Chi2 = NaN               % Chi-squared value from last fit
        DOF = NaN                % Degrees of freedom from last fit
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

                % Validate Params is numeric array
                if ~isnumeric(Params)
                    error('Parameters for function %s must be numeric array', FunName);
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

                % Validate that params and paraminfo have matching lengths
                if NumParams ~= length(ParamInfo)
                    error('Function "%s" has %d params but %d paraminfo entries. These must match.', ...
                          FunName, NumParams, length(ParamInfo));
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

            % Validate input structure
            if ~isstruct(AllFunPar) || ~isfield(AllFunPar, 'Val') || ~isfield(AllFunPar, 'FitPar')
                error('CompositeFun:setAllFunPar:InvalidInput', 'Input must be structure with Val and FitPar fields');
            end

            NumAllFunPar = Obj.numAllFunPar();

            % Validate sizes
            if length(AllFunPar.Val) ~= NumAllFunPar
                error('CompositeFun:setAllFunPar:ValuesSizeMismatch', ...
                      'Val has %d elements but %d expected', length(AllFunPar.Val), NumAllFunPar);
            end
            if length(AllFunPar.FitPar) ~= NumAllFunPar
                error('CompositeFun:setAllFunPar:FitParSizeMismatch', ...
                      'FitPar has %d elements but %d expected', length(AllFunPar.FitPar), NumAllFunPar);
            end

            % Validate bounds if provided
            UpdateBounds = false;
            if isfield(AllFunPar, 'Min') && isfield(AllFunPar, 'Max')
                if length(AllFunPar.Min) ~= NumAllFunPar
                    error('CompositeFun:setAllFunPar:MinSizeMismatch', ...
                          'Min has %d elements but %d expected', length(AllFunPar.Min), NumAllFunPar);
                end
                if length(AllFunPar.Max) ~= NumAllFunPar
                    error('CompositeFun:setAllFunPar:MaxSizeMismatch', ...
                          'Max has %d elements but %d expected', length(AllFunPar.Max), NumAllFunPar);
                end
                UpdateBounds = true;
            end

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
                        ExistingEntries = [ExistingEntries, {NewEntry}];  % Concatenate instead of dynamic indexing
                        GlobalParamMap(GlobalIndex) = ExistingEntries;
                    else
                        % First time seeing this global parameter
                        GlobalParamMap(GlobalIndex) = {{Ifun, Ipar, ParamValue}};
                    end
                end
            end

            % Check for NaN fixed parameters (informational warning, not error)
            % Use cell array to avoid size-changing warnings
            NaNFixedParamsCell = {};
            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    if ~Obj.Funs(Ifun).FitPar(Ipar) && isnan(Obj.Funs(Ifun).Par(Ipar))
                        GlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                        if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                            ParamName = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                        else
                            ParamName = sprintf('Param_%d', GlobalIndex);
                        end
                        NewEntry = {GlobalIndex, Ifun, Ipar, ParamName, Obj.Funs(Ifun).Desc};
                        NaNFixedParamsCell = [NaNFixedParamsCell, {NewEntry}];
                    end
                end
            end

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

                if isempty(ArgNames) || ~isstruct(ArgNames)
                    error('Function did not return valid ArgNames structure');
                end

                % Validate structure has required fields
                RequiredFields = {'Name', 'Description', 'Min', 'Max'};
                for i = 1:length(RequiredFields)
                    if ~isfield(ArgNames, RequiredFields{i})
                        error('ArgNames missing required field: %s', RequiredFields{i});
                    end
                end

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

            % Validate sizes
            if length(Par) ~= length(FitPar)
                error('CompositeFun:addFun:SizeMismatch', 'Par and FitPar must have the same length');
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
                % Validate ArgNames
                if isempty(CurrentFun.ArgNames)
                    error('CompositeFun:argMapping:MissingArgNames', 'ArgNames is empty for function %s', CurrentFun.Name);
                end
                if Ipar > length(CurrentFun.ArgNames)
                    error('CompositeFun:argMapping:ArgNamesMismatch', 'ArgNames has %d elements but Par has %d elements for function %s', ...
                          length(CurrentFun.ArgNames), NumParams, CurrentFun.Name);
                end

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

            % Validate AllFunPar size if provided
            if ~isempty(AllFunPar)
                ExpectedSize = Obj.numAllFunPar();
                if size(AllFunPar, 2) ~= ExpectedSize
                    error('CompositeFun:evaluate:AllFunParSizeMismatch', ...
                          'AllFunPar has %d columns but %d expected', ...
                          size(AllFunPar, 2), ExpectedSize);
                end
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
                                error('CompositeFun:evaluate:NaNParameters', ...
                                      'Cannot evaluate: some parameters contain NaN values. Use setAllFunParsStruct() to set parameter values first.');
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
            [P_ref, ~] = Obj.Tran2DObj.forward(Coo_ref);

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
                [P_ref_new, ~] = Obj.Tran2DObj.forward(Coo_ref);

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

        function [Residuals, Cost, PredictedValues] = costFun(Obj, InputValues, ObservedValues, Args)
            % General cost function for CompositeFun optimization with optional Tran2D.
            % Evaluates CompositeFun model, compares predictions to observations, calculates residuals.
            %
            % Input  : - Obj - CompositeFun object
            %          - InputValues - Input values for function evaluation (e.g., wavelength grid)
            %                   Column vector [N_input x 1]
            %          - ObservedValues - Observed output values for comparison
            %                   Column vector [N_obs x 1] where N_obs is number of observations
            %          * ...,key,val,...
            %            'TransParams' - Parameter values vector to override Obj parameters
            %                   If empty, uses current parameters from Obj.
            %                   Default is [].
            %            'X' - Source X coordinates [N_obs x 1] for Tran2D corrections
            %                   Required if UseTran2D is true. Default is [].
            %            'Y' - Source Y coordinates [N_obs x 1] for Tran2D corrections
            %                   Required if UseTran2D is true. Default is [].
            %            'WeightMatrix' - Optional weight matrix for observations [N_obs x N_input]
            %                   Used when integrating model output (e.g., spectral weights)
            %                   For transmission mode: calibrator spectra [N_wvl x N_obs]
            %                   (Gaia XP, synthetic, or model spectra)
            %                   If empty, direct comparison is used. Default is [].
            %            'IntegrationDim' - Dimension along which to integrate (1 or 2)
            %                   Used when WeightMatrix is provided. Default is 2.
            %            'TransmissionMode' - Enable transmission-specific calculations
            %                   When true, performs photon conversion and magnitude residuals
            %                   Requires WeightMatrix (calibrator spectra). Default is false.
            %            'CalibWavelength' - Calibrator spectral wavelength grid [N_wvl x 1] in Angstrom
            %                   Works with any calibrator spectra (default: Gaia DR3 XP)
            %                   Used in transmission mode. Default is CompositeFun.SpecWvl.
            %            'ExpTime' - Exposure time [s] for photon conversion
            %                   Used in transmission mode. Default is 20.
            %            'Aperture_area_m2' - Telescope aperture area [m^2]
            %                   Used in transmission mode. Default is pi * (0.1397)^2 (LAST).
            %            'CostType' - Type of cost function:
            %                   'sse' - Sum of squared errors (default)
            %                   'mae' - Mean absolute error
            %                   'rmse' - Root mean squared error
            %                   Default is 'sse'.
            %            'ValInp' - Boolean flag for validation of inputs. Default is true.
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - Residuals - Differences between predicted and observed [N_obs x 1]
            %          - Cost - Scalar cost value (depends on CostType)
            %          - PredictedValues - Model predictions [N_obs x 1]
            % Author : D. Kovaleva (Dec 2025)
            % Example: % Simple 1D function without position corrections
            %          Model = tools.math.fun.CompositeFun.model(FunList);
            %          InputVals = linspace(300, 1110, 10)';
            %          ObsVals = randn(5, 1);  % 5 observations
            %          [Res, Cost, Pred] = Model.costFun(InputVals, ObsVals);
            %          % With Tran2D position corrections
            %          Model = tools.math.fun.CompositeFun.model(FunList, 'UseTran2D', true);
            %          X = [100; 200; 300; 400; 500];  % 5 sources
            %          Y = [100; 200; 300; 400; 500];
            %          [Res, Cost, Pred] = Model.costFun(InputVals, ObsVals, 'X', X, 'Y', Y);
            %          % TransmissionMode with calibrator spectra (photometric calibration)
            %          % This mode integrates transmission×spectrum, converts to photons,
            %          % and returns magnitude residuals: DeltaMag = 2.5*log10(Pred/Obs)
            %          Lambda = (300:2:1100)';   % Transmission wavelength grid [nm], 401 points
            %          SpecWvl = (336:2:1020)';  % Calibrator spectral wavelength grid [nm], 343 points (e.g., Gaia DR3 XP)
            %          % Use calibrator spectra (e.g., Gaia DR3 XP, or synthetic/model spectra)
            %          Spec = randn(343, 3) * 1e-17 + 1e-16;  % Calibrator spectra [343 x 3]
            %          ObsFlux = [1.2e5; 2.5e5; 1.8e5];  % Observed photon counts [3 x 1]
            %          X = [200; 863; 1500];  % Source positions [pixels]
            %          Y = [200; 863; 1500];
            %          [ResMag, Cost, PredFlux] = Model.costFun(Lambda, ObsFlux, ...
            %              'WeightMatrix', Spec, 'TransmissionMode', true, ...
            %              'CalibWavelength', SpecWvl, 'X', X, 'Y', Y, ...
            %              'ExpTime', 20, 'Aperture_area_m2', pi*0.1397^2);
            %          % ResMag are magnitude differences [mag], PredFlux are photons

            arguments
                Obj
                InputValues                           % Input values [N_input x 1]
                ObservedValues                        % Observed values [N_obs x 1]
                Args.TransParams = []                 % Parameter override
                Args.X = []                           % X coordinates [N_obs x 1]
                Args.Y = []                           % Y coordinates [N_obs x 1]
                Args.WeightMatrix = []                % Weight matrix [N_obs x N_input] or calibrator spectra
                Args.IntegrationDim = 2               % Integration dimension
                Args.TransmissionMode logical = false % Enable transmission-specific mode
                Args.CalibWavelength = CompositeFun.SpecWvl  % Calibrator spectra wavelength grid [Angstrom] (default: Gaia DR3 XP)
                Args.ExpTime = 20                     % Exposure time [s]
                Args.Aperture_area_m2 = pi * (0.1397)^2  % LAST aperture [m^2]
                Args.CostType = 'sse'                 % Cost function type
                Args.ValInp logical = true
                Args.Verbose logical = false
            end
            
            
                H = constant.h('SI');      % Planck constant [J·s]
                C = constant.c('SI');      % Speed of light [m/s]

            % ====================================================================
            % STEP 1: VALIDATE INPUTS
            % ====================================================================

            if Args.ValInp
                if ~isa(Obj, 'tools.math.fun.CompositeFun')
                    error('Obj must be a tools.math.fun.CompositeFun object');
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
                        error('TransParams size (%d) does not match Obj parameter count (%d)', ...
                              length(TransParams), ExpectedNumParams);
                    end
                end
                if Args.Verbose
                    fprintf('Using provided TransParams (%d parameters)\n', length(TransParams));
                end
            end

            if Args.ValInp
                if any(isnan(TransParams))
                    error('TransParams contains NaN');
                end
            end

            % Ensure column vectors
            InputValues = InputValues(:);
            ObservedValues = ObservedValues(:);

            NumObs = length(ObservedValues);
            NumInput = length(InputValues);

            if Args.ValInp
                if NumObs == 0
                    error('ObservedValues is empty');
                end
                if NumInput == 0
                    error('InputValues is empty');
                end

                % Validate X, Y if using Tran2D and coordinates are provided
                if Obj.UseTran2D && ~isempty(Args.X) && ~isempty(Args.Y)
                    if length(Args.X) ~= NumObs
                        error('X size (%d) does not match number of observations (%d)', length(Args.X), NumObs);
                    end
                    if length(Args.Y) ~= NumObs
                        error('Y size (%d) does not match number of observations (%d)', length(Args.Y), NumObs);
                    end
                end

                % Validate WeightMatrix if provided (skip for TransmissionMode - different dimensions)
                if ~isempty(Args.WeightMatrix) && ~Args.TransmissionMode
                    [WRows, WCols] = size(Args.WeightMatrix);
                    if WRows ~= NumObs || WCols ~= NumInput
                        error('WeightMatrix size [%d x %d] must match [NumObs=%d x NumInput=%d]', ...
                              WRows, WCols, NumObs, NumInput);
                    end
                end
            end

            if Args.Verbose
                fprintf('=== COMPOSITEFUN COST FUNCTION ===\n');
                fprintf('Number of observations: %d\n', NumObs);
                fprintf('Number of input points: %d\n', NumInput);
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
                    error('TransmissionMode requires WeightMatrix (spectra)');
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

                % WeightMatrix = calibrator spectra [N_SpecWvl x N_obs]
                Spec = Args.WeightMatrix;
                SpecWvl = Args.CalibWavelength(:);

                % Determine integration range from InputValues (Lambda)
                Lambda_min = min(InputValues);
                Lambda_max = max(InputValues);

                % Find spectral wavelengths within Lambda range
                SpecInRange = (SpecWvl >= Lambda_min) & (SpecWvl <= Lambda_max);
                SpecWvl_InRange = SpecWvl(SpecInRange);

                % Build integration wavelength grid (with extrapolation if needed)
                SpecWvl_Integration = SpecWvl_InRange;
                NeedExtrapolationBelow = Lambda_min < SpecWvl(1);
                NeedExtrapolationAbove = Lambda_max > SpecWvl(end);

                if NeedExtrapolationBelow
                    SpecWvl_Integration = [Lambda_min; SpecWvl_Integration];
                end
                if NeedExtrapolationAbove
                    SpecWvl_Integration = [SpecWvl_Integration; Lambda_max];
                end

                % Interpolate transmission onto integration grid
                if UsePositionCorrections
                    % ModelOutput is [N_obs x N_lambda]
                    % Transpose to [N_lambda x N_obs], interpolate, transpose back
                    Transmission_Spec = interp1(InputValues, ModelOutput', SpecWvl_Integration, 'linear');
                    % Result: [N_integration_points x N_obs]
                else
                    % ModelOutput is [N_lambda x 1]
                    Transmission_Spec = interp1(InputValues, ModelOutput, SpecWvl_Integration, 'linear');
                    % Result: [N_integration_points x 1], broadcasts to all calibrators
                end

                % Extract and extrapolate calibrator spectra for integration range
                SpecFluxMatrix = Spec(SpecInRange, :);  % [NumInRange x N_obs]

                if NeedExtrapolationBelow
                    FirstRow = Spec(1, :);
                    SpecFluxMatrix = [FirstRow; SpecFluxMatrix];
                end
                if NeedExtrapolationAbove
                    LastRow = Spec(end, :);
                    SpecFluxMatrix = [SpecFluxMatrix; LastRow];
                end
                % Now: [N_integration_points x N_obs]

                % Apply transmission to all spectra
                TransmittedSpectra = SpecFluxMatrix .* Transmission_Spec;  % [N_integration_points x N_obs]

                % Integrate: ∫ Flux(λ) × Transmission(λ) × λ dλ
                % NOTE: Gaia flux is in W/m²/nm, so λ must be in nm for dimensional consistency
                TransmittedSpectraT = TransmittedSpectra';  % [N_obs x N_integration_points]
                SpecWvl_nm = SpecWvl_Integration / 10;  % Convert Angstrom to nm (Gaia flux is per nm)
                Integrand = TransmittedSpectraT .* SpecWvl_nm(:)';
                % Integration still uses Angstrom grid (physical step size unchanged)
                A_vector = tools.math.integral.trapzmat(SpecWvl_Integration(:)', Integrand, 2);
                A_vector = A_vector(:);  % [N_obs x 1]

                % Convert to photons
                B = H * C * 1e10;          % H*C with Angstrom to m conversion (1 Angstrom = 1e-10 m)

                Dt = Args.ExpTime;
                Ageom = Args.Aperture_area_m2;

                PredictedFlux_photons = Dt * Ageom * A_vector / B;  % [N_obs x 1]

                % Calculate magnitude difference
                % Formula: 2.5 * log10(Predicted/Observed) = Mag_obs - Mag_pred
                % Residual convention: Observed - Predicted (in magnitude space)
                % Positive when Mag_obs > Mag_pred → model too bright (in flux space)
                DiffMag = 2.5 * log10(PredictedFlux_photons ./ ObservedValues);

                % For transmission mode, residuals are magnitude differences
                Residuals = DiffMag;
                PredictedValues = PredictedFlux_photons;

                if Args.Verbose
                    fprintf('Transmission mode: integrated over %d wavelength points\n', length(SpecWvl_Integration));
                    fprintf('Predicted flux range: %.2e - %.2e photons\n', min(PredictedFlux_photons), max(PredictedFlux_photons));
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
                    if NumInput == NumObs
                        PredictedValues = ModelOutput;
                    else
                        error('Direct comparison requires NumInput (%d) == NumObs (%d) or use WeightMatrix', ...
                              NumInput, NumObs);
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
                    error('Unknown CostType: %s', Args.CostType);
            end

            if Args.Verbose
                fprintf('Residuals: mean=%.4e, std=%.4e\n', mean(Residuals), std(Residuals));
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
            %                   .Description - Description of the stage
            %                   Default is [] (single-stage mode if Obj.OptSeq is also empty).
            %            'ValInp' - Boolean flag for validation of inputs and setup. Default is true.
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - Obj - Updated CompositeFun object with fitted parameters
            %                   Also sets Obj.Chi2 and Obj.DOF from the final fit.
            %          - FitResult - Structure with fields:
            %                   Single-stage mode:
            %                     .Cost - Final cost value
            %                     .RMS - RMS of residuals from lsqNonLinWithFixed
            %                     .Residuals - Final residuals [N_obs x 1]
            %                     .NumObs - Number of observations after clipping
            %                     .NumClipped - Number of clipped outliers
            %                     .KeepMask - Logical mask [N_obs_initial x 1] of surviving observations
            %                     .ConvergedSigmaClip - True if sigma clipping converged
            %                     .Chi2 - Chi-squared from lsqNonLinWithFixed
            %                     .DOF - Degrees of freedom from lsqNonLinWithFixed
            %                   Multi-stage mode: Array of structs with per-stage results
            %                     FitResult(i).StageName, .Method, .Cost, .RMS, .Residuals,
            %                     .NumObs, .NumClipped, .IsFieldCorrection, .Chi2, .DOF
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
                Args.OptimOptions = []
                Args.OptimizationSequence = []  % Multi-stage optimization sequence
                Args.ValInp logical = true
                Args.Verbose logical = false
            end

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

            NumObsInitial = length(ObservedValues);

            if Args.ValInp
                if Obj.UseTran2D && Args.FitPosition
                    if isempty(Args.X) || isempty(Args.Y)
                        error('X and Y coordinates required when fitting position parameters');
                    end
                    if length(Args.X) ~= NumObsInitial || length(Args.Y) ~= NumObsInitial
                        error('X, Y size must match number of observations (%d)', NumObsInitial);
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
                fprintf('Initial observations: %d\n', NumObsInitial);
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
            NumObsInitial = length(ObservedValues);
            KeepMask = true(NumObsInitial, 1);
            CurrentIndices = (1:NumObsInitial)';  % Maps current obs to original indices

            NumIterations = Args.SigmaClip * Args.SigmaIter + ~Args.SigmaClip;
            ConvergedSigmaClip = false;

            for Iter = 1:NumIterations
                if Args.Verbose && Args.SigmaClip
                    fprintf('--- Sigma clipping iteration %d/%d ---\n', Iter, Args.SigmaIter);
                end

                % =============================================================
                % FIT TRANSMISSION PARAMETERS (if requested)
                % =============================================================

                if Args.FitTransmission && ~isempty(FreeParamIndices)
                    if Args.Verbose
                        fprintf('Fitting transmission parameters (nonlinear)...\n');
                    end

                    % Get bounds and current parameter values
                    AllFunPar = Obj.getAllFunPar();
                    CurrentTransParams = AllFunPar.Val;

                    % Setup FitMask for all parameters
                    FitMask = false(size(CurrentTransParams));
                    FitMask(FreeParamIndices) = true;

                    % Model function for lsqNonLinWithFixed
                    % Signature: @(X_dummy, P) -> Residuals
                    % X_dummy is ignored, P is the full parameter vector
                    % Pass P directly to costFun via 'TransParams' argument
                    if ~isempty(CurrentX)
                        ModelFun = @(X_dummy, P) Obj.costFun(InputValues, CurrentObs, ...
                            Args.CostArgs{:}, 'TransParams', P, 'X', CurrentX, 'Y', CurrentY);
                    else
                        ModelFun = @(X_dummy, P) Obj.costFun(InputValues, CurrentObs, ...
                            Args.CostArgs{:}, 'TransParams', P);
                    end

                    % Dummy X (observation indices), Y = 0 (fit residuals to zero), uniform weights
                    NumCurrent = length(CurrentObs);
                    X_dummy = (1:NumCurrent)';
                    Y_target = zeros(NumCurrent, 1);
                    Sigma_weights = ones(NumCurrent, 1);

                    % Call lsqNonLinWithFixed
                    [OptTransParams, ~, MinimizerInfo] = tools.math.fit.lsqNonLinWithFixed(...
                        X_dummy, Y_target, Sigma_weights, ModelFun, ...
                        'InitPar', CurrentTransParams, ...
                        'FitPar', FitMask, ...
                        'Lb', AllFunPar.Min, ...
                        'Ub', AllFunPar.Max, ...
                        'Opts', OptimOpts);

                    % Update Obj with optimized parameters
                    AllFunPar.Val = OptTransParams;
                    Obj.setAllFunPar(AllFunPar);

                    if Args.Verbose
                        fprintf('Transmission optimization complete\n');
                        fprintf('  RMS: %.4f, Chi2: %.4f, DOF: %d, Chi2/DOF: %.4f\n', ...
                                sqrt(sum(MinimizerInfo.Resid.^2) / length(MinimizerInfo.Resid)), ...
                                MinimizerInfo.Chi2, MinimizerInfo.Dof, MinimizerInfo.Chi2/MinimizerInfo.Dof);
                    end
                end

                % =============================================================
                % FIT POSITION PARAMETERS (if requested and Tran2D enabled)
                % =============================================================

                if Args.FitPosition && Obj.UseTran2D
                    if Args.Verbose
                        fprintf('Fitting position parameters (linear)...\n');
                    end

                    % Save current Tran2D parameters
                    SavedParX = Obj.Tran2DObj.ParX;

                    % Zero out position correction to get base residuals
                    Obj.Tran2DObj.ParX = zeros(1, length(SavedParX));

                    % Calculate residuals without position correction
                    if ~isempty(CurrentX)
                        [BaseResiduals, ~, ~] = Obj.costFun(InputValues, CurrentObs, ...
                            Args.CostArgs{:}, 'X', CurrentX, 'Y', CurrentY);
                    else
                        [BaseResiduals, ~, ~] = Obj.costFun(InputValues, CurrentObs, Args.CostArgs{:});
                    end

                    % Restore position parameters
                    Obj.Tran2DObj.ParX = SavedParX;

                    % Fit position polynomial with base residuals
                    % BaseResiduals are magnitude differences (Predicted - Observed)
                    [~, Obj] = Obj.fitPositionPolynomial(CurrentX, CurrentY, BaseResiduals, ...
                        'Verbose', false);

                    if Args.Verbose
                        fprintf('Position optimization complete\n');
                    end
                end

                % =============================================================
                % CALCULATE RESIDUALS AND APPLY SIGMA CLIPPING
                % =============================================================

                % Calculate current residuals with all fitted parameters
                if ~isempty(CurrentX)
                    [Residuals, Cost, ~] = Obj.costFun(InputValues, CurrentObs, ...
                        Args.CostArgs{:}, 'X', CurrentX, 'Y', CurrentY);
                else
                    [Residuals, Cost, ~] = Obj.costFun(InputValues, CurrentObs, Args.CostArgs{:});
                end

                RMS = sqrt(Cost / length(Residuals));

                if Args.Verbose
                    fprintf('Current RMS: %.4f, NumObs: %d\n', RMS, length(Residuals));
                end

                % Apply sigma clipping if enabled
                if Args.SigmaClip
                    % Calculate robust statistics
                    MedianRes = median(Residuals);
                    MAD = median(abs(Residuals - MedianRes));
                    Sigma = 1.4826 * MAD;  % Convert MAD to std estimate

                    % Find outliers
                    OutlierMask = abs(Residuals - MedianRes) > Args.SigmaThresh * Sigma;
                    NumOutliers = sum(OutlierMask);

                    if Args.Verbose
                        fprintf('Sigma clipping: median=%.4f, MAD=%.4f, outliers=%d\n', ...
                                MedianRes, MAD, NumOutliers);
                    end

                    if NumOutliers == 0
                        ConvergedSigmaClip = true;
                        if Args.Verbose
                            fprintf('Sigma clipping converged (no outliers)\n');
                        end
                        break;
                    end

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

                    % Also subset WeightMatrix if present (for TransmissionMode)
                    % Args.CostArgs is a cell array {key1, val1, key2, val2, ...}
                    WeightMatrixIdx = find(strcmp(Args.CostArgs(1:2:end), 'WeightMatrix'));
                    if ~isempty(WeightMatrixIdx)
                        % WeightMatrixIdx is the index in the keys (1:2:end), so actual index is 2*WeightMatrixIdx
                        ActualIdx = 2 * WeightMatrixIdx;
                        % WeightMatrix columns correspond to observations
                        Args.CostArgs{ActualIdx} = Args.CostArgs{ActualIdx}(:, IterKeepMask);
                    end

                    if Args.Verbose
                        fprintf('Removed %d outliers, %d observations remaining\n', ...
                                NumOutliers, length(CurrentObs));
                    end
                else
                    % No sigma clipping, exit after first iteration
                    break;
                end
            end

            % ====================================================================
            % STEP 4: FINALIZE RESULTS
            % ====================================================================

            NumClipped = NumObsInitial - length(CurrentObs);

            % Get quality metrics from minimizer if available
            if exist('MinimizerInfo', 'var')
                Chi2 = MinimizerInfo.Chi2;
                DOF = MinimizerInfo.Dof;
                RMS = sqrt(sum(MinimizerInfo.Resid.^2) / length(MinimizerInfo.Resid));
            else
                Chi2 = NaN;
                DOF = NaN;
                % RMS already calculated from costFun residuals
            end

            % Store fit quality metrics in object
            Obj.RMS = RMS;
            Obj.Chi2 = Chi2;
            Obj.DOF = DOF;

            FitResult = struct();
            FitResult.Cost = Cost;
            FitResult.RMS = RMS;
            FitResult.Residuals = Residuals;
            FitResult.NumObs = length(CurrentObs);
            FitResult.NumClipped = NumClipped;
            FitResult.KeepMask = KeepMask;  % Logical mask of which original observations survived
            FitResult.ConvergedSigmaClip = ConvergedSigmaClip;
            FitResult.Chi2 = Chi2;
            FitResult.DOF = DOF;

            if Args.Verbose
                fprintf('\n=== FITTING COMPLETE ===\n');
                fprintf('Final cost: %.4e\n', Cost);
                fprintf('Final observations: %d (clipped: %d)\n', length(CurrentObs), NumClipped);
                fprintf('RMS: %.4f\n', RMS);
                if ~isnan(Chi2)
                    fprintf('Chi2: %.4f, DOF: %d, Chi2/DOF: %.4f\n', ...
                            Chi2, DOF, Chi2/DOF);
                end
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
            %   OptSeq(i).Description - Description of the stage
            % Author : D. Kovaleva (Nov 2025)

            % Use stored Obj.OptSeq directly (already set by fitPar)
            OptSeq = Obj.OptSeq;
            NumStages = length(OptSeq);

            % Initialize results array
            FitResult = struct('StageName', {}, 'Method', {}, 'Cost', {}, 'RMS', {}, ...
                           'Residuals', {}, 'NumObs', {}, 'NumClipped', {}, 'KeepMask', {}, ...
                           'IsFieldCorrection', {}, 'Chi2', {}, 'DOF', {});

            % Current data (will be updated after sigma clipping in each stage)
            CurrentObs = ObservedValues(:);
            CurrentX = Args.X(:);
            CurrentY = Args.Y(:);
            CurrentCostArgs = Args.CostArgs;

            % Track cumulative KeepMask across all stages (relative to original observations)
            NumObsInitial = length(ObservedValues);
            GlobalKeepMask = true(NumObsInitial, 1);
            CurrentIndices = (1:NumObsInitial)';

            if Args.Verbose
                fprintf('\n=== MULTI-STAGE OPTIMIZATION ===\n');
                fprintf('Number of stages: %d\n', NumStages);
                fprintf('Initial observations: %d\n\n', length(CurrentObs));
            end

            % Loop through optimization stages
            for IStage = 1:NumStages
                Stage = OptSeq(IStage);
                StageName = Stage.StageName;
                FreeParamsStage = Stage.FreeParams;
                SigmaClip = Stage.SigmaClip;
                SigmaThresh = Stage.SigmaThresh;
                SigmaIter = Stage.SigmaIter;

                % Detect field correction stage (empty freeparams)
                IsFieldCorrectionStage = isempty(FreeParamsStage);

                if IsFieldCorrectionStage
                    Method = 'linear';
                else
                    Method = 'nonlinear';
                end

                if Args.Verbose
                    fprintf('=== Stage %d/%d: %s [%s] ===\n', IStage, NumStages, StageName, Method);
                    fprintf('Description: %s\n', Stage.Description);
                end

                if IsFieldCorrectionStage
                    % Field correction stage: fit position only
                    [Obj, StageResult] = Obj.fitPar(InputValues, CurrentObs, ...
                        'CostArgs', CurrentCostArgs, ...
                        'X', CurrentX, 'Y', CurrentY, ...
                        'FitTransmission', false, ...
                        'FitPosition', true, ...
                        'SigmaClip', SigmaClip, ...
                        'SigmaThresh', SigmaThresh, ...
                        'SigmaIter', SigmaIter, ...
                        'OptimizationSequence', OptSeq(IStage), ...
                        'OptimOptions', Args.OptimOptions, ...
                        'Verbose', Args.Verbose);
                else
                    % Transmission parameter stage: set FitPar flags for specified parameters
                    AllFunPar = Obj.getAllFunPar();
                    AllFunPar.FitPar(:) = false;  % Reset all to false

                    % Set FitPar for parameters specified in this stage
                    for I = 1:length(FreeParamsStage)
                        FunctionName = FreeParamsStage(I).Function;
                        ParameterName = FreeParamsStage(I).Parameter;
                        Idx = find(strcmp(AllFunPar.Name, ParameterName), 1);
                        if isempty(Idx)
                            error('Parameter "%s" (from function "%s") not found in Model', ...
                                  ParameterName, FunctionName);
                        end
                        AllFunPar.FitPar(Idx) = true;
                    end

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
                        'OptimizationSequence', OptSeq(IStage), ...
                        'OptimOptions', Args.OptimOptions, ...
                        'Verbose', Args.Verbose);
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

                    % Update WeightMatrix if present (for TransmissionMode)
                    WeightMatrixIdx = find(strcmp(CurrentCostArgs(1:2:end), 'WeightMatrix'));
                    if ~isempty(WeightMatrixIdx)
                        ActualIdx = 2 * WeightMatrixIdx;
                        CurrentCostArgs{ActualIdx} = CurrentCostArgs{ActualIdx}(:, StageKeepMask);
                    end
                end

                % Store stage results (after updating GlobalKeepMask)
                FitResult(IStage).StageName = StageName;
                FitResult(IStage).Method = Method;
                FitResult(IStage).Cost = StageResult.Cost;
                FitResult(IStage).RMS = StageResult.RMS;
                FitResult(IStage).Residuals = StageResult.Residuals;
                FitResult(IStage).NumObs = StageResult.NumObs;
                FitResult(IStage).NumClipped = NumObsInitial - sum(GlobalKeepMask);  % Cumulative clipped
                FitResult(IStage).KeepMask = GlobalKeepMask;  % Cumulative mask relative to original
                FitResult(IStage).IsFieldCorrection = IsFieldCorrectionStage;
                FitResult(IStage).Chi2 = StageResult.Chi2;
                FitResult(IStage).DOF = StageResult.DOF;

                if Args.Verbose
                    fprintf('Stage complete: Cost=%.4e, RMS=%.4f mag, NumObs=%d\n', ...
                            StageResult.Cost, StageResult.RMS, StageResult.NumObs);
                    if ~isnan(StageResult.Chi2)
                        fprintf('  Chi2: %.4f, DOF: %d, Chi2/DOF: %.4f\n', ...
                                StageResult.Chi2, StageResult.DOF, StageResult.Chi2/StageResult.DOF);
                    end
                    fprintf('\n');
                end
            end

            if Args.Verbose
                fprintf('=== MULTI-STAGE OPTIMIZATION COMPLETE ===\n\n');
            end
        end
    end

end