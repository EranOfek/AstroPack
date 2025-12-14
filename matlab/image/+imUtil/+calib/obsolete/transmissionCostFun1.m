function [Residuals, Cost, PredictedFlux] = transmissionCostFun1(Lambda, Spec, SpecErr, Flux, FluxErr, TransFun, Args)
    % Cost function for transmission-based photometric calibration with integrated Tran2D.
    % Part of Transmission-based photometric calibration pipeline: calculates cost
    % function for optimization by imUtil.calib.transmissionFit1. Function is comparing
    % the instrumental fluxes of stars in the image to the synthetic photometry of the stars
    % given their spectrum and the composite wavelength-dependent transmission function
    % which has free parameters with addition of position-dependent Tran2D corrections
    % integrated into the CompositeFun model to account for ZP variations across the field.
    %
    % Input  : - Lambda - Wavelength grid [N_lambda x 1] in nm
    %          - Spec - Gaia XP spectra matrix [N_GaiaWvl x N_calib]
    %                   Each column is the flux spectrum for one calibrator
    %          - SpecErr - Gaia XP spectra errors matrix [N_GaiaWvl x N_calib]
    %                   Each column is the flux error spectrum (currently unused)
    %          - Flux - Observed LAST flux (vector of N_calib values)
    %          - FluxErr - Observed flux errors (vector of N_calib values) (currently unused)
    %          - TransFun - CompositeFun object with integrated Tran2D (for structure/evaluation)
    %          * ...,key,val,...
    %            'TransParams' - Transmission parameter values vector.
    %                   If provided, overrides parameters in TransFun.
    %                   If empty, extracts current parameters from TransFun using valuesAllPar().
    %                   Default is [].
    %            'X' - Source X coordinates (vector of N_calib values).
    %                   Required if using position-dependent corrections (Tran2D).
    %                   Default is [].
    %            'Y' - Source Y coordinates (vector of N_calib values).
    %                   Required if using position-dependent corrections (Tran2D).
    %                   Default is [].
    %            'GaiaWavelength' - Gaia XP wavelength vector of N_GaiaWvl values [nm].
    %                   Default is linspace(336, 1020, 343)'.
    %            'Airmass' - Atmospheric airmass.
    %                   Default is 1.2.
    %            'Temperature' - Temperature [C].
    %                   Default is 15.
    %            'Pressure_mbar' - Atmospheric pressure [mbar].
    %                   Default is 965.
    %            'Time' - Exposure time [s].
    %                   Default is 20.
    %            'Aperture_area_m2' - Telescope aperture area [m^2].
    %                   Default is pi * (0.1397)^2 (LAST).
    %            'ValInp' - Validate inputs (logical).
    %                   Default is true.
    %            'Verbose' - Enable verbose output.
    %                   Default is false.
    % Output : - Residuals - Magnitude differences [N_calib x 1]
    %          - Cost - Sum of squared residuals (scalar)
    %          - PredictedFlux - Predicted flux in photons [N_calib x 1]
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Define transmission functions as struct array
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
    %          % Build model with integrated Tran2D position corrections
    %          Model = imUtil.calib.transmissionModel1(TransFunList, ...
    %              'Airmass', 1.2, 'Temperature', 15, 'Pressure_mbar', 965);
    %          % Create test data (3 calibrators with realistic Gaia-like spectra)
    %          Lambda = linspace(336, 1020, 343)';
    %          Spec = [(5e-17) ./ (Lambda / 400).^2, ...      % Blue star
    %                  (3e-17) ./ (Lambda / 550).^0.5, ...    % Solar-type star
    %                  (2e-17) * (Lambda / 700).^1.5];        % Red star [343 x 3]
    %          SpecErr = 0.05 * Spec;  % 5% errors
    %          Flux = [5.1e4; 7.5e4; 6.3e4];  % Observed photons
    %          FluxErr = [5e3; 4e2; 3e2];
    %          X = [500; 1000; 1500];  % Pixel coordinates
    %          Y = [500; 1000; 1500];
    %          % Option 1: Pass TransParams explicitly (override model parameters)
    %          TransParams = Model.valuesAllPar();
    %          [Res, Cost, Pred] = imUtil.calib.transmissionCostFun1(Lambda, Spec, SpecErr, ...
    %              Flux, FluxErr, Model, 'TransParams', TransParams, 'X', X, 'Y', Y);
    %          % Option 2: Use parameters from Model (automatic extraction)
    %          [Res, Cost, Pred] = imUtil.calib.transmissionCostFun1(Lambda, Spec, SpecErr, ...
    %              Flux, FluxErr, Model, 'X', X, 'Y', Y);

    arguments
        Lambda                      % Wavelength grid [N_lambda x 1]
        Spec                        % Gaia XP spectra matrix [N_GaiaWvl x N_calib]
        SpecErr                     % Gaia XP spectra errors [N_GaiaWvl x N_calib]
        Flux                        % Observed LAST flux [N_calib x 1]
        FluxErr                     % Observed flux errors [N_calib x 1]
        TransFun                    % CompositeFun object with integrated Tran2D
        Args.TransParams = []       % Transmission parameter values (optional)
        Args.X = []                 % X coordinates [N_calib x 1]
        Args.Y = []                 % Y coordinates [N_calib x 1]
        Args.GaiaWavelength = linspace(336, 1020, 343)'  % Gaia XP grid
        Args.Airmass = 1.2          % Atmospheric airmass
        Args.Temperature = 15       % Temperature [C]
        Args.Pressure_mbar = 965    % Atmospheric pressure [mbar]
        Args.ExpTime = 20           % Exposure time [s]
        Args.Aperture_area_m2 = pi * (0.1397)^2  % Telescope aperture [m^2]
        Args.ValInp logical = true  % Validate inputs
        Args.Verbose logical = false
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS (if enabled)
    % ====================================================================

    if Args.ValInp
        % Validate TransFun
        if ~isa(TransFun, 'tools.math.fun.CompositeFun')
            error('TransFun must be a tools.math.fun.CompositeFun object');
        end
    end

    % Extract or validate transmission parameters
    if isempty(Args.TransParams)
        % Extract current parameters from TransFun
        TransParams = TransFun.valuesAllPar();
        if Args.Verbose
            fprintf('Extracted %d transmission parameters from TransFun\n', length(TransParams));
        end
    else
        % Use provided TransParams
        TransParams = Args.TransParams;
        if Args.ValInp
            % Validate that TransParams matches expected size
            ExpectedNumParams = TransFun.numAllPar();
            if length(TransParams) ~= ExpectedNumParams
                error('TransParams size (%d) does not match TransFun parameter count (%d)', ...
                      length(TransParams), ExpectedNumParams);
            end
        end
        if Args.Verbose
            fprintf('Using provided TransParams (%d parameters)\n', length(TransParams));
        end
    end

    if Args.ValInp
        % Validate parameter values
        if any(isnan(TransParams))
            error('TransParams contains NaN. Check parameter initialization.');
        end
    end

    % Check number of calibrators (Spec is [N_GaiaWvl x N_calib])
    NumCalibrators = size(Spec, 2);
    if Args.ValInp
        if NumCalibrators == 0
            error('No calibrators in Spec');
        end

        % Validate dimensions
        if length(Flux) ~= NumCalibrators
            error('Flux size (%d) does not match number of calibrators (%d)', ...
                  length(Flux), NumCalibrators);
        end

        % Validate X, Y if provided
        if ~isempty(Args.X) && length(Args.X) ~= NumCalibrators
            error('X size (%d) does not match number of calibrators (%d)', length(Args.X), NumCalibrators);
        end
        if ~isempty(Args.Y) && length(Args.Y) ~= NumCalibrators
            error('Y size (%d) does not match number of calibrators (%d)', length(Args.Y), NumCalibrators);
        end
        if size(SpecErr, 1) ~= size(Spec, 1) || size(SpecErr, 2) ~= size(Spec, 2)
            error('SpecErr dimensions must match Spec dimensions');
        end

        % Validate Lambda
        if isempty(Lambda) || ~isvector(Lambda)
            error('Lambda must be a non-empty vector');
        end
    end
    Lambda = Lambda(:);  % Ensure column vector 

    % Validate GaiaWavelength
    if isempty(Args.GaiaWavelength)
        Args.GaiaWavelength = linspace(336, 1020, 343)';
    end
    Args.GaiaWavelength = Args.GaiaWavelength(:);  % Ensure column vector 

    if Args.Verbose
        fprintf('=== TRANSMISSION COST FUNCTION ===\n');
        fprintf('Number of calibrators: %d\n', NumCalibrators);
        fprintf('Airmass: %.3f\n', Args.Airmass);
        fprintf('Temperature: %.1f C\n', Args.Temperature);
        fprintf('Wavelength grid: %.0f - %.0f nm (%d points)\n', ...
                Lambda(1), Lambda(end), length(Lambda));
        fprintf('Gaia XP range: %.0f - %.0f nm (%d points)\n', ...
                Args.GaiaWavelength(1), Args.GaiaWavelength(end), length(Args.GaiaWavelength));
    end

    % ====================================================================
    % STEP 2: APPLY TRANSMISSION TO GAIA SPECTRA 
    % ====================================================================

    if Args.Verbose
        fprintf('Applying transmission function to %d spectra...\n', NumCalibrators);
    end

    % ----------------------------------------------------------------
    % DETERMINE INTEGRATION RANGE AND EXTEND GAIA GRID IF NEEDED
    % ----------------------------------------------------------------

    % Lambda defines the integration range [Lambda_min, Lambda_max]
    Lambda_min = min(Lambda);
    Lambda_max = max(Lambda);

    % Get Gaia wavelength range
    GaiaWvl_min = Args.GaiaWavelength(1);
    GaiaWvl_max = Args.GaiaWavelength(end);

    % Check if Lambda range extends beyond Gaia range
    NeedExtrapolationBelow = Lambda_min < GaiaWvl_min;
    NeedExtrapolationAbove = Lambda_max > GaiaWvl_max;

    % Find subset of Gaia wavelength grid within Lambda range
    GaiaInRange = (Args.GaiaWavelength >= Lambda_min) & (Args.GaiaWavelength <= Lambda_max);
    GaiaWvl_InRange = Args.GaiaWavelength(GaiaInRange);

    if isempty(GaiaWvl_InRange)
        error('No Gaia wavelengths within Lambda range [%.1f, %.1f] nm', Lambda_min, Lambda_max);
    end

    % Build integration wavelength grid (may include extrapolated points)
    GaiaWvl_Integration = GaiaWvl_InRange;

    % Add extrapolation points if needed
    if NeedExtrapolationBelow
        % Add point at Lambda_min for extrapolation
        GaiaWvl_Integration = [Lambda_min; GaiaWvl_Integration];
    end
    if NeedExtrapolationAbove
        % Add point at Lambda_max for extrapolation
        GaiaWvl_Integration = [GaiaWvl_Integration; Lambda_max];
    end

    NumIntegrationPoints = length(GaiaWvl_Integration);

    if Args.Verbose
        fprintf('Integration range: %.1f - %.1f nm (%d points)\n', ...
                GaiaWvl_Integration(1), GaiaWvl_Integration(end), NumIntegrationPoints);
        if NeedExtrapolationBelow
            fprintf('  Extrapolating below Gaia range (%.1f < %.1f nm)\n', Lambda_min, GaiaWvl_min);
        end
        if NeedExtrapolationAbove
            fprintf('  Extrapolating above Gaia range (%.1f > %.1f nm)\n', Lambda_max, GaiaWvl_max);
        end
    end

    % ----------------------------------------------------------------
    % INTERPOLATE TRANSMISSION ONTO GAIA INTEGRATION GRID
    % ----------------------------------------------------------------

    % Check if position-dependent corrections are enabled (Tran2D integrated)
    UsePositionCorrections = TransFun.UseTran2D && ~isempty(Args.X) && ~isempty(Args.Y);

    if UsePositionCorrections
        % Evaluate transmission with position-dependent corrections on Lambda grid
        % evaluateAllParInputWithPosition returns [N_calib x N_lambda]
        Transmission_Lambda = TransFun.evaluateAllParInputWithPosition(Lambda, Args.X, Args.Y, ...
            'TransParams', TransParams(:)');

        % Interpolate: transpose to [N_lambda x N_calib], interpolate to get [N_integration_points x N_calib]
        Transmission_Gaia = interp1(Lambda, Transmission_Lambda', GaiaWvl_Integration, 'linear');
        % Result: [N_integration_points x N_calib]
    else
        % Evaluate transmission without position corrections on Lambda grid
        % Returns [N_lambda x 1]
        Transmission_Lambda = TransFun.evaluateAllParInput(Lambda, TransParams(:)');

        % Interpolate transmission onto Gaia integration grid (broadcasts to all calibrators)
        Transmission_Gaia = interp1(Lambda, Transmission_Lambda, GaiaWvl_Integration, 'linear');
        % Result: [N_integration_points x 1]
    end

    if Args.Verbose
        if UsePositionCorrections
            fprintf('Transmission with position corrections (Tran2D):\n');
            fprintf('  Range: %.4f - %.4f (mean: %.4f)\n', ...
                    min(Transmission_Gaia(:)), max(Transmission_Gaia(:)), mean(Transmission_Gaia(:)));
        else
            fprintf('Transmission range: %.4f - %.4f (mean: %.4f)\n', ...
                    min(Transmission_Gaia), max(Transmission_Gaia), mean(Transmission_Gaia));
        end
    end

    % ----------------------------------------------------------------
    % EXTRACT AND EXTRAPOLATE GAIA SPECTRA FOR INTEGRATION RANGE
    % ----------------------------------------------------------------

    % Spec is already a matrix [N_GaiaWvl x N_calib]
    % Extract wavelengths within integration range
    GaiaFluxMatrix_InRange = Spec(GaiaInRange, :);  % [NumInRange x NumCalibrators]

    % Build full flux matrix with extrapolation if needed
    GaiaFluxMatrix = GaiaFluxMatrix_InRange;

    % Add extrapolated values if needed
    if NeedExtrapolationBelow
        % Use first Gaia spectrum value for wavelengths below range
        % Spec(1, :) is the flux at GaiaWvl_min for all calibrators
        FirstRow = Spec(1, :);  % [1 x NumCalibrators]
        GaiaFluxMatrix = [FirstRow; GaiaFluxMatrix];
    end
    if NeedExtrapolationAbove
        % Use last Gaia spectrum value for wavelengths above range
        % Spec(end, :) is the flux at GaiaWvl_max for all calibrators
        LastRow = Spec(end, :);  % [1 x NumCalibrators]
        GaiaFluxMatrix = [GaiaFluxMatrix; LastRow];
    end
    % Now GaiaFluxMatrix is [NumIntegrationPoints x NumCalibrators]

    % ----------------------------------------------------------------
    % APPLY TRANSMISSION AND INTEGRATE
    % ----------------------------------------------------------------

    % Apply transmission to all spectra
    % Without position corrections: Transmission_Gaia is [NumIntegrationPoints x 1], broadcasts to all calibrators
    % With position corrections: Transmission_Gaia is [NumIntegrationPoints x NumCalibrators], element-wise multiplication
    TransmittedSpectra = GaiaFluxMatrix .* Transmission_Gaia;  % [NumIntegrationPoints x NumCalibrators]

    % ====================================================================
    % STEP 3: INTEGRATE TRANSMITTED FLUX OVER GAIA GRID
    % ====================================================================

    if Args.Verbose
        fprintf('Integrating transmitted flux over Gaia wavelength grid...\n');
    end

    % Transpose for integration: [NumCalibrators x NumIntegrationPoints]
    TransmittedSpectraT = TransmittedSpectra';

    % Calculate integrand: Flux * Lambda
    Integrand = TransmittedSpectraT .* GaiaWvl_Integration(:)';

    % Integrate over wavelength dimension using Gaia grid
    GaiaWvl_Row = GaiaWvl_Integration(:)';
    A_vector = tools.math.integral.trapzmat(GaiaWvl_Row, Integrand, 2);
    A_vector = A_vector(:);  % Ensure column vector

    % ====================================================================
    % STEP 4: CONVERT INTEGRATED FLUX TO PHOTONS
    % ====================================================================

    % Physical constants
    H = constant.h('SI');      % Planck constant [J·s]
    C = constant.c('SI');      % Speed of light [m/s]
    B = H * C * 1e9;           % H*C with nm to m conversion

    % Get telescope parameters
    Dt = Args.ExpTime;         % Exposure time [s]
    Ageom = Args.Aperture_area_m2;  % Aperture area [m^2]

    % Convert to photons
    PredictedFlux = Dt * Ageom * A_vector / B;

    if Args.Verbose
        fprintf('Photon conversion: Dt=%.1f s, Ageom=%.4f m^2, B=%.4e\n', Dt, Ageom, B);
        fprintf('Predicted flux (photons) range: %.2e - %.2e\n', ...
                min(PredictedFlux), max(PredictedFlux));
        fprintf('Predicted flux (photons) mean: %.2e\n', mean(PredictedFlux));
    end

    % ====================================================================
    % STEP 5: CALCULATE MAGNITUDE DIFFERENCES
    % ====================================================================

    % Ensure Flux is column vector
    Flux = Flux(:);

    % Calculate magnitude difference
    DiffMag = 2.5 * log10(PredictedFlux ./ Flux);

    % Note: Position-dependent corrections (if enabled) are already integrated
    % into the transmission calculation via Tran2D in evaluateAllParInputWithPosition()

    % ====================================================================
    % STEP 6: CALCULATE COST AND RESIDUALS
    % ====================================================================

    % Residuals are the magnitude differences
    Residuals = DiffMag;

    % Cost is sum of squared residuals
    Cost = sum(Residuals.^2);

    % Calculate RMS for reporting
    RMS = sqrt(Cost / length(Residuals));

    if Args.Verbose
        fprintf('Residuals: mean=%.4f mag, std=%.4f mag, RMS=%.4f mag\n', ...
                mean(Residuals), std(Residuals), RMS);
        fprintf('Cost: %.4e\n', Cost);
        fprintf('=== transmissionCostFun COMPLETE ===\n\n');
    end
end
