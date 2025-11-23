function [Residuals, Cost, PredictedFlux] = transmissionFun(Lambda, Spec, Flux, FluxErr, X, Y, TransParams, TransFun, PolyCheb, Args)
    % Transmission-based photometric calibration: calculation of cost
    % function
    % Input  : - Lambda - Wavelength grid [N_lambda x 1] in nm
    %          - Spec - Gaia XP spectra cell array {N_calib x 2}
    %                   Column 1: Flux values [N_GaiaWvl x 1]
    %                   Column 2: Flux errors [N_GaiaWvl x 1]
    %          - Flux - Observed LAST flux [N_calib x 1]
    %          - FluxErr - Observed flux errors [N_calib x 1] (currently unused)
    %          - X - Source X coordinates [N_calib x 1]
    %          - Y - Source Y coordinates [N_calib x 1]
    %          - TransParams - Transmission parameter values vector (REQUIRED)
    %          - TransFun - CompositeFun object (for structure/evaluation)
    %          - PolyCheb - Function handle @(X, Y, FieldParams) for field correction
    %          * ...,key,val,...
    %            'FieldParams' - Field correction parameters [1 x 10].
    %                   Default is zeros(1, 10).
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
    %                   Default is false.
    % Output : - Residuals - Magnitude differences [N_calib x 1]
    %          - Cost - Sum of squared residuals (scalar)
    %          - PredictedFlux - Predicted flux in photons [N_calib x 1]
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Create transmission model
    %          Model = tools.math.fun.CompositeFun();
    %          Model.addFun('Ozone', @astro.transmission.ozoneTransmission, [], ...
    %                       'Par', [30, 300], 'FitPar', [false, false]);
    %          Model.addFun('Aerosol', @astro.transmission.aerosolTransmission, [], ...
    %                       'Par', [30, 0.05, 1.2], 'FitPar', [false, true, false]);
    %          TransParams = [30, 300, 0.05, 1.2];  % All parameter values
    %          % Create test data (3 calibrators with realistic Gaia-like spectra)
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
    %          PolyCheb = @(X, Y, P) telescope.optics.fieldCorrectionLAST([X(:), Y(:)], P);
    %          FieldParams = zeros(1, 10);
    %          [Res, Cost, Pred] = imUtil.calib.transmissionFun(Lambda, Spec, Flux, FluxErr, ...
    %              X, Y, TransParams, Model, PolyCheb, 'FieldParams', FieldParams);

    arguments
        Lambda double               % Wavelength grid [N_lambda x 1]
        Spec cell                   % Gaia XP spectra {N_calib x 2}
        Flux double                 % Observed LAST flux [N_calib x 1]
        FluxErr double              % Observed flux errors [N_calib x 1]
        X double                    % X coordinates [N_calib x 1]
        Y double                    % Y coordinates [N_calib x 1]
        TransParams double          % Transmission parameter values
        TransFun                    % CompositeFun object
        PolyCheb function_handle    % Field correction function handle
        Args.FieldParams = zeros(1, 10)  % Field correction parameters
        Args.GaiaWavelength = linspace(336, 1020, 343)'  % Gaia XP grid
        Args.Airmass = 1.2          % Atmospheric airmass
        Args.Temperature = 15       % Temperature [C]
        Args.ExpTime = 20           % Exposure time [s]
        Args.Aperture_area_m2 = pi * (0.1397)^2  % Telescope aperture [m^2]
        Args.Verbose logical = false
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS
    % ====================================================================

    % Validate TransFun
    if ~isa(TransFun, 'tools.math.fun.CompositeFun')
        error('TransFun must be a tools.math.fun.CompositeFun object');
    end

    % Check number of calibrators
    NumCalibrators = size(Spec, 1);
    if NumCalibrators == 0
        error('No calibrators in Spec');
    end

    % Validate dimensions
    if length(Flux) ~= NumCalibrators
        error('Flux size (%d) does not match number of calibrators (%d)', ...
              length(Flux), NumCalibrators);
    end
    if length(X) ~= NumCalibrators || length(Y) ~= NumCalibrators
        error('X, Y size must match number of calibrators (%d)', NumCalibrators);
    end

    % Validate parameter values
    if any(isnan(TransParams))
        error('TransParams contains NaN. Check parameter initialization.');
    end

    % Validate Lambda
    if isempty(Lambda) || ~isvector(Lambda)
        error('Lambda must be a non-empty vector');
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
    % DETERMINE INTEGRATION RANGE ON GAIA GRID
    % ----------------------------------------------------------------

    % Lambda defines the integration range [Lambda_min, Lambda_max]
    Lambda_min = min(Lambda);
    Lambda_max = max(Lambda);

    % Find subset of Gaia wavelength grid within Lambda range
    GaiaInRange = (Args.GaiaWavelength >= Lambda_min) & (Args.GaiaWavelength <= Lambda_max);
    GaiaWvl_Integration = Args.GaiaWavelength(GaiaInRange);
    NumIntegrationPoints = length(GaiaWvl_Integration);

    if NumIntegrationPoints == 0
        error('No Gaia wavelengths within Lambda range [%.1f, %.1f] nm', Lambda_min, Lambda_max);
    end

    if Args.Verbose
        fprintf('Integration range: %.1f - %.1f nm (%d Gaia grid points)\n', ...
                GaiaWvl_Integration(1), GaiaWvl_Integration(end), NumIntegrationPoints);
    end

    % ----------------------------------------------------------------
    % INTERPOLATE TRANSMISSION ONTO GAIA INTEGRATION GRID
    % ----------------------------------------------------------------

    % Evaluate transmission on Lambda grid (arbitrary grid)
    Transmission_Lambda = TransFun.evaluateAllParInput(Lambda, TransParams(:)');

    % Interpolate transmission onto Gaia integration grid
    % Use linear interpolation (Gaia grid is always within or equal to Lambda range)
    Transmission_Gaia = interp1(Lambda, Transmission_Lambda, GaiaWvl_Integration, 'linear');

    if Args.Verbose
        fprintf('Transmission range: %.4f - %.4f (mean: %.4f)\n', ...
                min(Transmission_Gaia), max(Transmission_Gaia), mean(Transmission_Gaia));
    end

    % ----------------------------------------------------------------
    % EXTRACT GAIA SPECTRA FOR INTEGRATION RANGE
    % ----------------------------------------------------------------

    % Extract all Gaia fluxes from cell array into matrix
    % Spec is {N_calib x 2} where column 1 contains flux vectors [NumGaia x 1]
    % Assume column vectors as input (no dimension checking)
    GaiaFluxMatrix_Full = cell2mat(Spec(:, 1)');  % [NumGaia x NumCalibrators]

    % Extract only the wavelengths within integration range
    GaiaFluxMatrix = GaiaFluxMatrix_Full(GaiaInRange, :);  % [NumIntegrationPoints x NumCalibrators]

    % ----------------------------------------------------------------
    % APPLY TRANSMISSION AND INTEGRATE
    % ----------------------------------------------------------------

    % Apply transmission to all spectra (implicit broadcasting)
    % Transmission_Gaia is [NumIntegrationPoints x 1], broadcasts to all calibrators
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

    % ====================================================================
    % STEP 6: APPLY FIELD CORRECTION
    % ====================================================================

    % Check if field correction parameters are non-zero
    if any(Args.FieldParams ~= 0)
        if Args.Verbose
            fprintf('Applying field corrections...\n');
        end

        % Apply field correction using provided function handle
        FieldCorrectionMag = PolyCheb(X, Y, Args.FieldParams);

        % Ensure column vector
        FieldCorrectionMag = FieldCorrectionMag(:);

        % Add field correction to magnitude difference (additive in magnitude space)
        DiffMag = DiffMag + FieldCorrectionMag;

        if Args.Verbose
            fprintf('Field correction range: %.4f - %.4f mag\n', ...
                    min(FieldCorrectionMag), max(FieldCorrectionMag));
        end
    end

    % ====================================================================
    % STEP 7: CALCULATE COST AND RESIDUALS
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
        fprintf('=== transmissionFun COMPLETE ===\n\n');
    end
end
