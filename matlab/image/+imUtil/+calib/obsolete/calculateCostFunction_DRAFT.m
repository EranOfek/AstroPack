function [Cost, Residuals, DiffMag, PredictedFlux] = calculateCostFunction_DRAFT(CalibData, ParamValues, TransFun, Args)
    % Calculate cost function for transmission-based photometric calibration
    % This function applies transmission to Gaia XP spectra, integrates flux,
    % and calculates residuals vs observed LAST flux.
    % OPTIMIZED VERSION: Vectorized spectrum processing and eliminated repmat operations.
    %
    % Input  : - CalibData - Structure with calibrator data:
    %            .Spec - Cell array {N x 2} with Gaia XP spectra:
    %                    Column 1: Flux values (343 wavelength points)
    %                    Column 2: Flux error values
    %            .Coords - Structure array with coordinate information
    %            .LASTData - Table with LAST catalog data (includes FLUX_APER_3, X, Y)
    %            .Metadata - Observation metadata (airmass, temperature, pressure, etc.)
    %          - ParamValues - Vector of transmission parameter values to evaluate.
    %          - TransFun - CompositeFun object (read-only, used for structure only).
    %          * ...,key,val,...
    %            'WavelengthRange' - Wavelength range for integration [min max] nm.
    %                   Default is [300 1100].
    %            'GaiaWavelength' - Gaia XP wavelength grid (nm).
    %                   Default is standard Gaia XP grid (336-1020 nm, 343 points).
    %            'FieldParams' - Field correction parameters [1 x 10].
    %                   Default is zeros(1, 10).
    %            'FluxColName' - LAST flux column name. Default is 'FLUX_APER_3'.
    %            'Verbose' - Enable verbose output. Default is false.
    %
    % Output : - Cost - Sum of squared magnitude differences (scalar).
    %          - Residuals - Individual magnitude differences (DiffMag) [N x 1].
    %          - DiffMag - Magnitude differences: 2.5*log10(PredictedFlux/ObservedFlux) [N x 1].
    %          - PredictedFlux - Predicted flux in photons for each calibrator [N x 1].
    %
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Build transmission function
    %          TransFun = tools.math.fun.CompositeFun();
    %          TransFun.addFun('Normalization', @(Lambda, Norm) Norm*ones(size(Lambda)), [], ...
    %                          'Par', [1.0], 'FitPar', [true]);
    %          TransFun.addFun('Water', @astro.transmission.waterTransmission, [], ...
    %                          'Par', [2.0], 'FitPar', [true]);
    %          % Calculate cost
    %          [Cost, Residuals, DiffMag] = imUtil.calib.calculateCostFunction_DRAFT(...
    %              CalibData, TransFun, 'Verbose', true);

    arguments
        CalibData struct
        ParamValues double  % Parameter values vector
        TransFun  % tools.math.fun.CompositeFun object (read-only)
        Args.WavelengthRange = [300, 1100]  % nm, will create linspace
        Args.GaiaWavelength = linspace(336, 1020, 343)'  % Standard Gaia XP grid
        Args.FieldParams = zeros(1, 10)  % Field correction parameters [kx0, kx, kx2, kx3, kx4, ky, ky2, ky3, ky4, kxy]
        Args.FluxColName = 'FLUX_APER_3'
        Args.Verbose logical = false
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS
    % ====================================================================

    % Validate TransFun
    if ~isa(TransFun, 'tools.math.fun.CompositeFun')
        error('TransFun must be a tools.math.fun.CompositeFun object');
    end

    % Validate CalibData structure
    RequiredFields = {'Spec', 'LASTData', 'Metadata'};
    for i = 1:length(RequiredFields)
        if ~isfield(CalibData, RequiredFields{i})
            error('CalibData missing required field: %s', RequiredFields{i});
        end
    end

    % Check number of calibrators
    NumCalibrators = length(CalibData.Spec);
    if NumCalibrators == 0
        error('No calibrators in CalibData.Spec');
    end

    % Validate metadata (error if missing critical values)
    % Note: Pressure is not in Metadata - it's read from YAML config
    if ~isfield(CalibData.Metadata, 'Airmass') || isnan(CalibData.Metadata.Airmass)
        error('CalibData.Metadata.Airmass is missing or NaN');
    end
    if ~isfield(CalibData.Metadata, 'Temperature') || isnan(CalibData.Metadata.Temperature)
        error('CalibData.Metadata.Temperature is missing or NaN');
    end

    if Args.Verbose
        fprintf('=== COST FUNCTION CALCULATION ===\n');
        fprintf('Number of calibrators: %d\n', NumCalibrators);
        fprintf('Airmass: %.3f\n', CalibData.Metadata.Airmass);
        fprintf('Temperature: %.1f C\n', CalibData.Metadata.Temperature);
    end

    % ====================================================================
    % STEP 2: SETUP WAVELENGTH GRID
    % ====================================================================

    % Define Gaia XP wavelength grid if not provided
    if isempty(Args.GaiaWavelength)
        % Standard Gaia XP wavelength grid: 336-1020 nm, 343 points
        % Spacing: ~2 nm
        Args.GaiaWavelength = linspace(336, 1020, 343)';
    end

    % Create wavelength range for integration (beyond Gaia range)
    % We'll pad with zeros outside Gaia coverage
    WavelengthRange = Args.WavelengthRange;

    % Convert to numeric array if passed as cell array from YAML
    if iscell(WavelengthRange)
        WavelengthRange = cell2mat(WavelengthRange);
    end

    Lambda = linspace(WavelengthRange(1), WavelengthRange(2), ...
                     round((WavelengthRange(2) - WavelengthRange(1)) / 2) + 1)';

    if Args.Verbose
        fprintf('Wavelength grid: %.0f - %.0f nm (%d points)\n', ...
                Lambda(1), Lambda(end), length(Lambda));
        fprintf('Gaia XP range: %.0f - %.0f nm (%d points)\n', ...
                Args.GaiaWavelength(1), Args.GaiaWavelength(end), length(Args.GaiaWavelength));
    end

    % ====================================================================
    % STEP 3: APPLY TRANSMISSION TO GAIA SPECTRA (VECTORIZED)
    % ====================================================================

    if Args.Verbose
        fprintf('Applying transmission function to %d spectra...\n', NumCalibrators);
    end

    % Validate parameter values
    if any(isnan(ParamValues))
        error('ParamValues contains NaN. Check parameter initialization.');
    end

    % Evaluate transmission function with provided parameter values
    % TransFun is read-only - we don't modify it, just use its structure
    Transmission = TransFun.evaluateAllParInput(Lambda, ParamValues(:)');

    if Args.Verbose
        fprintf('Transmission range: %.4f - %.4f (mean: %.4f)\n', ...
                min(Transmission), max(Transmission), mean(Transmission));
    end

    % Find indices for mapping Gaia wavelengths to extended grid
    StartIdx = find(Lambda == Args.GaiaWavelength(1), 1);
    EndIdx = find(Lambda == Args.GaiaWavelength(end), 1);

    if isempty(StartIdx) || isempty(EndIdx)
        error('Gaia wavelength range not found in Lambda grid. Check wavelength grids are compatible.');
    end

    % Get dimensions for pre-allocation
    NumLambda = length(Lambda);
    NumGaia = length(Args.GaiaWavelength);

    % ----------------------------------------------------------------
    % OPTIMIZATION 1: VECTORIZED SPECTRUM PROCESSING
    % Instead of looping over calibrators, process all at once
    % ----------------------------------------------------------------

    % Extract all Gaia fluxes from cell array into matrix (vectorized, no loop)
    % CalibData.Spec is {N x 2} where column 1 contains flux vectors
    % If flux vectors are row vectors [1 x 343], cell2mat stacks them as rows: [116 x 343]
    % Then transpose to get [343 x 116] which is [NumGaia x NumCalibrators]
    GaiaFluxMatrix = cell2mat(CalibData.Spec(:, 1))';  % [NumGaia x NumCalibrators]

    % Pre-allocate extended flux matrix [NumLambda x NumCalibrators]
    ExtendedFluxMatrix = zeros(NumLambda, NumCalibrators);

    % Fill in Gaia data for all calibrators at once (middle section)
    ExtendedFluxMatrix(StartIdx:EndIdx, :) = GaiaFluxMatrix;

    % Extrapolate flux at 336 nm down to 300 nm (constant value)
    % Replicate first row of GaiaFluxMatrix for all wavelengths below StartIdx
    if StartIdx > 1
        ExtendedFluxMatrix(1:StartIdx-1, :) = repmat(GaiaFluxMatrix(1, :), StartIdx-1, 1);
    end

    % Extrapolate flux at 1020 nm up to 1100 nm (constant value)
    % Replicate last row of GaiaFluxMatrix for all wavelengths above EndIdx
    if EndIdx < NumLambda
        ExtendedFluxMatrix(EndIdx+1:end, :) = repmat(GaiaFluxMatrix(end, :), NumLambda-EndIdx, 1);
    end

    % Apply transmission to all spectra at once using implicit broadcasting
    % Transmission is [NumLambda x 1], ExtendedFluxMatrix is [NumLambda x NumCalibrators]
    % MATLAB automatically broadcasts Transmission across all columns
    TransmittedSpectra = ExtendedFluxMatrix .* Transmission;

    % ====================================================================
    % STEP 4: INTEGRATE TRANSMITTED FLUX (OPTIMIZED)
    % ====================================================================

    if Args.Verbose
        fprintf('Integrating transmitted flux...\n');
    end

    % ----------------------------------------------------------------
    % OPTIMIZATION 2: ELIMINATE REPMAT - USE IMPLICIT EXPANSION
    % ----------------------------------------------------------------

    % Transpose for integration: [NumCalibrators x NumLambda]
    TransmittedSpectraT = TransmittedSpectra';

    % Calculate integrand using implicit expansion (broadcasting)
    % Lambda is [NumLambda x 1], automatically broadcasts to [NumCalibrators x NumLambda]
    Integrand = TransmittedSpectraT .* Lambda(:)';

    % Integrate over wavelength dimension (dimension 2)
    % For trapzmat, we need to provide the X values (Lambda)
    % Use implicit expansion for Lambda as well - no need for repmat
    LambdaRow = Lambda(:)';  % [1 x NumLambda]
    A_vector = tools.math.integral.trapzmat(LambdaRow, Integrand, 2);

    % Ensure A_vector is a column vector [NumCalibrators x 1]
    A_vector = A_vector(:);

    % ====================================================================
    % STEP 5: CONVERT INTEGRATED FLUX TO PHOTONS
    % ====================================================================

    % Physical constants
    H = constant.h('SI');      % Planck constant [J·s]
    C = constant.c('SI');      % Speed of light [m/s]
    B = H * C * 1e9;           % H*C with nm to m conversion (1e9 for nm to m)

    % Get telescope parameters from metadata
    % Note: Exposure time should be in CalibData.Metadata if available
    if isfield(CalibData.Metadata, 'ExpTime') && ~isnan(CalibData.Metadata.ExpTime)
        Dt = CalibData.Metadata.ExpTime;  % Exposure time [seconds]
    else
        Dt = 20.0;  % Default exposure time [seconds]
    end

    if isfield(CalibData.Metadata, 'Aperture_area_m2') && ~isnan(CalibData.Metadata.Aperture_area_m2)
        Ageom = CalibData.Metadata.Aperture_area_m2;  % Telescope aperture area [m²]
    else
        % Default for LAST: π * (0.1397)² m²
        Ageom = pi * (0.1397)^2;
    end

    % Convert to photons
    % Note: Normalization is already included in TransFun (first parameter in CompositeFun)
    PredictedFlux = Dt * Ageom * A_vector / B;

    if Args.Verbose
        fprintf('Photon conversion: Dt=%.1f s, Ageom=%.4f m^2, B=%.4e\n', Dt, Ageom, B);
        fprintf('Predicted flux (photons) range: %.2e - %.2e\n', ...
                min(PredictedFlux), max(PredictedFlux));
        fprintf('Predicted flux (photons) mean: %.2e\n', mean(PredictedFlux));
    end

    % ====================================================================
    % STEP 6: CALCULATE MAGNITUDE DIFFERENCES
    % ====================================================================

    % Extract observed LAST flux
    if ~ismember(Args.FluxColName, CalibData.LASTData.Properties.VariableNames)
        error('Flux column "%s" not found in LASTData', Args.FluxColName);
    end
    ObservedFlux = CalibData.LASTData.(Args.FluxColName);

    % Calculate magnitude difference
    % DiffMag = -2.5 * log10(ObservedFlux / PredictedFlux)
    %         = 2.5 * log10(PredictedFlux / ObservedFlux)
    DiffMag = 2.5 * log10(PredictedFlux ./ ObservedFlux);

    % ====================================================================
    % STEP 7: APPLY FIELD CORRECTION
    % ====================================================================

    % Check if field correction parameters are non-zero
    if any(Args.FieldParams ~= 0)
        % Apply field-dependent correction in magnitude units
        if Args.Verbose
            fprintf('Applying field corrections...\n');
        end

        % Extract X, Y positions from LASTData
        X = CalibData.LASTData.X;
        Y = CalibData.LASTData.Y;

        % Build Coords matrix [N x 2]
        Coords = [X(:), Y(:)];

        % Apply field correction (returns magnitude correction for each calibrator)
        FieldCorrectionMag = astro.transmission.fieldCorrection(Coords, Args.FieldParams);

        % Add field correction to magnitude difference (additive in magnitude space)
        DiffMag = DiffMag + FieldCorrectionMag;

        if Args.Verbose
            fprintf('Field correction range: %.4f - %.4f mag\n', ...
                    min(FieldCorrectionMag), max(FieldCorrectionMag));
        end
    end

    % ====================================================================
    % STEP 8: CALCULATE COST AND RESIDUALS
    % ====================================================================

    % Residuals are the magnitude differences
    Residuals = DiffMag;

    % Check for NaN/Inf residuals
    ValidMask = isfinite(Residuals);
    if sum(ValidMask) < length(Residuals)
        error('Found %d invalid residuals (NaN/Inf). Check transmission parameters and calibrator data.', ...
              sum(~ValidMask));
    end

    % Cost is sum of squared residuals
    Cost = sum(Residuals.^2);

    % Calculate RMS for reporting
    RMS = sqrt(Cost / length(Residuals));

    if Args.Verbose
        fprintf('Residuals: mean=%.4f mag, std=%.4f mag, RMS=%.4f mag\n', ...
                mean(Residuals), std(Residuals), RMS);
        fprintf('Cost: %.4e\n', Cost);
        fprintf('=== COST FUNCTION COMPLETE ===\n\n');
    end
end
