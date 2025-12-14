function Transmission = transmissionFun(Lambda, TransFun, Args)
    % Calculate total transmission over wavelength grid and (optionally) positional grid.
    % This function evaluates the transmission model over a wavelength grid,
    % optionally including spatial field corrections. It returns either
    % a 1D transmission vector or a 2D transmission matrix [] if positions are provided.
    %
    % Input  : - Lambda - vector of wavelengths, in nm.
    %          - TransFun - CompositeFun object containing transmission model
    %                   (atmospheric + instrumental transmission functions).
    %          * ...,key,val,...
    %            'TransParams' - vector of N_params transmission parameter values.
    %                   If provided, overrides parameters from TransFun.
    %                   Default is [].
    %            'X' - vector of X coordinates of N_sources in pixels.
    %                   If provided with Y and PolyFieldCorr, the function calculates 2D transmission.
    %                   Default is [].
    %            'Y' - vector of Y coordinates of N_sources in pixels.
    %                   If provided with X and PolyFieldCorr, the function calculates 2D transmission.
    %                   Default is [].
    %            'PolyFieldCorr' - Field correction function handle.
    %                   Signature: FieldCorrectionMag = PolyFieldCorr(X, Y, PosParams)
    %                   Returns magnitude correction (additive in mag space).
    %                   Default is [].
    %            'PosParams' - vector of field correction polynomial coefficients.
    %                   Default is zeros(1, 10).
    %            'Verbose' - Enable verbose output.
    %                   Default is false.
    % Output : - Transmission - Transmission values:
    %                   1D mode (no X/Y): [N_lambda x 1] column vector
    %                   2D mode (with X/Y): [N_sources x N_lambda] matrix
    %                   where Transmission(i,j) is transmission for source i at wavelength j
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % 1D mode: transmission over wavelength grid only
    % Define transmission functions 
    % TransFunList(1).name = 'Ozone';
    % TransFunList(1).handle = '@astro.transmission.ozoneTransmission';
    % TransFunList(1).handletype = 'named';
    % TransFunList(1).params = [30, 300];
    % TransFunList(1).paraminfo(1).name = 'ZenithAngle_deg';
    % TransFunList(1).paraminfo(1).min = 0;
    % TransFunList(1).paraminfo(1).max = 90;
    % TransFunList(1).paraminfo(2).name = 'DobsonUnits';
    % TransFunList(1).paraminfo(2).min = 200;
    % TransFunList(1).paraminfo(2).max = 400;
    % TransFunList(2).name = 'Aerosol';
    % TransFunList(2).handle = '@astro.transmission.aerosolTransmission';
    % TransFunList(2).handletype = 'named';
    % TransFunList(2).params = [30, 0.05, 1.2];
    % TransFunList(2).paraminfo(1).name = 'ZenithAngle_deg';
    % TransFunList(2).paraminfo(1).min = 0;
    % TransFunList(2).paraminfo(1).max = 90;
    % TransFunList(2).paraminfo(2).name = 'TauAod500';
    % TransFunList(2).paraminfo(2).min = 0.01;
    % TransFunList(2).paraminfo(2).max = 0.5;
    % TransFunList(2).paraminfo(3).name = 'Alpha';
    % TransFunList(2).paraminfo(3).min = 0.5;
    % TransFunList(2).paraminfo(3).max = 2.5;
    % Build model with metadata injection
    % Model = imUtil.calib.transmissionModel(TransFunList, ...
    %    'Airmass', 1.2, 'Temperature', 15, 'Pressure_mbar', 965);
    %          Lambda = linspace(336, 1020, 343)';
    %          Transmission = imUtil.calib.transmissionFun(Lambda, Model);
    %     % 2D mode: transmission over wavelength and positions
    %          X = [500; 1000; 1500];
    %          Y = [500; 1000; 1500];
    %          PolyFieldCorr = @(X, Y, P) telescope.optics.fieldCorrectionLAST([X(:), Y(:)], P);
    %          PosParams = [0.000129522473, 0.000862727726, 0.001387635303, -0.000271156095, -0.002494055579, 0.000862727726, 0.001387635304, -0.000271156095, -0.002494055580, 0.000758578889];
    %          Transmission = imUtil.calib.transmissionFun(Lambda, Model, ...
    %              'X', X, 'Y', Y, 'PolyFieldCorr', PolyFieldCorr, 'TransParams', PosParams);
    
    arguments
        Lambda                      % Wavelength grid [N_lambda x 1]
        TransFun                    % CompositeFun object
        Args.TransParams = []       % Transmission parameter values (optional)
        Args.X = []                 % X coordinates [N_sources x 1]
        Args.Y = []                 % Y coordinates [N_sources x 1]
        Args.PolyFieldCorr = []     % Field correction function handle
        Args.PosParams = zeros(1, 10)  % Field correction parameters
        Args.Verbose logical = false
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS AND ENSURE COLUMN VECTORS
    % ====================================================================

    % Ensure Lambda is column vector
    Lambda = Lambda(:);
    N_lambda = length(Lambda);

    % Ensure X, Y are column vectors if provided
    if ~isempty(Args.X)
        Args.X = Args.X(:);
    end
    if ~isempty(Args.Y)
        Args.Y = Args.Y(:);
    end

    % ====================================================================
    % STEP 2: EXTRACT OR USE PROVIDED TRANSMISSION PARAMETERS
    % ====================================================================

    if isempty(Args.TransParams)
        % Extract from TransFun
        TransParams = TransFun.valuesAllPar();
    else
        TransParams = Args.TransParams;
    end

    % Ensure row vector
    TransParams = TransParams(:)';

    if Args.Verbose
        fprintf('=== TRANSMISSION CALCULATION ===\n');
        fprintf('Wavelength range: %.1f - %.1f nm (%d points)\n', ...
                Lambda(1), Lambda(end), N_lambda);
        fprintf('Number of transmission parameters: %d\n', length(TransParams));
    end

    % ====================================================================
    % STEP 3: EVALUATE BASE TRANSMISSION (ATMOSPHERIC + INSTRUMENTAL)
    % ====================================================================

    % Evaluate transmission on Lambda grid
    Transmission_base = TransFun.evaluateAllParInput(Lambda, TransParams);

    % Ensure column vector
    Transmission_base = Transmission_base(:);

    if Args.Verbose
        fprintf('Base transmission range: %.4f - %.4f (mean: %.4f)\n', ...
                min(Transmission_base), max(Transmission_base), mean(Transmission_base));
    end

    % ====================================================================
    % STEP 4: CHECK IF FIELD CORRECTION IS REQUESTED
    % ====================================================================

    % Field correction requires all three: X, Y, and PolyFieldCorr
    ApplyFieldCorrection = ~isempty(Args.X) && ~isempty(Args.Y) && ~isempty(Args.PolyFieldCorr);

    if ~ApplyFieldCorrection
        % ================================================================
        % 1D MODE: RETURN BASE TRANSMISSION (NO FIELD CORRECTION)
        % ================================================================

        Transmission = Transmission_base;

        if Args.Verbose
            fprintf('Mode: 1D (no field correction)\n');
            fprintf('Output size: [%d x 1]\n', N_lambda);
            fprintf('=== TRANSMISSION COMPLETE ===\n');
        end

        return;
    end

    % ====================================================================
    % 2D MODE: APPLY FIELD CORRECTION TO CREATE POSITION-DEPENDENT TRANSMISSION
    % ====================================================================

    N_sources = length(Args.X);

    % Validate X and Y have same length
    if length(Args.Y) ~= N_sources
        error('X and Y must have the same length (X: %d, Y: %d)', N_sources, length(Args.Y));
    end

    if Args.Verbose
        fprintf('Mode: 2D (with field correction)\n');
        fprintf('Number of sources: %d\n', N_sources);
        fprintf('Field parameters: %d\n', length(Args.PosParams));
    end

    % ====================================================================
    % STEP 5: CALCULATE FIELD CORRECTION FOR EACH SOURCE
    % ====================================================================

    % Calculate field correction in magnitude space [N_sources x 1]
    FieldCorrectionMag = Args.PolyFieldCorr(Args.X, Args.Y, Args.PosParams);
    FieldCorrectionMag = FieldCorrectionMag(:);

    if Args.Verbose
        fprintf('Field correction (mag) range: %.4f - %.4f mag\n', ...
                min(FieldCorrectionMag), max(FieldCorrectionMag));
    end

    % ====================================================================
    % STEP 6: CONVERT FIELD CORRECTION TO TRANSMISSION SPACE
    % ====================================================================

    % In magnitude space: mag_corrected = mag + FieldCorrectionMag
    % In flux space: flux_corrected = flux * 10^(-0.4 * FieldCorrectionMag)
    % In transmission space: transmission_corrected = transmission_base * 10^(-0.4 * FieldCorrectionMag)

    FieldCorrectionTransmission = 10.^(-0.4 * FieldCorrectionMag);  % [N_sources x 1]

    if Args.Verbose
        fprintf('Field correction (transmission) range: %.4f - %.4f\n', ...
                min(FieldCorrectionTransmission), max(FieldCorrectionTransmission));
    end

    % ====================================================================
    % STEP 7: BUILD 2D TRANSMISSION MATRIX
    % ====================================================================

    % Transmission_base is [N_lambda x 1]
    % FieldCorrectionTransmission is [N_sources x 1]
    % We want output: [N_sources x N_lambda]
    %   Transmission(i, j) = Transmission_base(j) * FieldCorrectionTransmission(i)

    % Replicate base transmission for all sources: [N_sources x N_lambda]
    Transmission_base_replicated = repmat(Transmission_base', N_sources, 1);

    % Replicate field correction for all wavelengths: [N_sources x N_lambda]
    FieldCorrectionTransmission_replicated = repmat(FieldCorrectionTransmission, 1, N_lambda);

    % Apply field correction (element-wise multiplication)
    Transmission = Transmission_base_replicated .* FieldCorrectionTransmission_replicated;

    if Args.Verbose
        fprintf('Output size: [%d x %d] (sources x wavelengths)\n', N_sources, N_lambda);
        fprintf('Total transmission range: %.4f - %.4f\n', min(Transmission(:)), max(Transmission(:)));
        fprintf('=== TRANSMISSION COMPLETE ===\n');
    end
end
