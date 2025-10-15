function Result = rayleighTransmission(Lambda, ParamMatrix, Args)
    % Rayleigh transmission of the Earth atmosphere 
    % Input  : - Lambda (double array): Wavelength array in nm.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is
    %            [ZenithAngle_deg, Pressure_mbar]. 
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    % Output : - Result (double matrix): Transmission matrix (wavelengths x parameter_sets).
    % Author : D. Kovaleva (Oct 2025)
    % Example: Lambda = linspace(300, 1100, 401)';
    %          ParamMatrix = [45, 1013; 60, 950; 30, 1020];
    %          Result = astro.transmission.rayleighTransmission(Lambda, ParamMatrix);

    arguments
        Lambda      = linspace(300, 1100, 401)
        ParamMatrix = [30, 965]               % [ZenithAngle_deg, Pressure_mbar]  
        Args.AbsorptionData = []
        Args.Return = []
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    % Validate input dimensions
    if size(ParamMatrix, 2) ~= 2
        error('ParamMatrix must have 2 columns: [ZenithAngle_deg, Pressure_mbar]');
    end

    % Extract parameters
    ZenithAngles = ParamMatrix(:, 1);  % Column vector
    Pressure = ParamMatrix(:, 2);      % Column vector
    NumParamSets = size(ParamMatrix, 1);
    NumWavelengths = length(Lambda);

    % Validate zenith angles
    if any(ZenithAngles > 90 | ZenithAngles < 0)
        error('Zenith angles out of range [0, 90] degrees');
    end

    % Initialize result matrix
    Result = zeros(NumWavelengths, NumParamSets);

    % Calculate transmission for each parameter set
    for i = 1:NumParamSets
        % Calculate airmass for Rayleigh scattering
        Airmasses = astro.transmission.airmassSMARTS(ZenithAngles(i));
        Am_rayleigh = Airmasses.rayleigh;

        % Calculate Rayleigh optical depth using AstroPack rayleighScattering
        TauRayleigh = astro.atmosphere.rayleighScattering(Lambda, Pressure(i), 'nm');

        % Calculate transmission
        Result(:, i) = exp(-Am_rayleigh .* TauRayleigh);
    end
end