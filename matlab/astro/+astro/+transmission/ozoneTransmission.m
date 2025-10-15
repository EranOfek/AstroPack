function Result = ozoneTransmission(Lambda, ParamMatrix, Args)
    % Ozone transmission of the Earth atmosphere
    % Input :    Lambda      - Wavelength array (nm), column vector
    %            ParamMatrix - Parameter matrix where each row is [ZenithAngle_deg, DobsonUnits]
    %                          ZenithAngle_deg: Solar zenith angle in degrees [0-90]
    %                          DobsonUnits: Total ozone column in Dobson units [typically 200-500]
    %          * ...,key,val,...
    %             'AbsorptionData': Pre-loaded AbsorptionData object for fast interpolation
    %             'Return': Pre-computed results for caching (external
    %             cache pattern). Default is [].
    % Output :   Result - Transmission matrix (wavelengths × parameter_sets) [0-1]
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author: D. Kovaleva (Oct 2025)
    % Example:
    %   AbsData = astro.transmission.AbsorptionData();
    %   Lambda = linspace(300, 1100, 401)';
    %   ParamMatrix = [45, 300; 60, 280; 30, 320];
    %   Result = astro.transmission.ozoneTransmission(Lambda, ParamMatrix, 'AbsorptionData', AbsData);

    arguments
        Lambda        = linspace(300,1100,401)'
        ParamMatrix   = [30,300]                 % [ZenithAngle_deg, DobsonUnits]
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
        error('ParamMatrix must have 2 columns: [ZenithAngle_deg, DobsonUnits]');
    end

    % Create or validate AbsorptionData object
    if isempty(Args.AbsorptionData)
        Args.AbsorptionData = astro.transmission.AbsorptionData();
    end

    % Extract parameters
    ZenithAngles = ParamMatrix(:, 1);  % Column vector
    DobsonUnits = ParamMatrix(:, 2);   % Column vector
    NumParamSets = size(ParamMatrix, 1);
    NumWavelengths = length(Lambda);

    % Validate zenith angles
    if any(ZenithAngles > 90 | ZenithAngles < 0)
        error('Zenith angles out of range [0, 90] degrees');
    end

    % Convert Dobson units to atm-cm
    OzoneAtmCm = DobsonUnits * 0.001;

    % Get ozone absorption cross-sections using AbsorptionData object
    OzoneCrossSection = Args.AbsorptionData.getInterpolated('O3UV', Lambda);

    % Loschmidt scaling
    Absorption_coeff = constant.Loschmidt * OzoneCrossSection;

    % Initialize result matrix
    Result = zeros(NumWavelengths, NumParamSets);

    % Calculate transmission for each parameter set
    for i = 1:NumParamSets
        % Calculate airmass for ozone
        Airmasses = astro.transmission.airmassSMARTS(ZenithAngles(i));
        Am_ozone = Airmasses.ozone;

        % Calculate optical depth
        TauOzone = Absorption_coeff * OzoneAtmCm(i);

        % Calculate transmission
        Result(:, i) = exp(-Am_ozone .* TauOzone);
    end
end