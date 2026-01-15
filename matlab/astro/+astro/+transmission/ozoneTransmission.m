function Result = ozoneTransmission(Lambda, ParamMatrix, Args)
    % Ozone transmission of the Earth atmosphere
    % Input :   - Lambda - Wavelength array (Angstrom), column vector
    %                     If empty, returns ArgNames structure for parameters
    %           - ParamMatrix - Parameter matrix where each row is [ZenithAngle_deg, DobsonUnits]
    %                          ZenithAngle_deg: Solar zenith angle in degrees [0-90]
    %                          DobsonUnits: Total ozone column in Dobson units [typically 200-500]
    %          * ...,key,val,...
    %             'AbsorptionData': Pre-loaded AbsorptionData object for fast interpolation
    %             'Return': Pre-computed results for caching (external
    %             cache pattern). Default is [].
    % Output :  - Result - Transmission matrix (wavelengths × parameter_sets) [0-1]
    %                     OR ArgNames structure if Lambda is empty
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author: D. Kovaleva (Oct 2025)
    % Example:
    %   AbsData = astro.transmission.loadAbsorptionInterpolantsSMARTS();
    %   Lambda = linspace(3000, 11000, 401)';
    %   ParamMatrix = [45, 300; 60, 280; 30, 320];  % Multiple parameter sets
    %   Result = astro.transmission.ozoneTransmission(Lambda, ParamMatrix);
    %
    %   % With pre-loaded absorption data for better performance:
    %   AbsData = astro.transmission.loadAbsorptionInterpolantsSMARTS();
    %   Result = astro.transmission.ozoneTransmission(Lambda, ParamMatrix, 'AbsorptionData', AbsData);
    %
    %   % Get parameter information:
    %   ArgNames = astro.transmission.ozoneTransmission('GetArgNames', true);
    %   % Returns: struct with Name={1,2}, Description={'ZenithAngle_deg','DobsonUnits'}, Min={0,200}, Max={90,500}
    %
    %   % Usage with CompositeFun:
    %   Model = astro.transmission.CompositeFun();
    %   Model.addFun('Ozone absorption', @astro.transmission.ozoneTransmission, [], 'Par', [45, 300]);

    arguments
        Lambda        = linspace(3000,11000,401)'
        ParamMatrix   = [30,300]                 % [ZenithAngle_deg, DobsonUnits]
        Args.AbsorptionData = []
        Args.Return = []
        Args.UsePersistentCache logical = true   % Enable/disable persistent cache
        Args.Tolerance = 1e-12                   % Parameter comparison tolerance
        Args.GetArgNames logical = false         % Return ArgNames structure instead of calculating
    end

    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {1, 2}, ...
                       'Description', {'ZenithAngle_deg', 'DobsonUnits'}, ...
                       'Min', {0, 200}, ...
                       'Max', {90, 500});
        return;
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    % Persistent cache 
    persistent CachedResult CachedParams CachedLambda

    if Args.UsePersistentCache && ~isempty(CachedResult)
        % Check if parameters and wavelengths match within tolerance
        if compareParams(Lambda, CachedLambda, Args.Tolerance) && ...
           compareParams(ParamMatrix, CachedParams, Args.Tolerance)
            Result = CachedResult;
            return;
        end
    end

    % Validate input dimensions
    if size(ParamMatrix, 2) ~= 2
        error('ParamMatrix must have 2 columns: [ZenithAngle_deg, DobsonUnits]');
    end

    % Create or validate AbsorptionData object
    if isempty(Args.AbsorptionData)
        Args.AbsorptionData = astro.transmission.loadAbsorptionInterpolantsSMARTS();
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

    % Store in persistent cache if enabled
    if Args.UsePersistentCache
        CachedResult = Result;
        CachedParams = ParamMatrix;
        CachedLambda = Lambda;
    end
end

function isSame = compareParams(ParamsCurrent, ParamsCached, Tolerance)
    % Compare numerical parameters with tolerance
    if nargin < 3
        Tolerance = 1e-12;
    end
    isSame = isequal(size(ParamsCurrent), size(ParamsCached)) && ...
             all(abs(ParamsCurrent(:) - ParamsCached(:)) < Tolerance);
end