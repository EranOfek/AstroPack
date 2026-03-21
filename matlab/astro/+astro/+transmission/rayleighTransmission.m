function Result = rayleighTransmission(Lambda, ParamMatrix, Args)
    % Rayleigh transmission of the Earth atmosphere
    % Input  : - Lambda (double array): Wavelength array in Angstrom.
    %            If GetArgNames flag is true, returns ArgNames structure for parameters.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is
    %            [ZenithAngle_deg, Pressure_mbar].
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'GetArgNames' - Return ArgNames structure instead of calculating. Default is false.
    % Output : - Result (double matrix): Transmission matrix (wavelengths x parameter_sets)
    %            OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % Example:
    %   % Basic usage:
    %   Lambda = linspace(3000, 11000, 401)';
    %   ParamMatrix = [45, 1013; 60, 950; 30, 1020];  % Multiple parameter sets
    %   Result = astro.transmission.rayleighTransmission(Lambda, ParamMatrix);
    %
    %   % Get parameter information:
    %   ArgNames = astro.transmission.rayleighTransmission('GetArgNames', true);
    %   % Returns: struct with Name={1,2}, Description={'ZenithAngle_deg','Pressure_mbar'},
    %   %          Min={0,800}, Max={90,1100}
    %
    %   % Usage with CompositeFun:
    %   Model = astro.transmission.CompositeFun();
    %   Model.addFun('Rayleigh scattering', @astro.transmission.rayleighTransmission, [], 'Par', [45, 1013]);

    arguments
        Lambda      = linspace(3000, 11000, 401)'
        ParamMatrix = [30, 965]               % [ZenithAngle_deg, Pressure_mbar]
        Args.AbsorptionData = []
        Args.Return = []
        Args.UsePersistentCache logical = true   % Enable/disable persistent cache
        Args.Tolerance = 1e-12                   % Parameter comparison tolerance
        Args.GetArgNames logical = false         % Return ArgNames structure instead of calculating
    end

    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {1, 2}, ...
                       'Description', {'ZenithAngle_deg', 'Pressure_mbar'}, ...
                       'Min', {0, 800}, ...
                       'Max', {90, 1100});
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

    % Calculate airmasses for all zenith angles at once (vectorized)
    Airmasses = astro.transmission.airmassSMARTS(ZenithAngles);
    Am_rayleigh = Airmasses.rayleigh(:)';  % [1 x N]

    % Compute optical depth per unique pressure, then broadcast with airmass
    UniqP = unique(Pressure);
    Result = zeros(NumWavelengths, NumParamSets);
    for ip = 1:numel(UniqP)
        Mask = (Pressure == UniqP(ip));
        TauRayleigh = astro.atmosphere.rayleighScattering(Lambda, UniqP(ip), 'Ang');  % [Nwave x 1]
        Result(:, Mask) = exp(-Am_rayleigh(Mask) .* TauRayleigh);  % implicit expansion
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