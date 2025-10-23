function Result = aerosolTransmission(Lambda, ParamMatrix, Args)
    % Aerosol transmission of the Earth atmosphere
    % Input  : - Lambda (double array): Wavelength array in nm.
    %            If GetArgNames flag is true, returns ArgNames structure for parameters.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is
    %            [ZenithAngle_deg, TauAod500, AngstromExponent].
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'GetArgNames' - Return ArgNames structure instead of calculating. Default is false.
    % Output : - Result (double matrix): Transmission matrix (wavelengths x parameter_sets)
    %            OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % Examples:
    %   % Basic usage:
    %   Lambda = linspace(300, 1100, 401)';
    %   ParamMatrix = [45, 0.1, 0.6; 60, 0.2, 1.1; 30, 0.05, 1.2];  % Multiple parameter sets
    %   Result = astro.transmission.aerosolTransmission(Lambda, ParamMatrix);
    %
    %   % Get parameter information:
    %   ArgNames = astro.transmission.aerosolTransmission('GetArgNames', true);
    %   % Returns: struct with Name={1,2,3}, Description={'ZenithAngle_deg','TauAod500','AngstromExponent'},
    %   %          Min={0,0.01,0.5}, Max={90,0.5,2.0}
    %
    %   % Usage with CompositeFun:
    %   Model = astro.transmission.CompositeFun();
    %   Model.addFun('Aerosol scattering', @astro.transmission.aerosolTransmission, [], 'Par', [45, 0.08, 1.2]);

    arguments
        Lambda      = linspace(300,1100,401)'
        ParamMatrix = [30, 0.085, 0.6]            %  [ZenithAngle_deg, TauAod500, AngstromExponent]
        Args.AbsorptionData = []
        Args.Return = []
        Args.UsePersistentCache logical = true   % Enable/disable persistent cache
        Args.Tolerance = 1e-12                   % Parameter comparison tolerance
        Args.GetArgNames logical = false         % Return ArgNames structure instead of calculating
    end

    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {1, 2, 3}, ...
                       'Description', {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'}, ...
                       'Min', {0, 0.01, 0.5}, ...
                       'Max', {90, 0.5, 2.0});
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
    if size(ParamMatrix, 2) ~= 3
        error('ParamMatrix must have 3 columns: [ZenithAngle_deg, Tau_aod500, ZngstromExponent]');
    end

    % Extract parameters
    ZenithAngles = ParamMatrix(:, 1);      % Column vector
    TauAod500 = ParamMatrix(:, 2);         % Column vector
    AngstromExponent = ParamMatrix(:, 3);  % Column vector
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
        % Calculate airmass for aerosol
        Airmasses = astro.transmission.airmassSMARTS(ZenithAngles(i));
        Am_aerosol = Airmasses.aerosol;

        % Calculate aerosol optical depth using Angstrom law
        TauLambda = astro.atmosphere.aerosolScattering(Lambda, TauAod500(i), AngstromExponent(i), 'nm');

        % Calculate transmission
        Result(:, i) = exp(-Am_aerosol .* TauLambda);
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