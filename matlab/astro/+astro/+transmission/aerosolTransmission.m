function Result = aerosolTransmission(Lambda, ParamMatrix, Args)
    % Aerosol transmission of the Earth atmosphere 
    % Input  : - Lambda (double array): Wavelength array in nm.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is
    %            [ZenithAngle_deg, TauAod500, AngstromExponent]. 
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    % Output : - Result (double matrix): Transmission matrix (wavelengths x parameter_sets).
    % Author : D. Kovaleva (Oct 2025)
    % Example: Lambda = linspace(300, 1100, 401)';
    %          ParamMatrix = [45, 0.1, 0.6; 60, 0.2, 1.1;];
    %          Result = astro.transmission.aerosolTransmission(Lambda, ParamMatrix);

    arguments
        Lambda      = linspace(300,1100,401)
        ParamMatrix = [30, 0.085, 0.6]            %  [ZenithAngle_deg, TauAod500, AngstromExponent]
        Args.AbsorptionData = []
        Args.Return = []
    end
   
    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

 %   persistent cachedResult cachedParamMatrix

    % Check if we can use cached data (same inputs)
 %   if ~isempty(cachedResult) && isequal(ParamMatrix, cachedParamMatrix)
 %       Result = cachedResult;
 %       return;
 %   end

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
  % Cache the results
 % cachedParamMatrix = ParamMatrix;
 % cachedResult = Result;
end