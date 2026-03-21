function Constituent_Airmasses = airmassSMARTS(ZenithAngle_deg)
    % Calculate airmasses for all atmospheric constituents with caching.
    % Returns cached data when called with empty input. Part of the
    % Transmission package for absolute photometric calibration.
    % Accepts scalar or vector ZenithAngle_deg input.
    % Constituents available are: 'rayleigh'; 'aerosol'; 'o3'/'ozone';
    % 'h2o'/'water'; 'o2'; 'ch4'; 'co'; 'n2o'; 'co2'; 'n2'; 'hno3';
    % 'no2'; 'no'; 'so2'; 'nh3'.
    % Based on SMARTS2.9.5 tabulated values.
    % Input    : - Zenith angle in degrees, scalar or vector (optional, on first call)
    % Output   : - Constituent_Airmasses: Structure with fields for all
    %              atmospheric constituents. Each field has the same size as
    %              ZenithAngle_deg.
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author   : D. Kovaleva (Sep 2025)
    % Example  : Constituent_Airmasses = astro.transmission.airmassSMARTS(45);
    %            Constituent_Airmasses = astro.transmission.airmassSMARTS([30; 45; 60]);
    %            Constituent_Airmasses = astro.transmission.airmassSMARTS();  % Return cached

    persistent cachedAirmasses cachedZenithAngle

    % If no input or empty input, return cached data
    if nargin == 0 || isempty(ZenithAngle_deg)
        if isempty(cachedAirmasses)
            error('No cached airmass data. Call with zenithAngle_deg first.');
        end
        Constituent_Airmasses = cachedAirmasses;
        return;
    end

    % Validate zenith angles
    if any(ZenithAngle_deg > 90 | ZenithAngle_deg < 0)
        error('Zenith angle out of range [0, 90] deg');
    end

    % Check if we already have cached data for these exact zenith angles
    if ~isempty(cachedAirmasses) && ~isempty(cachedZenithAngle) && ...
            isequal(size(cachedZenithAngle), size(ZenithAngle_deg)) && ...
            all(cachedZenithAngle(:) == ZenithAngle_deg(:))
        Constituent_Airmasses = cachedAirmasses;
        return;
    end

    % Coefficients for atmospheric constituents (from SMARTS2.9.5)
    % Each row: [a, b, c, d] for formula: 1 ./ (cos(z) + a .* z.^b .* (c - z).^d)
    Names = {'aerosol','ch4','co','co2','hno3','n2','n2o','nh3','no','no2','o2','ozone','rayleigh','so2','water'};
    C = [0.16851, 0.18198,   95.318, -1.9542;   % aerosol
         0.49381, 0.35569,   98.23,  -2.1616;   % ch4
         0.505,   0.063191,  95.899, -1.917;     % co
         0.65786, 0.064688,  96.974, -1.8083;    % co2
         1.044,   0.78456,  103.15,  -2.4794;    % hno3
         0.38155, 8.871e-05, 95.195, -1.8053;    % n2
         0.61696, 0.060787,  96.632, -1.8279;    % n2o
         0.32101, 0.010793,  94.337, -2.0548;    % nh3
         0.77738, 0.11075,  100.34,  -1.5794;    % no
         1.1212,  1.6132,   111.55,  -3.2629;    % no2
         0.65779, 0.064713,  96.974, -1.8084;    % o2
         1.0651,  0.6379,   101.8,   -2.2694;    % ozone
         0.48353, 0.095846,  96.741, -1.754;     % rayleigh
         0.63454, 0.00992,   95.804, -2.0573;    % so2
         0.10648, 0.11423,   93.781, -1.9203];   % water

    % Vectorized SMARTS airmass formula for all constituents
    InputSize = size(ZenithAngle_deg);
    Cosz = cos(deg2rad(ZenithAngle_deg(:)));  % [N x 1]
    ZA   = ZenithAngle_deg(:);               % [N x 1]

    Constituent_Airmasses = struct();
    for i = 1:numel(Names)
        a = C(i,1); b = C(i,2); c = C(i,3); d = C(i,4);
        Am = 1 ./ (Cosz + a .* (ZA.^b) .* ((c - ZA).^d));
        Constituent_Airmasses.(Names{i}) = reshape(Am, InputSize);
    end

    % Cache the results
    cachedAirmasses = Constituent_Airmasses;
    cachedZenithAngle = ZenithAngle_deg;
end