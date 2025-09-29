function Constituent_Airmasses = airmassSMARTS(ZenithAngle_deg)
    % Calculate airmasses for all atmospheric constituents with caching.
    % Returns cashed data when called with empty input. Part of the
    % Transmission package for absolute photometric calibration.
    % Constituents available are: 'rayleigh'; 'aerosol'; 'o3'/'ozone';
    % 'h2o'/'water'; 'o2'; 'ch4'; 'co'; 'n2o'; 'co2'; 'n2'; 'hno3';
    % 'no2'; 'no'; 'so2'; 'nh3'.
    % Based on SMARTS2.9.5 tabulated values. 
    % Input    : - Zenith angle in degrees (optional, on first call)
    % Output   : - Constituent_Airmasses: Structure with fields for all atmospheric Constituents
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author   : D. Kovaleva (Sep 2025)
    % Example  : First call:  Constituent_Airmasses = astro.atmosphere.airmassFromSMARTS(45);  % Calculate for 45 degrees
    %            Later calls: Constituent_Airmasses = astro.atmosphere.airmassFromSMARTS();    % Return cached values
    
    persistent cachedAirmasses cachedZenithAngle

    % If no input or empty input, return cached data
    if nargin == 0 || isempty(ZenithAngle_deg)
        if isempty(cachedAirmasses)
            error('No cached airmass data. Call with zenithAngle_deg first.');
        end
        Constituent_Airmasses = cachedAirmasses;
        return;
    end

    % Validate zenith angle
    if ZenithAngle_deg > 90 || ZenithAngle_deg < 0
        error('Zenith angle out of range [0, 90] deg');
    end

    % Check if we already have cached data for this exact zenith angle
    if ~isempty(cachedAirmasses) && ~isempty(cachedZenithAngle) && cachedZenithAngle == ZenithAngle_deg
        Constituent_Airmasses = cachedAirmasses;
        return;
    end

    % Define coefficients for different atmospheric constituents (from SMARTS2.9.5)
    Coefs = containers.Map();
    Coefs('rayleigh') = [0.48353, 0.095846,  96.741, -1.754];
    Coefs('aerosol')  = [0.16851, 0.18198,   95.318, -1.9542];
    Coefs('ozone')    = [1.0651,  0.6379,   101.8,   -2.2694];
    Coefs('water')    = [0.10648, 0.11423,   93.781, -1.9203];
    Coefs('o2')       = [0.65779, 0.064713,  96.974, -1.8084];
    Coefs('ch4')      = [0.49381, 0.35569,   98.23,  -2.1616];
    Coefs('co')       = [0.505,   0.063191,  95.899, -1.917];
    Coefs('n2o')      = [0.61696, 0.060787,  96.632, -1.8279];
    Coefs('co2')      = [0.65786, 0.064688,  96.974, -1.8083];
    Coefs('n2')       = [0.38155, 8.871e-05, 95.195, -1.8053];
    Coefs('hno3')     = [1.044,   0.78456,  103.15,  -2.4794];
    Coefs('no2')      = [1.1212,  1.6132,   111.55,  -3.2629];
    Coefs('no')       = [0.77738, 0.11075,  100.34,  -1.5794];
    Coefs('so2')      = [0.63454, 0.00992,   95.804, -2.0573];
    Coefs('nh3')      = [0.32101, 0.010793,  94.337, -2.0548];

    % Get all constituent names
    ConstituentNames = keys(Coefs);

    % Calculate airmass for each constituent
    Constituent_Airmasses = struct();
    Cosz = cos(deg2rad(ZenithAngle_deg));

    for i = 1:length(ConstituentNames)
        Constituent = ConstituentNames{i};
        P = Coefs(Constituent);

        % SMARTS airmass formula
        airmass = 1 / (Cosz + P(1) * (ZenithAngle_deg^P(2)) * (P(3) - ZenithAngle_deg)^P(4));

        % Store in structure
        Constituent_Airmasses.(Constituent) = airmass;
    end

    % Cache the results
    cachedAirmasses = Constituent_Airmasses;
    cachedZenithAngle = ZenithAngle_deg;
end