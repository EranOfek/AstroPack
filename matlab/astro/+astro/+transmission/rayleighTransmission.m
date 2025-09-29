function Transm = rayleighTransmission(ZenithAngle_deg, Pressure_mbar, Lam, WaveUnits)
    % Calculates Rayleigh transmission, returns cashed result if the inputs
    % did not change since last call. Part of the Transmission package for absolute photometric calibration.  
    % Input:  - zenithAngle_deg (double): Zenith angle in degrees [0, 90] (optional if cached)
    %         - pressure_mbar (double): Atmospheric pressure in mbar (default: 965, optional if cached)
    %         - Lam (double array): Wavelength array in nm (optional if cached)
    % Output: - Transm (double array): Transmission values (0-1)
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author: D. Kovaleva (Sep 2025)
    % Example: Transm = astro.atmosphere.rayleighTransmission(55.18, 1013.25);
    %          % Later calls with the same arguments
    %          % return output without calculation
    %          Transm = astro.atmosphere.rayleighTransmission(55.18, 1013.25);
    
    arguments
        ZenithAngle_deg = 30
        Pressure_mbar = 965
        Lam = linspace(300, 1100, 401);
        WaveUnits = 'nm'
    end

    persistent cachedTransm cachedZenith cachedPressure cachedLam

    % Validate zenith angle
    if ZenithAngle_deg > 90 || ZenithAngle_deg < 0
        error('Zenith angle out of range [0, 90] deg');
    end

    % Check if we can use cached data (same inputs)
    if ~isempty(cachedTransm) && isequal(ZenithAngle_deg, cachedZenith) && ...
            isequal(Pressure_mbar, cachedPressure) && isequal(Lam, cachedLam)
        Transm = cachedTransm;
        return;
    end
    
    Am_ = astro.atmosphere.airmassFromSMARTS(ZenithAngle_deg).rayleigh;
    
    % Calculate Rayleigh optical depth using AstroPack function
    % rayleighScatering
    Tau_rayleigh = astro.atmosphere.rayleighScattering(Lam, Pressure_mbar, WaveUnits);
    
    % Calculate transmission
    Transm = exp(-Am_ .* Tau_rayleigh);

    % Cache the results
    cachedTransm = Transm;
    cachedZenith = ZenithAngle_deg;
    cachedPressure = Pressure_mbar;
    cachedLam = Lam;
end
