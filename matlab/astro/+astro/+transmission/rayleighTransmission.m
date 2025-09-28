function Transm = rayleighTransmission(zenithAngle_deg, pressure_mbar, Lam, waveUnits)
    % Rayleigh transmission with caching 
    % Input:  - zenithAngle_deg (double): Zenith angle in degrees [0, 90] (optional if cached)
    %         - pressure_mbar (double): Atmospheric pressure in mbar (default: 965, optional if cached)
    %         - Lam (double array): Wavelength array in nm (optional if cached)
    % Output: - Transm (double array): Transmission values (0-1)
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author: D. Kovaleva (Sep 2025)
    % Example: Transm = astro.transmission.rayleighTransmission(55.18, 1013.25);
    %          % Later calls with the same arguments
    %          % return output without calculation
    %          Transm = astro.transmission.rayleighTransmission(55.18, 1013.25);    
    arguments
        zenithAngle_deg = 30;
        pressure_mbar = 965;
        Lam = linspace(300, 1100, 401);
        waveUnits = 'nm'
    end

    persistent cachedTransm cachedZenith cachedPressure cachedLam

    % Validate zenith angle
    if zenithAngle_deg > 90 || zenithAngle_deg < 0
        error('Zenith angle out of range [0, 90] deg');
    end

    % Check if we can use cached data (same inputs)
    if ~isempty(cachedTransm) && isequal(zenithAngle_deg, cachedZenith) && ...
            isequal(pressure_mbar, cachedPressure) && isequal(Lam, cachedLam)
        Transm = cachedTransm;
        return;
    end
    
    Am_ = astro.transmission.airmassSMARTS(zenithAngle_deg).rayleigh;
    
    % Calculate Rayleigh optical depth using AstroPack function
    % rayleighScatering
    Tau_rayleigh = astro.atmosphere.rayleighScattering(Lam, pressure_mbar, waveUnits);
    
    % Calculate transmission
    Transm = exp(-Am_ .* Tau_rayleigh);

    % Cache the results
    cachedTransm = Transm;
    cachedZenith = zenithAngle_deg;
    cachedPressure = pressure_mbar;
    cachedLam = Lam;
end
