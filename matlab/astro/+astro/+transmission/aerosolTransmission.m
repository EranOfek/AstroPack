function Transm = aerosolTransmission(ZenithAngle_deg, Tau_aod500, Alpha, Lam, WaveUnits)
    % Calculates aerosol transmission, returns cashed result if the inputs
    % did not change since last call. Part of the Transmission package for absolute photometric calibration. 
    % Input :  - zenithAngle_deg (double): Zenith angle in degrees [0, 90] 
    %          - tau_aod500 (double): Aerosol optical depth at 500nm 
    %          - alpha (double): Angstrom exponent 
    %          - Lam (double array): Wavelength array
    %          - waveUnits (string): Wavelength units 
    % Output : - Transm (double array): Transmission values (0-1)
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author: D. Kovaleva (Sep 2025)
    % Example: Trans = astro.atmosphere.aerosolTransmission(55.18, 0.1, 1.3);
    %          % Later calls with same arguments return cached result
    %          Trans = astro.atmosphere.aerosolTransmission(55.18, 0.1, 1.3); 

    arguments
        ZenithAngle_deg = 30
        Tau_aod500 = 0.1
        Alpha  = 1.3
        Lam = linspace(300, 1100, 401);
        WaveUnits string = 'nm'
    end

    persistent cachedTransm cachedZenith cachedTau cachedAlpha cachedLam cachedUnits

    % Check if we can use cached data (same inputs)
    if ~isempty(cachedTransm) && isequal(ZenithAngle_deg, cachedZenith) && ...
            isequal(Tau_aod500, cachedTau) && isequal(Alpha, cachedAlpha) && ...
            isequal(Lam, cachedLam) && isequal(WaveUnits, cachedUnits)
        Transm = cachedTransm;
        return;
    end

    % Validate zenith angle
    if ZenithAngle_deg > 90 || ZenithAngle_deg < 0
        error('Zenith angle out of range [0, 90] deg');
    end

    % Calculate airmass 
    Am_ = astro.atmosphere.airmassFromSMARTS(ZenithAngle_deg).aerosol;
        
    % Calculate aerosol optical depth using AstroPack aerosolScattering
    Tau_aerosol = astro.atmosphere.aerosolScattering(Lam, Tau_aod500, Alpha, WaveUnits);
    
    % Calculate transmission
    Transm = exp(-Am_ .* Tau_aerosol);

    % Cache the results
    cachedTransm = Transm;
    cachedZenith = ZenithAngle_deg;
    cachedTau = Tau_aod500;
    cachedAlpha = Alpha;
    cachedLam = Lam;
    cachedUnits = WaveUnits;
end