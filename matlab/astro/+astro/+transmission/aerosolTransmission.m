function Transm = aerosolTransmission(zenithAngle_deg, tau_aod500, alpha, Lam, waveUnits)
    % Input :  - zenithAngle_deg (double): Zenith angle in degrees [0, 90] 
    %          - tau_aod500 (double): Aerosol optical depth at 500nm 
    %          - alpha (double): Angstrom exponent 
    %          - Lam (double array): Wavelength array
    %          - waveUnits (string): Wavelength units 
    % Output : - Transm (double array): Transmission values (0-1)
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Author: D. Kovaleva (Sep 2025)
    % Example: Trans = astro.transmission.aerosolTransmission(55.18, 0.1, 1.3);
    %          % Later calls with same arguments return cached result
    %          Trans = astro.transmission.aerosolTransmission(55.18, 0.1, 1.3); 

    arguments
        zenithAngle_deg = 30
        tau_aod500 = 0.1
        alpha  = 1.3
        Lam = linspace(300, 1100, 401);
        waveUnits string = 'nm'
    end

    persistent cachedTransm cachedZenith cachedTau cachedAlpha cachedLam cachedUnits

    % Check if we can use cached data (same inputs)
    if ~isempty(cachedTransm) && isequal(zenithAngle_deg, cachedZenith) && ...
            isequal(tau_aod500, cachedTau) && isequal(alpha, cachedAlpha) && ...
            isequal(Lam, cachedLam) && isequal(waveUnits, cachedUnits)
        Transm = cachedTransm;
        return;
    end

    % Validate zenith angle
    if zenithAngle_deg > 90 || zenithAngle_deg < 0
        error('Zenith angle out of range [0, 90] deg');
    end

    % Calculate airmass 
    Am_ = astro.transmission.airmassSMARTS(zenithAngle_deg).aerosol;
        
    % Calculate aerosol optical depth using AstroPack aerosolScattering
    Tau_aerosol = astro.atmosphere.aerosolScattering(Lam, tau_aod500, alpha, waveUnits);
    
    % Calculate transmission
    Transm = exp(-Am_ .* Tau_aerosol);

    % Cache the results
    cachedTransm = Transm;
    cachedZenith = zenithAngle_deg;
    cachedTau = tau_aod500;
    cachedAlpha = alpha;
    cachedLam = Lam;
    cachedUnits = waveUnits;
end