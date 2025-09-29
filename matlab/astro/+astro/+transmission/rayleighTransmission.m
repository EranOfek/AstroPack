function Transm = rayleighTransmission(Lam, ModelPar, Args)
% function Transm = rayleighTransmission(ZenithAngle_deg, Pressure_mbar, Lam, WaveUnits, Args)
    % Calculates Rayleigh transmission, returns cashed result if the inputs
    % did not change since last call. Part of the Transmission package for absolute photometric calibration.  
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
        % ZenithAngle_deg = 30
        % Pressure_mbar = 965
        % WaveUnits = 'nm'
        Lam = linspace(300, 1100, 401);
        ModelPar    = {{'ZenithAngle_deg',30},{'Pressure_mbar',965},{'WaveUnits','nm'}}; 
        Args.Result = [];        
    end

    persistent cachedTransm cachedZenith cachedPressure cachedLam

    if isempty(Args.Result) 

        ZenithAngle_deg = ModelPar{cellfun(@(c) strcmp(c{1}, 'ZenithAngle_deg'), ModelPar)}{2};
        Pressure_mbar   = ModelPar{cellfun(@(c) strcmp(c{1}, 'Pressure_mbar'), ModelPar)}{2};
        WaveUnits       = ModelPar{cellfun(@(c) strcmp(c{1}, 'WaveUnits'), ModelPar)}{2};
      
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
        
        Am_ = astro.transmission.airmassSMARTS(ZenithAngle_deg).rayleigh;
        
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
    else
        Transm = Args.Result;
    end
end
