function [Tau] = aerosolScattering(Lam, Tau_a500, Alpha, LamUnits)
    % Approximate Aerosol Scattering absorption in the Earth's atmosphere as a function of wavelength
    % Input  : - An array of wavelength.
    %          - Tau optical depth at 500nm. Default is 0.5.
    %          - Alpha (power law). Default is 0.6.
    %          - Wavelength units. Default is 'Ang'.
    % Output : - An array of optical depth (tau) due to Rayleigh
    %            scattering.
    % Author : Eran Ofek (2024 Oct) 
    % Example: Tau = astro.atmosphere.aerosolScattering(5000);

    arguments
        Lam
        Tau_a500      = 0.5;
        Alpha         = 0.6;
        LamUnits      = 'Ang';
    end
    
    Lam = convert.length(LamUnits, 'micrometer', Lam);
    
    Tau = Tau_a500.*(2.*Lam).^(-Alpha);
    
end
