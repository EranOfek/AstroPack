function [Tau] = rayleighScattering(Lam, P, LamUnits)
    % Approximate Rayleigh Scattering absorption in the Earth's atmosphere as a function of wavelength
    % Input  : - An array of wavelength.
    %          - Surface pressure [mbar]. Default is 1000.
    %          - Wavelength units. Default is 'Ang'.
    % Output : - An array of optical depth (tau) due to Rayleigh
    %            scattering.
    % Author : Eran Ofek (2024 Oct) 
    % Example: Tau = astro.atmosphere.rayleighScattering(5000);

    arguments
        Lam
        P             = 1000;
        LamUnits      = 'Ang';
    end
    
    Lam = convert.length(LamUnits, 'micrometer', Lam);
    
    P0 = 1013.25;  % [mbar]
    Tau = P./( P0 .* (117.3405.*Lam.^4 - 1.5107.*Lam.^2 + 0.017535 - 0.00087743./(Lam.^2)));

end
