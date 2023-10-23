function Result=transmissionEffWave(Lambda, Response, Flambda, Dim)
    % Calculate the effective wavelength of a transmission function
    % Input  : - Array of wavelengths.
    %          - Array of transmissions.
    %          - Array of F_lambda. Default is 1.
    %          - Dimension over which to perform the integration.
    % Output : - Mean wavelength for each transmission.
    % Author : Eran Ofek (Oct 2023)
    % Reference: See Bohlin et al. 2014 for definitions
    % Example: astro.spec.transmissionEffWave((1:1:10)',rand(10,3))
    
    arguments
        Lambda
        Response
        Flambda   = 1;
        Dim       = 1;
    end
    
    Result = trapz(Lambda, Flambda.*Lambda.^2.*Response, Dim)./trapz(Lambda, Flambda.*Lambda.*Response, Dim);
    
end