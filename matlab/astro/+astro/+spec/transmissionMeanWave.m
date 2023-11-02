function Result=transmissionMeanWave(Lambda, Response, Dim)
    % Calculate the mean wavelength of a transmission function
    % Input  : - Array of wavelengths.
    %          - Array of transmissions.
    %          - Dimension over which to perform the integration.
    % Output : - Mean wavelength for each transmission.
    % Author : Eran Ofek (Oct 2023)
    % Reference: See Bohlin et al. 2014 for definitions
    % Example: astro.spec.transmissionMeanWave((1:1:10)',rand(10,3))
    
    arguments
        Lambda
        Response
        Dim       = 1;
    end
    
    Result = trapz(Lambda, Lambda.*Response, Dim)./trapz(Lambda, Response, Dim);
    
end