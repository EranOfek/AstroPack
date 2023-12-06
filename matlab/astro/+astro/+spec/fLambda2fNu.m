function Fnu=fLambda2fNu(Lambda, Flambda, LambdaUnits)
    % Convert specific flux in f_lambda to f_nu units
    % Input  : - Array of wavelength.
    %            Units are specified in LambdaUnits
    %          - Array of specic flux per unit wavelength [f_lambda]
    %            Units are energy/Area/s/LambdaUnits
    %            E.g., erg/cm^2/s/A
    %          - Lambda units. 
    %            See convert.length for options.
    %            Default is 'A'.
    % Output : - Specific flux per unit frequency.
    % Author : Eran Ofek (Oct 2023)
    % Example: Fnu=astro.spec.fLambda2fNu(5000, 1e-17)
    %          Flambda = astro.spec.fNu2fLambda(constant.c./5000e-8, Fnu)
    
    arguments
        Lambda
        Flambda                    % [erg/cm^2/s/A]
        LambdaUnits      = 'A';
    end
    C = 29979245800;  % [cm/s] constant.c
    Factor = convert.length(LambdaUnits,'cm');
    
    Fnu = Flambda.*Lambda.^2./C .* Factor;
    
    % Flambda = Fnu.*c/Lambda^2
    % Fnu     = Flambda.*Lambda^2./c
    
end

