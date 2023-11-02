function Flambda=fNu2fLambda(Nu, Fnu, LambdaUnits)
    % Convert specific flux in f_nu to f_lambda units
    % Input  : - Array of frequencies (nu)
    %            Units should be the same as F_nu (nu).
    %          - Array of specic flux per unit wavelength [f_nu]
    %            Units are energy/Area/s/Freq
    %            E.g., erg/cm^2/s/Hz
    %          - Units of Lambda in the F_lambda output.
    %            Default is 'A' - i.e., erg/cm^2/s/A
    % Output : - Specific flux per unit wavelength (in LambdaUnits).
    % Author : Eran Ofek (Oct 2023)
    % Example: Fnu=astro.spec.fLambda2fNu(5000, 1e-17)
    %          Flambda = astro.spec.fNu2fLambda(constant.c./5000e-8, Fnu)
    
    
    arguments
        Nu
        Fnu                    % [erg/cm^2/s/A]
        LambdaUnits    = 'A';
    end
    C = 29979245800;  % [cm/s] constant.c
    Factor = convert.length(LambdaUnits,'cm');
    
    %Flambda = Fnu.*Nu.^2./C;
    
    Lambda = C./Nu;
    Flambda = Fnu.*C./(Lambda.^2) .*Factor;
    
    % Flambda = Fnu.*c/Lambda^2 = Fnu*c/ (c/nu)^2
    % Fnu     = Flambda.*Lambda^2./c
    
end

