function [DF,ObsToRestFactor] = dopplerFactor(Gamma, Theta, Args)
    % Calculate the Doppler factor
    %   Given a jet moving at lorentz Factor \Gamma at angle \theta from the line of sight
    %   get the doppler factor and the conversion between the time in the rest fram of
    %   particles in the jet and the observed frame.
    % Input  : - Gamma (or beta, if IsBeta=true). 
    %          - Theta [deg].
    %          * ...,key,val,...
    %            'IsBeta' - Default is false.
    %            'z' - Redshift. Default is 0.
    % Output : - The Dopppler factor: 1./(Gamma.*(1 - Beta.*cosd(Theta)))
    %          - The Conversion factor to convert from the observer frane
    %          to the rest frame: (1+z)/DF.
    % Author : Eran Ofek (2025 Nov) 
    % Example: astro.grb.dopplerFactor(10,1)

    arguments
        Gamma
        Theta        = 0;
        Args.IsBeta  = false;
        Args.z       = 0;
    end

    if Args.IsBeta
        Beta  = Gamma;
        Gamma = (1-Beta.^2).^(-0.5);
    else
        Beta  = sqrt(1 - Gamma.^(-2));
    end

    DF = 1./(Gamma.*(1 - Beta.*cosd(Theta)));
   
    ObsToRestFactor = (1+Args.z)./DF;

end
