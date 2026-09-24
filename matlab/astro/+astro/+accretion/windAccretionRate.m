function [Mdot] = windAccretionRate(Ms, Mx, Rs, P, MdotW, Args)
    % The accretion rate from a wind
    %   Approximation for Ms>>Mx
    % Input  : - Mass of the primary [solar mass]
    %          - Mass of the secondary (accretor) [solar mass].
    %            This approximation is correct for Ms>>Mx
    %          - Radius of the primary [solar radius]
    %          - Period [days]
    %          - Wind mass loss of the primary [solar mass/yr]
    %          * ...,key,val,... 
    %            'Eta' - Default is 1.
    %            'Eps' - Default is 1.
    % Output : - Accretion rate of the secondary [solar mass/yr]
    % Author : Eran Ofek (2026 Jan) 
    % Example: [Mdot] = astro.accretion.windAccretionRate(30, 1.4, 30, 1,  1e-6);

    arguments
        Ms
        Mx
        Rs
        P
        MdotW
        Args.Eta      = 1;
        Args.Eps      = 1;
    end

    % [14.3.32]
    Mdot = 7e-9 .*Args.Eps.^2 .* Args.Eta.^-4 .* (Mx./10).^2 .* (Ms./30).^(-8./3) .* (Rs./20).^2 .* (P./5.6).^(-4./3) .* (MdotW./1e-6);


end
