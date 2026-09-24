function [Rs] = transonicRadius(Mass, Ainf, Gamma, Args)
    % Calculate te transonic radius for a spherical accretion at zero velocity.
    % Input  : - Mass of accreator (central object).
    %            Default units are [solar mass]
    %            Default is 1 solar mass.
    %          - Speed of sound at infinity [cm/s]. Default is 1e6.
    %          - Adiabatic index Gamma. Default is 5./3.
    %          * ...,key,val,... 
    %            'MassUnits' - Mass units. Default is 'SunM'.
    %            'LengthUnits' - Length units of velocity.
    %                   Default is 'cm'.
    %            'OutUnits' - Output units. Default is 'cm'.
    % Output : - Transonic point radius (Default units is 'cm').
    % Author : Eran Ofek (2026 Jan) 
    % Example: Rs=astro.accreation.transonicRadius(1,1e6)

    arguments
        Mass     = 1;
        Ainf     = 1e6;
        Gamma    = 4./3;
        Args.MassUnits         = 'SunM';
        Args.LengthUnits       = 'cm';
        Args.OutUnits          = 'cm';
    end

    Mass = convert.mass(Args.MassUnits, 'gr', Mass);
    Ainf = convert.length(Args.LengthUnits, 'cm', Ainf);

    Rs = (5 - 3.*Gamma).*constant.G.*Mass./(4.*Ainf.^2);

    Rs = convert.length('cm', Args.OutUnits, Rs);


end
