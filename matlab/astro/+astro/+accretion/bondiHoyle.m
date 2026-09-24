function [Mdot,Lambda_s,AccRadius] = bondiHoyle(Mass, Ninf, V, Ainf, Gamma, Args)
    % Bondi-Hoyle-Lyttleton spherical gas accreation
    % Input  : - Mass (default units 'SunM'). Default is .
    %          - Medium particle density [cm^-3]
    %          - Velocity of accreator (default units 'cm/s').
    %            Default is 30e5.
    %          - Speed of sound at infinity (default units 'cm/s').
    %            Default is 1e6.
    %          - Adiabatic index. Default is 5./3.
    %          * ...,key,val,... 
    %            'Mu' - Mean molecular weight. Default is 0.5.
    %            'MassUnits' - Mass units. Default is 'SunM'.
    %            'LengthUnits' - Length units of velocity.
    %                   Default is 'cm'.
    %            'OutUnits' - Output units:
    %                   'g/s' - gram/s
    %                   'sunm/yr' - Solar mass per year.
    %                   Default is 'sunm/yr'.
    % Output : - Bondi-Hoyle accreation rate (default units 'sunm/yr').
    %          - Lambda_s parameter.
    %          - Approximate accretion radius [cm].
    % Author : Eran Ofek (2026 Jan) 
    % Example: Mdot=astro.accretion.bondiHoyle(1,1,30e5,10e5);

    arguments
        Mass    = 1;
        Ninf    = 1;   % [particles /cm^3]
        V       = 30e5;
        Ainf    = 1e6;
        Gamma   = 5./3;
        Args.Mu                = 0.5;
        Args.MassUnits         = 'SunM';
        Args.LengthUnits       = 'cm';
        Args.OutUnits          = 'SunM/yr';
    end

    Mass = convert.mass(Args.MassUnits, 'gr', Mass);
    Ainf = convert.length(Args.LengthUnits, 'cm', Ainf);
    V    = convert.length(Args.LengthUnits, 'cm', V);

    Ninf   = Ninf.*Args.Mu;
    RhoInf = Ninf .* constant.mp;

    Lambda_s = 0.5.^((Gamma+1)./(2.*(Gamma-1))) .* ((5-3.*Gamma)./4).^(-(5-3.*Gamma)./(2.*(Gamma-1))); % [14.3.17]

    Mdot = 4.*pi.*Lambda_s .*(constant.G.*Mass).^2 .* (Ainf.^2 + V.^2).^(-3./2) .* RhoInf;  % [gr/s]   [14.3.31]

    switch lower(Args.OutUnits)
        case {'gr/s','g/s'}
            % do nothing
        case 'sunm/yr'
            Mdot = Mdot .*365.25.*86400 ./constant.SunM;
        otherwise
            error('Unknown OutUnits option');
    end

    if nargout>2
        AccRadius = (constant.G * Mass / (V.^2 + Ainf.^2));  % [cm] Calculate the approximate accretion radius [p.421]
    end

end
