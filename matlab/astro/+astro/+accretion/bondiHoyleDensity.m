function [Rho,N, FlowVel] = bondiHoyleDensity(R, Mass, Ninf, Ainf, Gamma, Args)
    % The density as a function of radius for spherical accreation
    %   For R<<R_s and 1<=Gamma<5/3
    %   For te R_s (transonic radius) see: astro.accreation.transonicRadius
    % Input  : - Array of radii at which to calculate the density.
    %          - Mass (default units 'SunM'). Default is .
    %          - Medium particle density [cm^-3]
    %          - Speed of sound at infinity (default units 'cm/s').
    %            Default is 1e6.
    %          - Adiabatic index. Default is 5./3.
    %          * ...,key,val,... 
    %            'Mu' - Mean molecular weight. Default is 0.5.
    %            'MassUnits' - Mass units. Default is 'SunM'.
    %            'LengthUnits' - Length units of velocity, and distance.
    %                   Default is 'cm'.
    % Output : - Density a s a function of radius [gr/cm^3]
    %          - Particle numbre density as a function of radius [cm^-3]
    %          - Flow velocity
    % Author : Eran Ofek (2026 Jan) 
    % Example: Rho=astro.accretion.bondiHoyleDensity([1e10, 1e13, 1e16]);

    arguments
        R
        Mass    = 1;
        Ninf    = 1;   % [particles /cm^3]
        Ainf    = 1e6;
        Gamma   = 4./3;
        Args.Mu                = 0.5;
        Args.MassUnits         = 'SunM';
        Args.LengthUnits       = 'cm';
    end

    Mass = convert.mass(Args.MassUnits, 'gr', Mass);
    Ainf = convert.length(Args.LengthUnits, 'cm', Ainf);

    Ninf   = Ninf.*Args.Mu;
    RhoInf = Ninf .* constant.mp;

    Lambda_s = 0.5.^((Gamma+1)./(2.*(Gamma-1))) .* ((5-3.*Gamma)./4).^(-(5-3.*Gamma)./(2.*(Gamma-1)));  % [14.3.17]

    Rho = RhoInf .* Lambda_s./sqrt(2) .* (constant.G.*Mass./Ainf.^2).^(3./2) .* R.^(-3./2);   % [14.3.24]
    N   = Rho./(constant.mp.*Args.Mu);

    if nargout>2
        % transoinc radius
        Rs = astro.accretion.transonicRadius(Mass, Ainf, Gamma, 'MassUnits','gr', 'LengthUnits','cm', 'OutUnits','cm');

        if Rs==0
            error('To calculate the transonic point Gamma should be <5/3');
        end

        % R>>Rs
        FlowVel = Lambda_s .* (constant.G.*Mass./(Ainf.^2)).^2 .* R.^(-2);  % [14.3.22]

        % R<<Rs (free fall):
        FlowVelS = sqrt(2.*constant.G.*Mass./R);   % [1<=Gamma<5/3]

        FlowVel(R<Rs) = FlowVelS;
    end
end
