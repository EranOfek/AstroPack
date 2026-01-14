function [P] = nodal_precession(Mp, Aout, Mbinary, Ain, Inc, Eccen, Args)
    % Approximate nodal precession of binary due to third massive object
    % Input  : - Mass of primary [g]. Default is sun mass.
    %          - Outer semi major axis [cm].
    %            If 'IsA' is false, then this is the outer period [s].
    %            Default is 1 au. 
    %          - Total mass of inner binary [g]. Default is Earth mass.
    %          - Inner semi major axis [cm].
    %            If 'IsA' is false, then this is the inner period [s].
    %            Default is 384400e5.
    %          - Inclination of the inner in respect to the outer [deg].
    %            Default is 5.
    %          - Outer eccentricity. Default is 0.01.
    %          * ...,key,val,... 
    %            'IsA' - A logical indicatinf if Ain/out are provided or
    %                   Pin/out. Default is true.
    % Output : - Approximate nodal precession period [s].
    % Author : Eran Ofek (2026 Jan) 
    % Example: [P] = astro.binary.nodal_precession()

    arguments
        Mp          = constant.SunM;
        Aout        = constant.au;
        Mbinary     = constant.EarthM;
        Ain         = 384400e5;
        Inc         = 5;
        Eccen       = 0.01;
        Args.IsA    = true;  % otherwise Ain/Aout are Pin/Put
        
    end

    G = constant.G;

    if Args.IsA
        N_in = sqrt(G.*Mbinary./(Ain.^3));
        Omega_in = -3./4 .* N_in .* Mp./Mbinary .* (Ain./Aout).^3 .* cosd(Inc)./((1-Eccen.^2).^1.5);
        P = 2.*pi./Omega_in;
    else
        % in this case Ain/out represent Pin/out
        P = 4./3.* (Mp+Mbinary)./Mp  .* Aout.^2./Ain .* (1 - Eccen.^2).^1.5 ./cosd(Inc);
    end

end
