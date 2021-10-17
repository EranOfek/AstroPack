function XYZ = ple_xyzAll(JD, OutType)
    % Get approximate cartesian coordinates of major planets in a matrix.
    %   Return a matrix of 3 X Nplanets X Ntimes of cartesian coordinates.
    % Input  : - A vector of JD.
    %          - Output type (always cartesian): 
    %               'Ecdate' - Ecliptic of date.
    %               'Eqdate' - Equatorial mean equinox of date.
    %               'EqJ2000' - Equatorial mean equinox of J2000.
    %            Default is 'EqJ2000'.
    % Output : - A matrix of 3 X Nplanets X Ntimes of cartesian coordinates.
    % Author : Eran Ofek (Oct 2021)
    % Example: S = celestial.SolarSys.ple_xyzAll(2451545+(1:1:10));
    
    arguments
        JD
        OutType   = 'EqJ2000';  % 'EcDate' | 'EqDate' | 'EqJ2000'
    end
   
    Funs = {@celestial.SolarSys.ple_mercury,...
            @celestial.SolarSys.ple_venus,...
            @celestial.SolarSys.ple_earth,...
            @celestial.SolarSys.ple_mars,...
            @celestial.SolarSys.ple_jupiter,...
            @celestial.SolarSys.ple_saturn,...
            @celestial.SolarSys.ple_uranus,...
            @celestial.SolarSys.ple_neptune};
    
   
    Npl  = numel(Funs);
    Njd  = numel(JD);
    Ncoo = 3;
    
    XYZ = zeros(Ncoo, Npl, Njd);
    JD  = JD(:);
    
    for Ipl=1:1:Npl
        [L, B, R] = Funs{Ipl}(JD);
        [X, Y, Z] = celestial.SolarSys.ec_longlat2cart(L, B, R, JD, OutType);
        XYZ(1,Ipl,:) = X;
        XYZ(2,Ipl,:) = Y;
        XYZ(3,Ipl,:) = Z;
    end
    
    

    
    
end
