function S = ple_all(JD, OutType)
    %
    
    arguments
        JD
        OutType
    end
   
   
    Npl  = 8;
    Njd  = numel(JD);
    Ncoo = 3;
    
    XYZ = zeros(Ncoo, Njd, Npl);
    
    [L, B, R] = celestial.SolarSys.ple_mercury(JD);
    [X, Y, Z] = celestial.SolarSys.ec_longlat2cart(L, B, R, JD, OutType);
    
    XYZ(1,:,1) = X;
    XYZ(2,:,1) = Y;
    XYZ(3,:,1) = Z;
    
    celestial.SolarSys.ple_venus
    celestial.SolarSys.ple_earth
    celestial.SolarSys.ple_mars
    celestial.SolarSys.ple_jupiter
    celestial.SolarSys.ple_saturn
    celestial.SolarSys.ple_uranus
    celestial.SolarSys.ple_neptune
    
    
    

    
    
end
