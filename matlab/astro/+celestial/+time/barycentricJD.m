function [BJD, BVel] = barycentricJD(JD, Args)
    %
   
    arguments
        JD
        Args.GeoPos         = [];
        Args.RefEllipsoid   = 'WGS84';
        Args.Object         = 'Ear';
    end
    
    IP = celestial.INPOP;
    IP.populateTables(Args.Object, 'FileData', 'pos');
    IP.populateTables(Args.Object, 'FileData', 'vel');
    
    Pos = IP.getPos(Args.Object, JD, 'OutUnits','au', 'IsEclipticOut',false);
    Vel = IP.getVel(Args.Object, JD, 'OutUnits','au', 'IsEclipticOut',false);
    
    [Gau, Gdot] = celestial.coo.topocentricVector(JD, Args.GeoPos, 'OutUnits','au',...
                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                             'Convert2ecliptic',false,...
                                                             'Equinox','J2000');



    
end
