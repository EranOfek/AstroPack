function [BJD, BVel] = barycentricJD(JD, RA, Dec, Args)
    % Convert JD (TDB) to Barycentric JD (TDB)
    % Input  : - JD in TDB time scale.
    %          - J2000.0 RA (default units radinas).
    %          - J2000.0 Dec (default units radinas).
    %          * ...,key,val,...
    %            'GeoPos' - Geodetic position. If [], then assume geocentric position
    %                   and return zeros. Otherwise should be [Long, Lat, Height]
    %                   in [rad, rad, m]. Default is [].
    %            'RefEllipsoid' - Reference ellipsoid. Default is 'WGS84'.
    %            'Object' - Object in the Solar system on which the JD
    %                   was measured. Default is 'Ear'.
    %            'CooUnits' - RA,Dec coordinates units. Default is 'rad'.
    %            'VelOutUnits' - 'cm/s' | 'm/s' | 'km/s' | 'au/day' | ...
    %                   Default is 'cm/s'.
    % Output : - Barycentric JD (TDB).
    %          - Barycentric velocity in Equatorial J2000.0 frame
    %            Default units [cm/s]. Control the output units using the
    %            VelOutUnits argument.
    % Author : Eran Ofek (May 2022)
    % Example: [BJD, BVel] = celestial.time.barycentricJD(2451545,1,1)
    %          [BJD, BVel] = celestial.time.barycentricJD(2451545,1,1,'VelOutUnits','au/day')
    
    arguments
        JD
        RA
        Dec
        Args.GeoPos         = [];
        Args.RefEllipsoid   = 'WGS84';
        Args.Object         = 'Ear';
        Args.CooUnits       = 'rad';
        Args.VelOutUnits    = 'cm/s';
    end
    
    warning('not tested')
    
    ConvFactor = convert.angular(Args.CooUnits, 'rad');
    RA         = RA.*ConvFactor;
    Dec        = Dec.*ConvFactor;
    
    % object coordinates to cosine direction
    % Equatorial J2000
    [CD1,CD2,CD3] = celestial.coo.coo2cosined(RA(:).', Dec(:).');
    ObjPos = [CD1; CD2; CD3];
    
    C          = constant.c;
    SEC_IN_DAY = 86400;
    
    
    IP = celestial.INPOP;
    IP.populateTables(Args.Object, 'FileData', 'pos');
    IP.populateTables(Args.Object, 'FileData', 'vel');
    
    AU         = IP.Constant.AU .* 1e5;   % cm
    
    Pos = IP.getPos(Args.Object, JD, 'OutUnits','au', 'IsEclipticOut',false);  % [au]
    Vel = IP.getVel(Args.Object, JD, 'OutUnits','au', 'IsEclipticOut',false);  % [au/day]
    
    [G, Gdot] = celestial.coo.topocentricVector(JD, Args.GeoPos, 'OutUnits','au',...
                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                             'Convert2ecliptic',false,...
                                                             'Equinox','J2000');

    
    TopoPos = Pos + G;
    TopoVel = Vel + Gdot;
    
    DelJD  = norm(TopoPos).*dot(ObjPos./norm(ObjPos),TopoPos./norm(TopoPos)).*AU./(C.*SEC_IN_DAY);  % [day]
    ObjVel = norm(TopoVel).*dot(ObjPos./norm(ObjPos),TopoVel./norm(TopoVel)).*AU./SEC_IN_DAY;       % [cm/s]
   
    BJD  = JD + DelJD;
    BVel = ObjVel;
    
    if ~strcmp(BVel,'cm/s')
        BVel = convert.velocity('cm/s',Args.VelOutUnits, BVel);
    end
    
end
