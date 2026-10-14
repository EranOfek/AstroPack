function [BJD, BVel] = barycentricJD(JD, RA, Dec, Args)
    % Convert JD (TDB) to Barycentric JD (TDB)
    % Input  : - Vector or scalar of JD (in some time scale).
    %          - Vector or scalar of J2000.0 RA (default units radinas).
    %          - Vector or scalar of J2000.0 Dec (default units radinas).
    %            JD, RA and Dec are either scalars or vectors of the same
    %            length N; scalars are applied to all N elements.
    %          * ...,key,val,...
    %            'InTimeScale' - Time scale of input JD: 'TT'|'TDB'|'UTC'.
    %                   Default is 'TDB'.
    %            'GeoPos' - Geodetic position. If [], then assume geocentric position
    %                   and return zeros. Otherwise should be [Long, Lat, Height]
    %                   in [rad, rad, m]. Default is [].
    %            'RefEllipsoid' - Reference ellipsoid. Default is 'WGS84'.
    %            'Object' - Object in the Solar system on which the JD
    %                   was measured. Default is 'Ear'.
    %            'CooUnits' - RA,Dec coordinates units. Default is 'rad'.
    %            'VelOutUnits' - 'cm/s' | 'm/s' | 'km/s' | 'au/day' | ...
    %                   Default is 'cm/s'.
    %            'INPOP' - A populated celestial.INPOP object.
    %                   If empty, then create and populate.
    %                   Default is [].
    % Output : - Barycentric JD (TDB).
    %          - Barycentric velocity in Equatorial J2000.0 frame
    %            Default units [cm/s]. Control the output units using the
    %            VelOutUnits argument.
    % Author : Eran Ofek (May 2022)
    % Example: [BJD, BVel] = celestial.time.barycentricJD(2451545,1,1)
    %          [BJD, BVel] = celestial.time.barycentricJD(2451545,1,1,'VelOutUnits','au/day')
    %          [BJD, BVel] = celestial.time.barycentricJD(2460000,1,1,'InTimeScale','UTC')
    
    arguments
        JD
        RA
        Dec
        Args.InTimeScale    = 'TDB'; % 'UTC'|'TDB'|'TT'
        Args.GeoPos         = [];
        Args.RefEllipsoid   = 'WGS84';
        Args.Object         = 'Ear';
        Args.CooUnits       = 'rad';
        Args.VelOutUnits    = 'cm/s';
        Args.INPOP          = [];
    end
    
    JD = JD(:);
    N = max([numel(JD), numel(RA), numel(Dec)]);
    if any(~ismember([numel(JD), numel(RA), numel(Dec)], [1 N]))
        error('JD, RA and Dec must be scalars or vectors of the same length');
    end
    if numel(RA)==1
        RA = RA.*ones(N,1);
    end
    if numel(Dec)==1
        Dec = Dec.*ones(N,1);
    end


    % sanity check:
    if sum(isnan(JD)) > 0 || sum(isnan(RA)) > 0 || sum(isnan(Dec)) > 0
        error('Some of the input times or coordinates is NaN');
    end
    
    SECOND_DAY = 86400;
        
    if isempty(Args.INPOP)
        IP = celestial.INPOP;
        IP.populateTables(Args.Object, 'FileData', 'pos');
        IP.populateTables(Args.Object, 'FileData', 'vel');
        IP.populateTables('TT');
    else
        IP = Args.INPOP;
    end

    
    switch Args.InTimeScale
        case 'TDB'
            % do nothing - already in TDB time scale.
    
        case 'TT'
            TTmTDB   = IP.getTT(JD);  % TT-TDB [s]
            TTmTDB   = TTmTDB(:);
            % UTC + TT - UTC - TT + TDB
            JD       = JD - (TTmTDB)./SECOND_DAY;             

        case 'UTC'
            [TTmUTC] = celestial.time.tt_utc(JD);  % [s]
            if isnan(TTmUTC)
                error('TT - UTC is not available, need to update via I = Installer; I.Install("Time")');
            end
            TTmTDB   = IP.getTT(JD);  % TT-TDB [s]
            TTmTDB   = TTmTDB(:);
            % UTC + TT - UTC - TT + TDB
            JD       = JD + (TTmUTC-TTmTDB)./SECOND_DAY;             
            
        otherwise
            error('Unknown InTimeScale option');
    end
    
    ConvFactor = convert.angular(Args.CooUnits, 'rad');
    RA         = RA.*ConvFactor;
    Dec        = Dec.*ConvFactor;
    
    % object coordinates to cosine direction
    % Equatorial J2000
    [CD1,CD2,CD3] = celestial.coo.coo2cosined(RA(:).', Dec(:).');
    ObjPos = [CD1; CD2; CD3];
    
    C          = constant.c;
    SEC_IN_DAY = 86400;
    
        
    AU         = IP.Constant.AU .* 1e5;   % cm
    
    Pos = IP.getPos(Args.Object, JD, 'OutUnits','au', 'IsEclipticOut',false);  % [au]   Equoatorial
    Vel = IP.getVel(Args.Object, JD, 'OutUnits','au', 'IsEclipticOut',false);  % [au/day]   Equoatorial
    
    [G, Gdot] = celestial.coo.topocentricVector(JD, Args.GeoPos, 'OutUnits','au',...
                                                             'TimeOutUnits','day',...
                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                             'Convert2ecliptic',false,...
                                                             'Equinox','J2000');

    
    TopoPos = Pos + G;
    TopoVel = Vel + Gdot;
    
    % projections on the (unit) object directions, column by column; a single
    % epoch (3x1) is expanded over all directions
    DelJD  = sum(ObjPos.*TopoPos, 1).*AU./(C.*SEC_IN_DAY);  % [day]
    ObjVel = sum(ObjPos.*TopoVel, 1).*AU./SEC_IN_DAY;       % [cm/s]
   
    BJD  = JD(:) + DelJD(:);
    BVel = ObjVel(:);
    
    if ~strcmp(Args.VelOutUnits,'cm/s')
        BVel = convert.velocity('cm/s',Args.VelOutUnits, BVel);
    end
    
end
