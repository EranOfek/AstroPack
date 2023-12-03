function [E_H, E_dotH, S_B, S_dotB]=earthObserverPos(Time, Args)
    % Earth/topocentric ecliptic position relative to the Solar System barycenter
    % Input  : - Vector of JD.
    %          * ...,key,val,...
    %            'CooSys' - Coordinate system:
    %                   'h'|'helio' - Heliocentric.
    %                   'b'|'bary' - Barycentric (default).
    %            'RefFrame' - Reference frame.
    %                   'ec' - J2000.0 ecliptic (default).
    %                   'eq' - J2000.0 equatorial.
    %            'SunLightTime' - Sun light time correction [day].
    %                   The Sun position (for heliocentric CooSys) is
    %                   evaluate at Time-SunLightTime.
    %                   Default is 0.
    %            'GeoPos' - Geodetic position. If [], then assume geocentric position
    %                   and return zeros. Otherwise should be [Long, Lat, Height]
    %                   in [rad, rad, m]. Default is [].
    %            'EarthEphem' - One of the following:
    %                   'vsop87' - use celestial.SolarSys.calc_vsop87
    %                   'inpop' - Use INPOP (Default).
    %            'INPOP' - A populated INPOP object.
    %                   If empty, then will generate.
    %            'TimeScale' - The JD time scale. Default is 'TDB'.
    %            'OutUnits'  - 'km','cm','au',... for velocity this
    %                   is always, the same per day.
    %                   Default is 'au'.
    %                   Note that the value of he AU is taken from
    %                   the Constant.AU property.
    %            'ObserverEphem' - An optional observer [X, Y, Z, VX, VY, VZ].
    %                   The RefFrame and CooSys is like the one assumed by
    %                   the input arguments.
    %                   Default is [].
    % Output : - Barycentric position of geocentric or topocentric
    %            observer [au] (column per JD).
    %          - Barycentric velocity of observer [au/day].
    %          - Sun barycentric position (available only with INPOP).
    %          - Sun barycentric velocity (available only with INPOP).
    % Author : Eran Ofek (Nov 2023)
    % Example: [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(2451545+(0:1).');
    
    
    arguments
        Time
        Args.CooSys          = 'b';  % 'H'|'B'
        Args.RefFrame        = 'ec';   % 'ec'|'eq'
        Args.SunLightTime    = 0;
        Args.GeoPos          = [];
        Args.EarthEphem      = 'inpop';
        Args.INPOP           = [];
        Args.TimeScale       = 'TDB';
        Args.OutUnits        = 'au';  % or AU/day
        Args.RefEllipsoid    = 'WGS84';
        Args.ObserverEphem   = [];
    end
   
    switch lower(Args.RefFrame)
        case 'ec'
            Frame_VSOP = 'd';
            Frame_Ec   = true;
        case 'eq'
            Frame_VSOP = 'E';
            Frame_Ec   = false;
        otherwise
            error('Unknown RefFrame option');
    end
    
    if ~isempty(Args.ObserverEphem)
        Args.EarthEphem = 'user';
    end
    
    IN = [];
    switch lower(Args.EarthEphem)
        case 'vsop87'            
            switch lower(Args.CooSys)
                case {'h','helio'}
                    % Heliocentric
                    [E_H,E_dotH] = celestial.SolarSys.calc_vsop87(Time, 'Earth', 'a', Frame_VSOP);
                case {'b','bary'}
                    % Barycentric
                    [E_H,E_dotH] = celestial.SolarSys.calc_vsop87(Time, 'Earth', 'e', Frame_VSOP);
            end
        case 'inpop'
            if isempty(Args.INPOP)
                IN = celestial.INPOP;  % need to make it singelton
                IN.populateTables({'Ear','Sun'});
                IN.populateTables({'Ear','Sun'},'FileData','vel');
            else
                IN = Args.INPOP;
            end
            switch lower(Args.CooSys)
                case {'h','helio'}
                    % Heliocentric
                    S_B    = IN.getPos('Sun',Time-Args.SunLightTime, 'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                    E_H    = IN.getPos('Ear',Time,                   'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits) - S_B; ...
                             
                    %E_H(1) = E_H(1) - 100./149.59787e6;
                    S_dotB = IN.getVel('Sun',Time-Args.SunLightTime, 'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                    E_dotH = IN.getVel('Ear',Time,                   'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits) - S_dotB;
                case {'b','bary'}
                    % Barycentric
                    E_H    = IN.getPos('Ear',Time, 'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                    E_dotH = IN.getVel('Ear',Time, 'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                    if nargout>2
                        S_B    = IN.getPos('Sun',Time-Args.SunLightTime, 'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                        if nargout>3
                            S_dotB = IN.getVel('Sun',Time-Args.SunLightTime, 'IsEclipticOut',Frame_Ec, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                        end
                    end
                        
            end
            % convert to eclipic coordinates

        case 'user'
            E_H    = Args.ObserverEphem(:,1:3).';
            E_dotH = Args.ObserverEphem(:,4:6).';
        otherwise
            error('Unknown EarthEphem option');
    end
    
    if ~isempty(Args.GeoPos)
        [Gau, Gdot] = celestial.coo.topocentricVector(Time, Args.GeoPos, 'OutUnits',Args.OutUnits,...
                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                             'Convert2ecliptic',Frame_Ec,...
                                                             'Equinox','J2000');

        E_H    = E_H + Gau;
        E_dotH = E_dotH + Gdot;
    end
    
    
end
