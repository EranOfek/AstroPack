function [E_H, E_dotH, IN]=earthObserverPos(Time, Args)
    % Earth/topocentric ecliptic position relative to the Solar System barycenter
    % Input  : - Vector of JD.
    %          * ...,key,val,...
    %            'CooSys' - Coordinate system:
    %                   'h' - Heliocentric.
    %                   'b' - Barycentric (default).
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
    % Output : - Barycentric position of geocentric or topocentric
    %            observer [au] (column per JD).
    %          - Barycentric velocity of observer [au/day].
    %          - A populated celestial.INPOP object (if used).
    % Author : Eran Ofek (Nov 2023)
    % Example: [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(2451545+(0:1).');
    
    
    arguments
        Time
        Args.CooSys          = 'b';  % 'H'|'B'
        Args.GeoPos          = [];
        Args.EarthEphem      = 'inpop'
        Args.INPOP           = [];
        Args.TimeScale       = 'TDB';
        Args.OutUnits        = 'au';  % or AU/day
        Args.RefEllipsoid    = 'WGS84';
        
    end
   
    IN = [];
    switch lower(Args.EarthEphem)
        case 'vsop87'
            switch lower(Args.CooSys)
                case 'h'
                    % Heliocentric
                    [E_H,E_dotH] = celestial.SolarSys.calc_vsop87(Time, 'Earth', 'a', 'd');
                case 'b'
                    % Barycentric
                    [E_H,E_dotH] = celestial.SolarSys.calc_vsop87(Time, 'Earth', 'e', 'd');
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
                case 'h'
                    % Heliocentric
                    E_H    = IN.getPos('Ear',Time, 'IsEclipticOut',true, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits) - ...
                             IN.getPos('Sun',Time, 'IsEclipticOut',true, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                    E_dotH = IN.getVel('Ear',Time, 'IsEclipticOut',true, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits) - ...
                             IN.getVel('Sun',Time, 'IsEclipticOut',true, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                case 'b'
                    % Barycentric
                    E_H    = IN.getPos('Ear',Time, 'IsEclipticOut',true, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
                    E_dotH = IN.getVel('Ear',Time, 'IsEclipticOut',true, 'TimeScale',Args.TimeScale, 'OutUnits',Args.OutUnits);
            end
            % convert to eclipic coordinates

        otherwise
            error('Unknown EarthEphem option');
    end
    
    if ~isempty(Args.GeoPos)
        [Gau, Gdot] = celestial.coo.topocentricVector(Time(It), Args.GeoPos, 'OutUnits','au',...
                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                             'Convert2ecliptic',true,...
                                                             'Equinox','J2000');

        E_H    = E_H + Gau;
        E_dotH = E_dotH + Gdot;
    end
    
    
end
