function [Az, Alt, AM, PA]=radec2azalt(JD, RA, Dec, Args)
    % Convert JD,RA,Dec to Az,Alt,AM,ParAng
    % Input  : - Array of JD [days]
    %          - Array of RA
    %          - Array of Dec
    %          * ...,key,val,...
    %            'GeoCoo' - Geodetic coordinates [Lon(deg), Lat(deg)]
    %            'InUnits'  - Input RA/Dec units. Default is 'deg'.
    %            'OutUnits' - Output Az/Alt units. Default is 'deg'.
    % Output : - Azimuth.
    %          - Altitude.
    %          - Hardie airmass.
    %          - Parallactic angle.
    % Author : Eran Ofek (May 2023)
    % Example: [Az, Alt, AM, ParAng]=celestial.coo.radec2azalt(2451545+90,0,0,'GeoCoo',[35 30]);

    arguments
        JD
        RA
        Dec
        Args.GeoCoo       = [];     % [deg deg km]
        Args.InUnits      = 'deg';
        Args.OutUnits     = 'deg';
        
    end
    RAD = 180./pi;

    if isempty(Args.GeoCoo)
        error('Geodetic position must be provided');
    end

    InFactor  = convert.angular(Args.InUnits, 'rad');
    OutFactor = convert.angular('rad', Args.OutUnits);

    LST = 2.*pi.*celestial.time.lst(JD, Args.GeoCoo(1)./RAD, 'a');  % [rad]
    HA = LST - InFactor.*RA;   % [rad]
    Lat = Args.GeoCoo(2)./RAD;
    [Az,Alt] = celestial.coo.hadec2azalt(HA, Dec.*InFactor, Lat);
    

    if nargout>2
        % airmass
        AM = celestial.coo.hardie(pi./2 - Alt);

        if nargout>3
            % parallactic angle
            TanQ = sin(HA)./(tan(Lat).*cos(Dec) - sin(Dec).*cos(HA));
            PA   = atan(TanQ);

            PA   = PA.*OutFactor;
        end
    end

    Az  = Az.*OutFactor;
    Alt = Alt.*OutFactor;

end
