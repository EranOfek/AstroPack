function [Az, Alt, AM, PA, HA]=radec2azalt(JD, RA, Dec, Args)
    % Convert JD,RA,Dec to Az,Alt,AM,ParAng
    % Input  : - Array of JD [days]
    %          - Array of RA
    %          - Array of Dec
    %          * ...,key,val,...
    %            'GeoCoo' - Geodetic coordinates [Lon(deg), Lat(deg)]
    %            'InUnits'  - Input RA/Dec units. Default is 'deg'.
    %            'OutUnits' - Output Az/Alt units. Default is 'deg'.
    %            'LSTType' - 'a' - apparent, or 'm' - mean. Default is 'a'.
    %            'InEquinoxJD' - The JD of the Equinox of the input RA, Dec.
    %                   Default is 2451545 (J2000).
    %            'OutEquinoxJD' - The JD of the Equinox of the output RA,
    %                   Dec, that will be used to calculate the Az and Alt.
    %                   This is tyically, the Equinox of date.
    %                   If [], then ignore and do not precess coordinates.
    %                   Default is [].
    % Output : - Azimuth.
    %          - Altitude.
    %          - Hardie airmass.
    %          - Parallactic angle.
    %          - HA.
    % Author : Eran Ofek (May 2023)
    % Example: [Az, Alt, AM, ParAng]=celestial.coo.radec2azalt(2451545+90,0,0,'GeoCoo',[35 30]);

    arguments
        JD
        RA
        Dec
        Args.GeoCoo       = [];     % [deg deg km]
        Args.InUnits      = 'deg';
        Args.OutUnits     = 'deg';
        Args.LSTType      = 'a';

        Args.OutEquinoxJD = [];
        Args.InEquinoxJD(1,1)  = 2451545;
    end
    RAD = 180./pi;

    if isempty(Args.GeoCoo)
        error('Geodetic position must be provided');
    end

    InFactor  = convert.angular(Args.InUnits, 'rad');
    OutFactor = convert.angular('rad', Args.OutUnits);

    RA  = InFactor.*RA;  % [rad]
    Dec = InFactor.*Dec; % [rad]

    % precess to equinox of date (if needed)
    if ~isempty(Args.OutEquinoxJD)
        % Precess RA and Dec to the equinox of date
        [RA,Dec] = CelCoo.precessCoo(RA, Dec, Args.InEquinoxJD,Args.OutEquinoxJD,...
                                              'CooUnits','rad','InIsTrue',false,'OutIsTrue',false);
    end


    LST = 2.*pi.*celestial.time.lst(JD, Args.GeoCoo(1)./RAD, Args.LSTType);  % [rad]
    HA = LST - RA;   % [rad]
    Lat = Args.GeoCoo(2)./RAD;
    [Az,Alt] = celestial.coo.hadec2azalt(HA, Dec, Lat);
    

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
    HA  = HA.*OutFactor;

end
