function [BestRA, BestDec, BestAlt] = nearZenithFarFromMoon(JD, Args)
    % Choose a point near zenith and far from the Moon (for focus)
    %   Choose from either zenith or Alt=70 deg in Az jumps of 45 deg.
    % Input  : - Scalar JD (UTC). Default is now.
    %          * ...,key,val,... 
    %            'GeoPos' - Geodetic position [deg deg km].
    %                   Default is [35.04073 30.05298 0.415]
    %            'MaxIllum' - Max illumination fraction of Moon above to
    %                   choose max Moon dist. Default is 0.4.
    %            'OutUnits' - Output units. Default 'deg'.
    % Output : - Best RA
    %          - Best Dec
    %          - Best Alt
    % Author : Eran Ofek (2025 Sep) 
    % Example: [RA, Dec, Alt] = telescope.obs.nearZenithFarFromMoon(2451545);
    %          [RA, Dec, Alt] = telescope.obs.nearZenithFarFromMoon(2451545, 'GeoPos',[35.04 30.05 0.415]);

    arguments
        JD(1,1)         = celestial.time.julday();
        Args.GeoPos     = [35.04073 30.05298 0.415];  % [deg deg km]
        Args.MaxIllum   = 0.4;
        Args.OutUnits   = 'deg';
    end

    RAD = 180./pi;

    [Illum,Ph]=celestial.SolarSys.moon_illum(JD);

    

    Long = Args.GeoPos(1)./RAD;
    Lat  = Args.GeoPos(2)./RAD;

    LST = celestial.time.lst(JD, Long).*2.*pi;  % [rad]

    DecZenith = Lat;

    [MoonRA, MoonDec] = celestial.SolarSys.mooncool(JD, [Long Lat], 'b');  % [rad]
    MoonHA = LST - MoonRA;
    [~,MoonAlt] = celestial.coo.hadec2azalt(MoonHA,MoonDec,Lat);
 
    ZenithHA = 0;
    %ZenithRA = celestial.convert.convert_ha(HA, JD, 'Long',Args.GeoPos(1), 'OutRange','2pi', 'OutUnits','rad');  % [rad]
    
    AzRange = [0; (0:45:315).'./RAD;];
    AltRange = [90./RAD; (70./RAD).*ones(numel(AzRange)-1,1)];
    [AltZenithHA,AltZenithDec] = celestial.coo.azalt2hadec(AzRange, AltRange, Lat, 'rad');
    AltZenithRA = LST - AltZenithHA;

    Dist = celestial.coo.sphere_dist_fast(MoonRA, MoonDec, AltZenithRA, AltZenithDec);
    if Illum<Args.MaxIllum || MoonAlt<0
        I = 1;
    else
        [~,I]=max(Dist);
    end
    ConvFactor = convert.angular('rad',Args.OutUnits);
    BestRA  = ConvFactor.*AltZenithRA(I);
    BestDec = ConvFactor.*AltZenithDec(I);
    BestAlt = ConvFactor.*AltRange(I);

end
