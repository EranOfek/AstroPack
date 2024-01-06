function [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = refractedCoo(InRA, InDec, Args)
    % Apply atmospheric refraction to RA and Dec coordinates
    % Input  : - R.A., [deg|rad|sex] or object name.
    %            If second input is provided and RA is not numeric, then
    %            will assume input is in sexagesinal coordinates.
    %            All coordinates refers to equinox of date.
    %          - Dec. [deg|rad|sex]. If empty, then will interpret the
    %            first input argument as an object name.
    %            Default is [].
    %          * ...,key,val,... 
    %            'JD' - JD of observing time at which to do the conversion.
    %                   Default is celestial.time.julday().
    %            'GeoPos' - Geodetic position of observer.
    %                   Default is [35 30 415] in [deg deg m].
    %            'PosUnits' - Angular units of GeoPos.
    %                   Default is 'deg'.
    %            'TypeLST' - 'm'|'a'. Default is 'm'.
    %
    %            'InUnits' - Default is 'deg'.
    %            'OutUnits' - Default is 'deg'.
    %            'Server' - If input is object name, then this is the name
    %                   server that will be used: @VO.name.server_simbad|
    %                   @VO.name.server_ned.
    %                   Default is @VO.name.server_simbad
    %
    %            'Wave' - Wavelength [Ang]. Default is 5000 Ang.
    %            'Temp' - Temperature [C]. Default is 15 C .
    %            'Pressure' - Pressure [hPa]. Default is 760 mm Hg.
    %            'Pw' - Partial vapour pressure. Default is 8 mm Hg.
    %
    % Output : - RA after refraction.
    %            Refer to equinox of date.
    %          - Dec after refraction.
    %          - Unrefracted Altitude. If <0 deg result should be ignored.
    %          - Refraction angle.
    %          - Parallactic angle.
    %          - Shift in RA.
    %          - Shift in Dec.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [RA, Dec, Alt]=celestial.convert.refractedCoo(0,80)

    arguments
        InRA
        InDec                  = [];

        Args.JD                = celestial.time.julday();
        Args.GeoPos            = [35 30 415];     
        Args.PosUnits          = 'deg';
        Args.TypeLST           = 'm';
        
        Args.InUnits           = 'deg';  % 'deg'|'rad'|'sex'|'ned'|'simbad'|
        Args.OutUnits          = 'deg';  % 'deg'|'rad'
        Args.Server            = @VO.name.server_simbad;
        
        Args.Wave              = 5000;  % [A]
        Args.Temp              = 15;
        Args.Pressure          = 760;
        Args.Pw                = 8;
                
    end

    % input coordinates / object name:
    [RA, Dec]=celestial.convert.cooResolve(InRA, InDec, 'InUnits',Args.InUnits, 'OutUnits','rad', 'Server',Args.Server); % [rad]
    
    % apply refraction
    
    Args.GeoPos = convert.angular(Args.PosUnits, 'rad', Args.GeoPos(1:2));  % [rad]
    
    LST     = celestial.time.lst(Args.JD, Args.GeoPos(1), Args.TypeLST);  % mean local sidereal time [frac of day]
    HA      = 2.*pi.*LST - RA;
    ParAng  = celestial.coo.parallactic_angle(RA, Dec, LST, Args.GeoPos(2));
    
    [Alt] = celestial.coo.ha2alt(HA, Dec, Args.GeoPos(2), 'rad');
 
    Refraction = celestial.coo.refraction_wave(Alt,Args.Wave, Args.Temp, Args.Pressure, Args.Pw);  % [rad]

    DelAlpha = Refraction.*sec(Dec).*sin(ParAng);
    DelDelta = Refraction.*cos(ParAng);
    
    OutRA    = RA + DelAlpha;
    OutDec   = Dec + DelDelta;
    
    OutRA    = mod(OutRA, 2.*pi);
    
    AngFactor  = convert.angular('rad', Args.OutUnits);
    OutRA      = AngFactor .* OutRA;
    OutDec     = AngFactor .* OutDec;
    Alt        = AngFactor .* Alt;
    Refraction = AngFactor .* Refraction;
    ParAng     = AngFactor .* ParAng;
    DelAlpha   = AngFactor .* DelAlpha;
    DelDelta   = AngFactor .* DelDelta;
    
    
end
