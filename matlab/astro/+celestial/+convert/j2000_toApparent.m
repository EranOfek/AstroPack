function [OutRA, OutDec, Alt, Refraction, Aux] = j2000_toApparent(RA, Dec, JD, Args)
    % Convert J2000 coordinates to the apparent position of a star
    %   Including:
    %   proper motion
    %   parallax
    %   abberation of light
    %   (no light deflection)
    %   precession and nutation
    %   atmospheric refraction
    % Input  : - J2000.0 R.A., [deg|rad|sex] or object name.
    %            If second input is provided and RA is not numeric, then
    %            will assume input is in sexagesinal coordinates.
    %          - J2000.0 Dec. [deg|rad|sex]. If empty, then will interpret the
    %            first input argument as an object name.
    %            Default is [].
    %          - JD of position at time scale (default 'TDB').
    %            Default is celestial.time.julday().
    %          * ...,key,val,... 
    %            'InUnits' - Units of input coo. 'deg'|'rad'.
    %                   Default is 'deg'.
    %            'Epoch' - Coordinates epoch (of proper motion).
    %                   Default is 2000.
    %            'EpochUnits' - Epoch units. 'J'|'B'|'JD'|'MJD'.
    %                   Default is 'J'.
    %            'OutUnits' - Output units.
    %                   Default is 'deg'.
    %            'OutEquinox' - Output coordinates equinox time.
    %                   If empty, then use the same as JD input.
    %                   Default is [].
    %            'OutEquinoxUnits' - Default is 'JD'.
    %            'OutMean' - A logical indicating if output coordinates are
    %                   refered to mean equinox of date (true), or true
    %                   equinox of date (false).
    %                   Default is false.
    %            'PM_RA' - PM in RA [mas/yr]. Default is 0.
    %            'PM_Dec' - PM in Dec [mas/yr]. Default is 0.
    %            'Plx' - Parallax [mas]. Default is 0.01.
    %            'RV' - Radial vel. [km/s]. Default is 0.
    %            'INPOP' - celestial.INPOP object.
    %                   Default is celestial.INPOP.init({'Ear'})
    %            'GeoPos' - Observer geodetic position [rad rad m].
    %                   If ApplyRefraction is true then this must provided.
    %                   Default is [35/RAD 30/RAD 415].
    %            'TypeLST' - 'm'|'a'. Default is 'm'.
    %
    %            'Server' - If input is object name, then this is the name
    %                   server that will be used: @VO.name.server_simbad|
    %                   @VO.name.server_ned.
    %                   Default is @VO.name.server_simbad
    %
    %            'TimeScale' - Default is 'TDB'.
    %            'ApplyAberration' - Apply aberration of light.
    %                   Default is true.
    %            'ApplyRefraction' - Apply atmospheric refraction.
    %                   Default is true.
    %
    %            'Wave' - Wavelength [Ang]. Default is 5000 Ang.
    %            'Temp' - Temperature [C]. Default is 15 C .
    %            'Pressure' - Pressure [hPa]. Default is 760 mm Hg.
    %            'Pw' - Partial vapour pressure. Default is 8 mm Hg.
    %
    %            'ShiftRA' - Shift in deg to add to RA/HA. Default is 0.
    %            'ShiftDec' - Shift in deg to add to Dec. Default is 0.
    %            'ApplyDistortion' - Apply additional distortion (e.g.,
    %                   mount distortion).
    %                   Default is false.
    %            'InterpHA' - A distortion to add to HA/RA or an
    %                   interpolating function Fun(HA, Dec) that returns the
    %                   distortions to add. All in deg-time.
    %                   Default is 0.
    %            'InterpDec' - Line 'InterpHA', but for declination.
    %                   Default is 0. Units deg.
    % Output : - Apparent RA
    %          - Apparent Dec
    %          - If ApplyRefraction=true, then this is the unrefracted
    %            Altitude (otherwise NaN). If <0, then object is below the
    %            horizon, and refraction is not relevant.
    %          - Atmospheric refraction angle.
    %          - A structure containing the input J2000 and apparent
    %            coordinates [all in deg].
    %            Note that Az/Alt are calculated using the App coordinates.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [OutRA, OutDec] = celestial.convert.j2000_toApparent(180, 0, celestial.time.julday([1 1 2024]))

    arguments
        RA
        Dec
        JD                     = celestial.time.julday();
        Args.InUnits           = 'deg';
        Args.Epoch             = 2000;
        Args.EpochUnits        = 'J';
        Args.OutUnits          = 'deg';
        
        Args.OutEquinox        = [];
        Args.OutEquinoxUnits   = 'JD';
        Args.OutMean           = false;
        
        Args.PM_RA             = 0;
        Args.PM_Dec            = 0;
        Args.Plx               = 1e-2;
        Args.RV                = 0;
        
        Args.INPOP             = celestial.INPOP.init({'Ear'});
        
        Args.GeoPos            = [[35 30].*pi./180, 415];   % [rad rad m]
        Args.TypeLST           = 'a';
        
        Args.Server            = @VO.name.server_simbad;
        
        Args.TimeScale         = 'TDB';
        
        Args.ApplyAberration logical = true;
        Args.ApplyRefraction logical = true;
        Args.Wave              = 5000;  % [A]
        Args.Temp              = 15;
        Args.Pressure          = 760;
        Args.Pw                = 8;                           

        Args.ShiftRA                  = 0;
        Args.ShiftDec                 = 0;

        Args.ApplyDistortion logical  = false;
        Args.InterpHA                 = 0;  % numeric [deg] or interpolation function [deg]
        Args.InterpDec                = 0;
        
    end
    RAD       = 180./pi;
    InputEqJD = 2451545.5;
    JYear     = 365.25;
    
    

    if isempty(Args.OutEquinox)
        Args.OutEquinox = JD;
    end

    EpochJD = convert.time(Args.Epoch, Args.EpochUnits, 'JD');
        
    % terminology (Seidlemann eq. 3.32-1 [p. 152])
    % U_B - Barycentric equtaorial J2000.0 star position (cosine dir), at epoch t0
    % U_dotB - Star space motion.
    % E_B - Barycentric position of observer at epoch t.
    
    % convert to radians
    [RA, Dec]=celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','rad', 'Server',Args.Server); % [rad]
    
    % input coordinates 
    Aux.RA_J2000  = RA.*RAD;   % [deg]
    Aux.Dec_J2000 = Dec.*RAD;  % [deg]

    if nargout>4
        Aux.RA_App = NaN;
        Aux.HA_App = NaN;
        Aux.Dec_App = NaN;
        Aux.RA_AppDist = NaN;
        Aux.HA_AppDist = NaN;
        Aux.Dec_AppDist = NaN;
        Aux.Alt_App     = NaN;
        Aux.Az_App      = NaN;
       
        Aux.LST         = celestial.time.lst(JD, Args.GeoPos(1), Args.TypeLST) .* 360; % [deg]
    end

    % calculate space position and space motion of star
    [U_dotB, U_B] = celestial.coo.pm2space_motion(RA, Dec, Args.PM_RA, Args.PM_Dec, Args.Plx, Args.RV); % au/day; au
    % convert to 3xN matrix
    U_dotB = U_dotB.';
    U_B    = U_B.';
    
    % get observer position [au; au/day]
    [E_B, E_dotB] = celestial.SolarSys.earthObserverPos(JD, 'CooSys','b',...
                                                            'RefFrame','eq',...
                                                            'GeoPos',Args.GeoPos,...
                                                            'EarthEphem','inpop',...
                                                            'INPOP',Args.INPOP,...
                                                            'TimeScale',Args.TimeScale);
    
    % apply proper motion and parallax
    U0 = U_B + U_dotB.*(JD - EpochJD) - E_B;
    
    % apply light deflection
    % IGNORE
    U1 = U0;
    
    % apply aberration of light
    % note velocity should be E_dotH (so this is an approximation)
    if Args.ApplyAberration
        Delta = sqrt(sum(U1.^2, 1));
        U1 = U1./Delta;
        U2 = celestial.SolarSys.aberrationSolarSystem(U1, E_dotB, 1);
    else
        U2 = U1;
    end
    
    % Precess/nutate to OutEquinox
    [OutRA, OutDec] = celestial.convert.precessCoo(U2(1,:), U2(2,:), U2(3,:), 'InEquinox',InputEqJD,...
                                                                              'InMean',true,...
                                                                              'InType','JD',...
                                                                              'OutEquinox',Args.OutEquinox,...
                                                                              'OutType',Args.OutEquinoxUnits,...
                                                                              'OutMean',Args.OutMean,...
                                                                              'OutUnits','deg');
    % apply refraction of light
    if Args.ApplyRefraction
        % [DelAlpha,DelDelta]=refraction_coocor(RA,Dec,Ref,varargin)
        % Instead write: celestial.convert.refractedCoo(RA, Dec, Args)
        [OutRA, OutDec, Alt, Refraction]=celestial.convert.refractedCoo(OutRA, OutDec, 'InUnits', 'deg', 'OutUnits','deg',...
                                                                           'JD',JD,...
                                                                           'GeoPos',Args.GeoPos,...
                                                                           'PosUnits','rad',...
                                                                           'TypeLST',Args.TypeLST,...
                                                                           'Wave',Args.Wave,...
                                                                           'Temp',Args.Temp,...
                                                                           'Pressure',Args.Pressure,...
                                                                           'Pw',Args.Pw);
    else
        Alt = NaN;
        Refraction = NaN;
    end

    
    % add shift
    OutRA  = OutRA  + Args.ShiftRA./cosd(OutDec);
    OutDec = OutDec + Args.ShiftDec;

    if nargout>4 || Args.ApplyDistortion
        Aux.RA_App  = OutRA;
        Aux.Dec_App = OutDec;
        Aux.HA_App  = celestial.convert.convert_ha(Aux.RA_App, JD, 'InUnits','deg', 'OutUnits','deg',...
                                                                 'Long',Args.GeoPos(1),...
                                                                 'LongUnits','rad',...
                                                                 'TypeLST','a',...
                                                                 'OutRange','pi');
       Aux.HA_J2000 = celestial.convert.convert_ha(Aux.RA_J2000, JD, 'InUnits','deg', 'OutUnits','deg',...
                                                                 'Long',Args.GeoPos(1),...
                                                                 'LongUnits','rad',...
                                                                 'TypeLST','a',...
                                                                 'OutRange','pi');
    end
        

    if Args.ApplyDistortion
        % apply geometric distortion of mount - mount specific - not
        % astrophysical

        if isnumeric(Args.InterpHA)
            Aux.HA_AppDist  = Aux.HA_App + Args.InterpHA;
        elseif (isa(Args.InterpHA, 'struct') || isa(Args.InterpHA,'scatteredInterpolant')) && ~isempty(Args.InterpHA.Points)
            Aux.HA_AppDist  = Aux.HA_App  + Args.InterpHA(Aux.HA_App, Aux.Dec_App);
        else
            Aux.HA_AppDist  = Aux.HA_App;
        end
        if isnumeric(Args.InterpDec)
            Aux.Dec_AppDist = Aux.Dec_App + Args.InterpDec;
        elseif (isa(Args.InterpDec, 'struct') || isa(Args.InterpDec,'scatteredInterpolant')) && ~isempty(Args.InterpDec.Points)
            Aux.Dec_AppDist = Aux.Dec_App + Args.InterpDec(Aux.HA_App, Aux.Dec_App);
        else
            Aux.Dec_AppDist  = Aux.Dec_App;
        end

        % convert HA to RA
        Aux.RA_AppDist = celestial.convert.convert_ha(Aux.HA_AppDist, JD, 'InUnits','deg', 'OutUnits','deg',...
                                                 'Long',Args.GeoPos(1),...
                                                 'LongUnits','rad',...
                                                 'TypeLST','a',...
                                                 'OutRange','2pi');

        OutRA  = Aux.RA_AppDist;
        OutDec = Aux.Dec_AppDist;

    end

    % add Az/Alt to Aux
    if nargout>4
        %if Args.ApplyDistortion
        %    [Aux.Az_App, Aux.Alt_App]=celestial.coo.hadec2azalt(Aux.HA_AppDist,Aux.Dec_AppDist, Args.GeoPos(2).*RAD, 'deg');
        %else
            [Aux.Az_App, Aux.Alt_App]=celestial.coo.hadec2azalt(Aux.HA_App, Aux.Dec_App, Args.GeoPos(2).*RAD, 'deg');
        %end
        Aux.AirMass = celestial.coo.hardie((90-Aux.Alt_App)./RAD);

    end

    % convert to OutUnits
    AngFactor = convert.angular('deg',Args.OutUnits);
    OutRA     = AngFactor .* OutRA;
    OutDec    = AngFactor .* OutDec;

end
