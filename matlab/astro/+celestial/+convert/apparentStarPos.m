function [OutRA, OutDec] = apparentStarPos(RA, Dec, JD, Args)
    % Calculate apparent position of stars
    %   Including:
    %   proper motion
    %   parallax
    %   abberation of light
    %   (no light deflection)
    %   precession and nutation
    %   atmospheric refraction
    % Input  : - J2000.0 R.A.
    %          - J2000.0 Dec.
    %          - JD of position at time scale (default 'TDB').
    %            Default is celestial.time.julday().
    %          * ...,key,val,... 
    %            'InCooUnits' - Units of input coo. 'deg'|'rad'.
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
    %                   Default is [].
    %            'TimeScale' - Default is 'TDB'.
    %            'ApplyAberration' - Apply aberration of light.
    %                   Default is true.
    %            'ApplyRefraction' - Apply atmospheric refraction.
    %                   Default is true.
    % Output : - Apparent RA
    %          - Apparent Dec
    % Author : Eran Ofek (2024 Jan) 
    % Example: [OutRA, OutDec] = celestial.convert.apparentStarPos(180, 0, celestial.time.julday([1 1 2024]))

    arguments
        RA
        Dec
        JD                     = celestial.time.julday();
        Args.InCooUnits        = 'deg';
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
        
        Args.GeoPos            = [];   % [rad rad m]
        Args.TimeScale         = 'TDB';
        
        Args.ApplyAberration logical = true;
        Args.ApplyRefraction logical = true;
    end
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
    AngFactor = convert.angular(Args.InCooUnits, 'rad');
    RA        = AngFactor .* RA;
    Dec       = AngFactor .* Dec;
    
    % calculate space position and space motion of star
    [U_dotB, U_B] = celestial.coo.pm2space_motion(RA, Dec, Args.PM_RA, Args.PM_Dec, Args.Plx, Args.RV); % au/day; au
    % convert to 3xN matrix
    U_dotB = U_dotB.';
    U_B    = U_B.';
    
    % get observer position
    [E_B, E_dotB] = celestial.SolarSys.earthObserverPos(JD, 'CooSys','b', 'RefFrame','eq', 'GeoPos',Args.GeoPos, 'INPOP',Args.INPOP, 'TimeScale',Args.TimeScale);
    
    % apply proper motion and parallax
    U0 = U_B + U_dotB.*(JD - EpochJD) - E_B;
    
    % apply light deflection
    % IGNORE
    U1 = U0;
    
    % apply abberation of light
    % note velocity should be E_dotH (so this is an approximation)
    if Args.ApplyAberration
        Delta = sqrt(sum(U1.^2, 1));
        U2 = celestial.SolarSys.aberrationSolarSystem(U1, E_dotB, Delta);
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
                                                                              'OutUnits',Args.OutUnits);
    % apply refraction of light
    if Args.ApplyRefraction
        % [DelAlpha,DelDelta]=refraction_coocor(RA,Dec,Ref,varargin)
        % Instead write: celestial.convert.refractedCoo(RA, Dec, Args)
        error('refraction not yet ready');
    end
    
end
