function [Result, ColNames, ColUnits] = ephemKeplerMultiObj(Obj, Time, Args)
    % Calculate ephemerides for OrbitalEl object by solving the Kepler equation.
    %   This function is optimized for multi objects.
    %   See ephemKeplerMultiTime for a function optimied for
    %   multiple times.
    %
    %   For each orbital-element or time, return the Geocentric or
    %   topocentric ephemerides of the target.
    %   This is like the ephem function, but not including the
    %   orbital integation option.
    %   This is accurate for times which are close to the epoch.
    %
    %   For definitions and formulae, see Explanatory Supplement to the Astronomical
    %   Alamanac (Seidelmann 2006), chapter 3.313, p. 148.
    % Input  : - A single element OrbitalEl object.
    %            This object may include multiple orbital elements
    %            in vectors of parameters.
    %          - JD (scalar).
    %          * ...,key,val,...
    %            See celestial.ephem.ephemKepler for arguments.
    % Output : - A table/AstroCatalog/matrix of output ephemeris for the
    %            selected targets.
    %          - A cell array of column names.
    %          - A cell array of column units.
    % Author : Eran Ofek (Nov 2023) 
    % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
    %          JD = 2460000;
    %          Result = celestial.ephem.ephemKeplerMultiObj(OrbEl, JD)
    %          [T] = celestial.SolarSys.getJPL_ephem('1;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 

    
    arguments
        Obj(1,1)
        Time(1,1)
        
        Args.INPOP                   = [];
       
        Args.GeoPos                  = [];
        Args.RefEllipsoid            = 'WGS84';
        
        Args.MaxIterLT               = 2;
        Args.TimeScale               = 'TDB';
        Args.ObserverEphem           = [];
        Args.Tol                     = 1e-8;
               
        Args.OutType               = 'AstroCatalog'; % 'mat'|'astrocatalog'|'table'
        Args.Aberration logical    = false;
        Args.OutUnitsDeg logical   = true;
        Args.IncludeMag logical    = true;
        Args.IncludeAngles logical = true;
        Args.IncludeDesignation logical = true;
    end
    RAD  = 180./pi;
    Caud = constant.c.*86400./constant.au;  % speed of light [au/day]

    Ntarget = numEl(Obj);
   
    [E_H, E_Hdot]=celestial.SolarSys.earthObserverPos(Time, 'CooSys','helio',...
                                                            'RefFrame','eq',...
                                                            'INPOP',Args.INPOP,...
                                                            'ObserverEphem',Args.ObserverEphem,...
                                                            'SunLightTime',0,...
                                                            'RefEllipsoid',Args.RefEllipsoid);

    % Rotation matrix: Ecliptic to Equatorial, J2000
    RotMatEc2Eq = celestial.coo.rotm_coo('E');

    LightTime             = 0;
    for Iter=1:1:Args.MaxIterLT

        % Solution via Kepler equation
        % Target, Heliocentric, ecliptic
        [Nu, R]          = keplerSolve(Obj, Time-LightTime,'Tol',Args.Tol);
        % Target, Ecliptic Heliocentric rect. position
        [U_H] = trueAnom2rectPos(Obj, Nu, R, 'rad');
        U_H   = U_H.';  % a 3 X N matrix
        % convert to Equatorial, Heliocentric
        U_H   = RotMatEc2Eq * U_H;

        U = U_H - E_H;  % U_B(t-tau)
        Delta = sqrt(sum(U.^2, 1));

        %PrevLightTime = LightTime;
        LightTime = Delta./Caud;

    end
    %R     = sqrt(sum(U_B.^2, 1));
    
    % prepare output table:
    [Result, ColNames, ColUnits] = prepEphemOutput(Obj, Time,...
                                                   U, U_H, ...
                                                   E_H,...
                                                   E_Hdot,...
                                                   'OutType',Args.OutType,...
                                                   'Aberration',Args.Aberration,...
                                                   'OutUnitsDeg',Args.OutUnitsDeg,...
                                                   'IncludeMag',Args.IncludeMag,...
                                                   'IncludeAngles',Args.IncludeAngles,...
                                                   'IncludeDesignation',Args.IncludeDesignation);        
    
end
