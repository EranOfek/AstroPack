function [Result, ObjTime] = ephemMultiObj(Obj, Time, Args)
    % Generate multiple targets ephemeris for a single epoch by orbital integration.
    %   Given orbital elements of multiple targets with the same
    %   epoch, integrate the position of the targets to the (scalar)
    %   requested time, taking into account planetray
    %   perturbations. The propagated position is then transformed
    %   back to orbital elements.
    %   The object ephemeris for an observer are evaluated by
    %   solving the Kepler equation in the new epoch (taking into
    %   account light time corrections).
    %   This function is typically good to a sub arcsecond
    %   precision.
    %       List of other related functions:
    %
    % Input  : - A single element celestial.OrbitalEl object that
    %            may contain orbital elements for multiple targets.
    %            All the targets must have a common epoch.
    %            Use the function commonEpoch to convert all
    %            elements to the same epoch.
    %          - Julian day (scalar) of requested ephemeris.
    %          * ...,key,val,...
    %            'INPOP' - A populated celestial.INPOP object.
    %                   If empty, will generate one.
    %            'Integration' - A logical indicating if to use orbital
    %                   integration in the first iteration (up to the light
    %                   time correction). Default is true.
    %            'IntegrationLT' - A logical indicating if to do orbital
    %                   integration in the light time correction.
    %                   (Not really needed).
    %                   Default is false.
    %
    %            'GeoPos' - Geodetic position of the observer (on
    %                   Earth). [Lon (rad), Lat (rad), Height (m)].
    %                   If empty, then calculate geocentric
    %                   positions. Default is [].
    %            'RefEllipsoid' - Reference ellipsoid for the
    %                   geodetic positions. Default is 'WGS84'.
    %
    %            'MaxIterLT' - Maximum numbre of iterations for
    %                   light-time corrections. Default is 2.
    %                   1 will force to no ligh-time correction
    %                   (e.g., for quick calculation).
    %            'TimeScale' - Time scale of JD. Relevant only if
    %                   EarthEphem='INPOP'.
    %                   Default is 'TDB'.
    %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
    %                   Heliocentric equatorial coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
    %                   If empty, the function will use EarthEphem and GeoPos.
    %                   In case of size [Nepoch,3], the function assume zero velocity.
    %                   Defauls is [].
    %            'Tol' - Tolerance [rad] for solving the Kepler
    %                   equation. Default is 1e-8.
    %            'TolInt' - Tolerance for integration. Default is 1e-8.
    %
    %            'OutType' - Output type:
    %                   'mat' - A matrix output.
    %                   'AstroCatalog' - An AstroCatalog object.
    %                   'table' - A table.
    %            'OrbEl' - An optional celestial.OrbitalEl object.
    %                   Will be used for the mag calculations.
    %                   If empty, then exclude mag.
    %                   Default is [].
    %
    %            'OutUnitsDeg' - A logical indicating if to list
    %                   the RA and Dec in degrees. If false list in
    %                   radians. Default is true.
    %            'Aberration' - A logical indicating if to include
    %                   aberration of light. Default is false.
    %                   Note that for the default (false) the
    %                   output is in an "astrometric" reference
    %                   frame (i.e., relative to the stars).
    %            'IncludeMag' - A logical indicating if to add
    %                   magnitude to the output table.
    %                   Default is true.
    %            'IncludeAngles' - A logical indicating if to
    %                   include angles. Default is true.
    %            'IncludeDesignation' - A logical indicatig if to
    %                   include desigmation.
    %                   Default is true.
    % Output : - A table/AstroCatalog/matrix of output ephemeris for the
    %            selected targets.
    %          - A celestial.OrbitalEl object with the propagated orbital
    %            elements to the new epoch.
    % Author : Eran Ofek (Nov 2023)
    % Example: OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9900]);
    %          IN=celestial.INPOP;
    %          IN.populateAll;
    %          JD = 2460000;
    %          Result = celestial.ephem.ephemMultiObj(OrbEl, JD, 'INPOP',IN)
    %          % compare to JPL
    %          [T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
    %          [(Result.Catalog.Dec(1)-T.Dec(1)), (Result.Catalog.RA(1)-T.RA(1))].*3600
    
    arguments
        Obj(1,1)
        Time(1,1)
        Args.INPOP                   = [];
        Args.Integration logical     = true;
        Args.IntegrationLT logical   = false;
        
        Args.GeoPos                  = [];
        Args.RefEllipsoid            = 'WGS84';
        
        Args.MaxIterLT               = 2;
        Args.TimeScale               = 'TDB';
        Args.ObserverEphem           = [];
        Args.Tol                     = 1e-8;
        Args.TolInt                  = 1e-8;

        Args.OutType               = 'AstroCatalog'; % 'mat'|'astrocatalog'|'table'
        Args.Aberration logical    = false;
        Args.OutUnitsDeg logical   = true;
        Args.IncludeMag logical    = true;
        Args.IncludeAngles logical = true;
        Args.IncludeDesignation logical = true;

    end

    Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
    if Args.Integration
        % Propgate the targets orbital elements to the new epoch (Time)
        % Here U_B, S_B are in the ecliptic system
        [ObjTime, U_B, U_Bdot, S_B, S_Bdot] = integrateElements(Obj, Time, 'TimeScale',Args.TimeScale,...
                                               'INPOP',Args.INPOP,...
                                               'Tol',Args.Tol,...
                                               'TolInt',Args.TolInt);
        % DEBUG: compare with:
        %[OrbEl_J] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB','StartTime',Time,'StopTime',Time+0.5, 'OutType','OrbitalEl');
    else
        % Use orbital elements with no change
        ObjTime = Obj;
    end
    
    % Use the new orbital elements to calculate the target position                                  
    [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(ObjTime, Time, 'Integration',false,...
                                                          'INPOP',Args.INPOP,...
                                                          'CooSys','eq',...
                                                          'TimeScale',Args.TimeScale,...
                                                          'RefFrame','bary',...
                                                          'LightTime',0,...
                                                          'SunLightTime',0,...
                                                          'Tol',Args.Tol,...
                                                          'TolInt',Args.TolInt);
    % compare with:
    %[T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','VECTORS','TimeScale','TDB','StartTime',Time,'StopTime',Time+0.5, 'OutType','OrbitalEl','CENTER','500@0');
    %celestial.coo.rotm_coo('E') * [T.X;T.Y;T.Z]
                                                         
                                                     
    % Observer position
    [E_B, E_Bdot] = celestial.SolarSys.earthObserverPos(Time, 'CooSys','bary',...
                                                              'RefFrame','eq',...
                                                              'INPOP',Args.INPOP,...
                                                              'SunLightTime',0,...
                                                              'TimeScale',Args.TimeScale,...
                                                              'ObserverEphem',Args.ObserverEphem,...
                                                              'GeoPos',Args.GeoPos,...
                                                              'RefEllipsoid',Args.RefEllipsoid,...
                                                              'OutUnits','au');
                                                              
    
    % Topocentric position
    U = U_B - E_B;  % U_B(t-tau)
    % Topocentric distance
    Delta = sqrt(sum(U.^2, 1));
    % Light time correction iteration
    if Args.MaxIterLT>1
        LightTime = Delta./Caud;   % scalar (single object)     
        [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(ObjTime, Time, 'Integration',Args.IntegrationLT,...
                                                         'INPOP',Args.INPOP,...
                                                         'LightTime',LightTime,...
                                                         'SunLightTime',0,...
                                                         'CooSys','eq',...
                                                         'TimeScale',Args.TimeScale,...
                                                         'RefFrame','bary',...
                                                         'Tol',Args.Tol,...
                                                         'TolInt',Args.TolInt);
                                                                                            
                                                         
        %
        U = U_B - E_B;  % U_B(t-tau)
    end
    
    % prepare output table:
    [Result, ColNames, ColUnits] = prepEphemOutput(Obj, Time,...
                                                   U, U_B, ...
                                                   E_B - S_B,...
                                                   E_Bdot - S_Bdot,...
                                                   'OutType',Args.OutType,...
                                                   'Aberration',Args.Aberration,...
                                                   'OutUnitsDeg',Args.OutUnitsDeg,...
                                                   'IncludeMag',Args.IncludeMag,...
                                                   'IncludeAngles',Args.IncludeAngles,...
                                                   'IncludeDesignation',Args.IncludeDesignation);
    

end
