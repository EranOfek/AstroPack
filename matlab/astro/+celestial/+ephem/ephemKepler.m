function [Result,ColNames,ColUnits] = ephemKepler(Obj, Time, Args)
    % Calculate ephemerides for OrbitalEl object.
    %   This function calls ephemKeplerMultiObj or
    %   ephemKeplerMultiTime, based on the input.
    %
    %   For each orbital-element or time, return the Geocentric or
    %   topocentric ephemerides of the target.
    %
    %
    %   For definitions and formulae, see Explanatory Supplement to the Astronomical
    %   Alamanac (Seidelmann 2006), chapter 3.313, p. 148.
    % Input  : - A single element OrbitalEl object.
    %            This object may include multiple orbital elements
    %            in vectors of parameters.
    %            If time is a vector then only one target is allowd.
    %          - A vector of JD.
    %          * ...,key,val,...
    %            'INPOP' - A populated celestial.INPOP object.
    %                   If empty, will generate one.
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
    %          - A cell array of column names.
    %          - A cell array of column units.
    % Author : Eran Ofek (2023 Nov) 
    % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);
    %          JD = OrbEl.Epoch;
    %          Result = celestial.ephem.ephemKepler(OrbEl, JD+(0:1:10)');
    %          [T] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+10.5); 
    %
    %          OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
    %          JD = 2460000;
    %          Result = celestial.ephem.ephemKepler(OrbEl, JD)
    %          [T] = celestial.SolarSys.getJPL_ephem('1;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 


    arguments
        Obj(1,1)
        Time
        
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

    Ntarget = Obj.numEl;
    Ntime   = numel(Time);
            
    if Ntarget>1 && Ntime>1
        error('Number of targets or number of times must be 1');
    end

    if isempty(Args.INPOP)
        Args.INPOP = celestial.INPOP;
        Args.INPOP.populateAll;
    end
            
    ArgsCell = namedargs2cell(Args);

    % use kepler equation
    if Ntarget==1
        [Result,ColNames,ColUnits] = celestial.ephem.ephemKeplerMultiTime(Obj, Time, ArgsCell{:});
    
    else
        % Ntime==1
        [Result,ColNames,ColUnits] = celestial.ephem.ephemKeplerMultiObj(Obj, Time, ArgsCell{:});
    end
            


end
