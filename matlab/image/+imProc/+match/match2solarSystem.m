function Result = match2solarSystem(Obj, Args)
    %
    
    arguments
        Obj                                              % AstroCatalog | AstroImage
        Args.JD                            = [];         % [] - take from header
        Args.OrbEl                         = [];         % [] - read from disk | OrbitalEl object | AstroCatalog object with asteroids
        Args.AddPlanets(1,1) logical       = false;
        Args.SearchRadius                  = 5;
        Args.SearchRadiusUnits             = 'arcsec';
        Args.MagLimit                      = Inf;
        Args.LonKey                        = 
        Args.LatKey                        = 
        Args.HeightKey                     = 
        Args.GeoPos                        = [];
        Args.RefEllipsoid                  = 'WGS84';
        Args.getObsCooArgs cell            = {};
    end
    RAD = 180./pi;

    % read orbital elements from disk
    if isempty(Args.OrbEl)
        Args.OrbEl= celestial.OrbitalEl.loadSolarSystem;
    end
    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
       
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
            if isempty(Args.JD)
                JD  = julday(Obj(Iobj));
            else
                JD = Args.JD;
            end
        elseif isa(Obj, 'AstroCatalog')
            Cat = Obj(Iobj);
            if isempty(Args.JD)
                error('When first input argument is AstroCatalog, JD argument must provided');
            else
                JD = Args.JD;
            end
        else
            error('Unknwon first input object type (must be AstroImage or AstroCatalog)');
        end
        
        % Geodetic position
        if isempty(Args.GeoPos)
            % attempt to read Geodetic position from header
            [Lon, Lat, Alt] = getObsCoo(Obj(Iobj), Args.getObsCooArgs{:}); % assmed [deg, deg, m]
            GeoPos = [Lon./RAD, Lat./RAD, Alt];   % assume [rad, rad. m]
        else
            GeoPos = Args.GeoPos;
        end
        
        % get bounding box
        [RA, Dec, FOV_Radius] = boundingCircle(Obj(Iobj), 'OutUnits','rad' ,'CooTy[e','sphere');
        
        % search all asteroids within bounding box
        if isa(Args.OrbEl, 'AstroCatalog')
            % user supplied an AstroCatalog object with asteroids found in
            % reegion
        else
            % assume the user supplied an OrbitalEl object
            % find their coordinates
            [ResultNear, Names] = searchMinorPlanetsNearPosition(Args.OrbEl, JD, RA, Dec, FOV_Radius,...
                                                                         'SearchRadiusUnits','rad',...
                                                                         'CooUnits','rad',...
                                                                         'MagLimit',Args.MagLimit,...
                                                                         'GeoPos',GeoPos,...
                                                                         'RefEllipsoid',Args.RefEllipsoid,...
                                                                         'OutUnitsDeg',true);
        end

        % add object names
        
        % merge ResultNear
        % imProc.match.match
        
        % match
        
        
        
        
    
    end
end