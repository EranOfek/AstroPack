function Result = match2solarSystem(Obj, Args)
    %
    
    arguments
        Obj                                              % AstroCatalog | AstroImage
        Args.JD                            = [];         % [] - take from header
        Args.OrbEl                         = [];         % [] - read from disk
        Args.AddPlanets(1,1) logical       = false;
        Args.SearchRadius                  = 5;
        Args.SearchRadiusUnits             = 'arcsec';
        Args.MagLimit                      = Inf;
        Args.LonKey                        = 
        Args.LatKey                        = 
        Args.HeightKey                     = 
        Args.GeoPos                        = [];
        Args.RefEllipsoid                  = 'WGS84';
    end
    
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
            
        else
            GeoPos = Args.GeoPos;
        end
        
        % get bounding box
        RA  =
        Dec = 
        FoV =
        
        % search all asteroids within bounding box
        [ResultNear, Names] = searchMinorPlanetsNearPosition(OrbEl, JD, RA, Dec, FoV, 'SearchRadiusUnits',??,...
                                                                         'CooUnits',??,...
                                                                         'MagLimit',Args.MagLimit,...
                                                                         'GeoPos',GeoPos,...
                                                                         'RefEllipsoid',Args.RefEllipsoid,...
                                                                         'OutUnitsDeg',??);
        % add object names
        
        % merge ResultNear
        
        % match
        
        
        
        
    
    end
end