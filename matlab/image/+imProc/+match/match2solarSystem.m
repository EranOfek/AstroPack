function Result = match2solarSystem(Obj, Args)
    %
    % Example: Cat = ephem(OrbEl, JD, 'AddDesignation',false);
    %          R = rand(10,8); R(:,2:3) = R(:,2:3) + [82 11];
    %          Cat1 = AstroCatalog({R});
    %          Cat2  = merge([Cat;Cat1]);
    %          Result = imProc.match.match2solarSystem(Cat2, 'JD',JD, 'GeoPos',[]);
    
    arguments
        Obj                                              % AstroCatalog | AstroImage
        Args.JD                            = [];         % [] - take from header
        Args.OrbEl                         = [];         % [] - read from disk | OrbitalEl object | AstroCatalog object with asteroids
        Args.AddPlanets(1,1) logical       = false;
        Args.SearchRadius                  = 5;
        Args.SearchRadiusUnits             = 'arcsec';
        Args.MagLimit                      = Inf;
        Args.GeoPosFromHeader logical      = true;
        Args.KeyLon                        = 'OBSLON';
        Args.KeyLat                        = 'OBSLAT';
        Args.KeyAlt                        = 'OBSALT';
        Args.IsInputAlt logical            = false;
        Args.GeoPos                        = [];
        Args.RefEllipsoid                  = 'WGS84';

        Args.AddColDist(1,1) logical       = true;
        Args.ColDistPos                    = Inf;
        Args.ColDistName                   = 'Dist';
        Args.ColDistUnits                  = 'arcsec';

        Args.AddColNmatch(1,1) logical     = true;
        Args.ColNmatchPos                  = Inf;
        Args.ColNmatchName                 = 'Nmatch';

        Args.AddColDesignation(1,1) logical = true;

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
            if Args.GeoPosFromHeader
                if isa(Obj, 'AstroImage')
                    [Lon, Lat, Alt] = getObsCoo(Obj(Iobj).HeaderData, 'KeyLon',Args.KeyLon,...
                                                                  'KeyLat',Args.KeyLat,...
                                                                  'KeyAlt',Args.KeyAlt,...
                                                                  'IsInputAlt',Args.IsInputAlt); % assmed [deg, deg, m]
                    if isnan(Lon) || isnan(Lat) || isnan(Alt)
                        GeoPos = [];
                    else
                        GeoPos = [Lon./RAD, Lat./RAD, Alt];  % assume [deg deg m] -> [rad rad m]
                    end
                else
                    % no header - try to use user input
                    GeoPos = Args.GeoPos;
                end
            else
                GeoPos = Args.GeoPos;
            end
        else
            GeoPos = Args.GeoPos;
        end
        
        % get bounding box
        [RA, Dec, FOV_Radius] = boundingCircle(Obj(Iobj), 'OutUnits','rad' ,'CooType','sphere');
        
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
                                                                         'AddDesignation',true,...
                                                                         'OutUnitsDeg',true);
        end

        
        % merge ResultNear
        % NOTE: Obj may be modified and returned sorted
        ResInd = imProc.match.matchReturnIndices(Obj(Iobj), ResultNear, 'CooType','sphere',...
                                                                        'Radius',Args.SearchRadius,...
                                                                        'RadiusUnits',Args.SearchRadiusUnits);
        
        SourcesWhichAreMP(Iobj) = selectRows(Obj(Iobj), ResInd(Iobj).Obj2_IndInObj1, 'IgnoreNaN',true, 'CreateNewObj',true);

        LinesNN = ~isnan(ResInd(Iobj).Obj2_IndInObj1);
        % add columns: Dist, Nmatch, Designation
        if Args.AddColDist
            Dist = convert.angular('rad', Args.ColDistUnits, ResInd(Iobj).Obj2_Dist(LinesNN));
            SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), Dist, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
        end
        
        if Args.AddColNmatch
            SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), ResInd(Iobj).Obj2_NmatchObj1(LinesNN), Args.ColNmatchPos, Args.ColNmatchName, '');
        end

        if Args.AddColDesignation
            Desig = getCol(ResultNear(Iobj), 'Designation', 'SelectRows',LinesNN);
error('insertCol need to take care of cells/tables')
            SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), Desig, Inf, 'Designation', '');
        end
    end
    Result = merge(SourcesWhichAreMP);
end