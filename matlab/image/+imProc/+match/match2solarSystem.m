function [SourcesWhichAreMP, Obj] = match2solarSystem(Obj, Args)
    % Match an AstroCatalog object sources to Solar System objects.
    %       This is done using the celestial.OrbitalEl class.
    %       For each source in the AstroCatalog, search for minor planets
    %       at the same position and epoch.
    %       Returns the lines from the AstroCatalog that are matched with
    %       minor planets, and add the angular distance of the match and
    %       the minor planet designation.
    % Input  : - An AstroCatalog, or AstroImage object (multi-elements supported).
    %            Note that unless 'CreateNewObj'=true, this ibject may be
    %            modified by the function.
    %          * ...,key,val,...
    %            'JD' - Scalar Julian Day (JD), corresponding to the epoch of the
    %                   catalog. If empty, and input is an AstroImage, then
    %                   will attempt to read the JD from the image header.
    %                   Default is [].
    %            'OrbEl' - An OrbitalEl class with the orbital elements of
    %                   all the minor planets to match against the sources.
    %                   If empty, then will load the JPL orbital elements
    %                   from disk. For instellation of the JPL orbital
    %                   elements file see the Installer class.
    %                   Default is [].
    %            'AddPlanets' - Match sources also against planets.
    %                   THIS OPTION IS NOT YET AVAILABLE.
    %                   Default is false.
    %            'SearchRadius' - Matching search radius between source and
    %                   minor planets. Deafult is 5.
    %            'SearchRadiusUnits' - Units for the SearchRadius argument.
    %                   Default is 'arcsec'.
    %            'MagLimit' - Magnitude limit for minor planets. Default is Inf.
    %            'GeoPosFromHeader' - A logical indicating if to attempt
    %                   reading Geodetic position from header.
    %                   Default is true.
    %            'KeyLon' - Obs. Longitude main keyword dictionary
    %                   name. Default is 'OBSLON'.
    %            'KeyLat' - Obs. Latitude main keyword dictionary
    %                   name. Default is 'OBSLAT'.
    %            'KeyAlt' - Obs. Altitude main keyword dictionary
    %                   name. Default is 'OBSALT'.
    %            'IsInputAlt' - IsInputAlt argument to pass to
    %                   getVal. If true, will search keyword name
    %                   in alternate names list. Default is false.
    %            'GeoPos' - Geodetic position of the observer (on
    %                   Earth). [Lon (rad), Lat (rad), Height (m)].
    %                   If empty, then calculate geocentric
    %                   positions. If 'GeoPosFromHeader'=true, then will
    %                   attemp getting the position from the header.
    %                   Default is [].
    %            'RefEllipsoid' - Reference ellipsoid for the
    %                   geodetic positions. Default is 'WGS84'.
    %            'AddColDist' - Adding match angular distance column to
    %                   output catalog. Default is true.
    %            'ColDistPos' - Position in which to add ang. dist column.
    %                   Default is Inf.
    %            'ColDistName' - Name of ang. dist column.
    %                   Default is 'Dist'.
    %            'ColDistUnits' - Units of ang. dist column.
    %                   Default is 'arcsec'.
    %            'AddColNmatch' - Adding number of matches column to
    %                   output catalog. Default is true.
    %            'ColNmatchPos' - Position in which to add Nmatch column.
    %                   Default is Inf.
    %            'ColNmatchName' - Name of Nmatch column.
    %                   Default is 'Nmatch'.
    %            'AddColDesignation' -Adding minor planet designation column to
    %                   output catalog. If true, then the output 'Catalog'
    %                   field in the AStroCatalog will be of table class.
    %                   Default is true.
    %            'ColDesigPos' - Position in which to add designation column.
    %                   Default is Inf.
    %            'ColDesigName' - Name of designation column.
    %                   Default is 'Designation'.
    %            'CreateNewObj' - {false|true} Indicating if to create a
    %                   new copy of the input Astrocatalog object.
    %                   Default is true.
    %       If nargout>1, then add, for each
    %       source in the input AstroCatalog object, the
    %       angular distance to the nearest minor planet (NaN if no match).
    %            'SourcesColDistPos' - The position of the ang. dist.
    %                   column that will be added to the input AstroCatalog
    %                   object. Default is Inf.
    %            'SourcesColDistName' - The name of the ang. dist.
    %                   column that will be added to the input AstroCatalog
    %                   object. Default is 'DistMP.
    %            'SourcesColDistUnits' - The units of the ang. dist. added
    %                   to the input AstroCatalog object. 
    %                   Default is 'arcsec'.
    % Output : - An AstroCatalog object containing only the sources in the
    %            input AstroCatalog that are matched with minor planets.
    %            Possibly adding ang. dist, Nmatch, and minor planet
    %            designation to the output.
    %          - If a second output is requested, then the input
    %            AstroCatalog object will be modified (see 'CreateNewObj'
    %            argument). The modified object may be sorted and may
    %            include additional information on the angular distance to
    %            the nearest minor planet.
    % Author : Eran Ofek (Sep 2021)
    % Example: Cat = ephem(OrbEl, JD, 'AddDesignation',false);
    %          R = rand(10,8); R(:,2:3) = R(:,2:3) + [82 11];
    %          Cat1 = AstroCatalog({R});
    %          Cat2  = merge([Cat;Cat1]);
    %          Cat2  = deleteCol(Cat2, 'JD');
    %          Result = imProc.match.match2solarSystem(Cat2, 'JD',JD, 'GeoPos',[]);
    %          O = celestial.OrbitalEl.loadSolarSystem;
    %          Result = imProc.match.match2solarSystem(Cat2, 'JD',JD, 'GeoPos',[], 'OrbEl',O);
    %          [Result, CatOut] = imProc.match.match2solarSystem(Cat2, 'JD',JD, 'GeoPos',[], 'OrbEl',O);
    
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
        Args.ColDesigPos                    = Inf;
        Args.ColDesigName                   = 'Designation';

        Args.CreateNewObj(1,1) logical      = true;

        Args.SourcesColDistPos              = Inf;
        Args.SourcesColDistName             = 'DistMP';
        Args.SourcesColDistUnits            = 'arcsec';

    end
    RAD = 180./pi;

    % read orbital elements from disk
    if isempty(Args.OrbEl)
        Args.OrbEl= celestial.OrbitalEl.loadSolarSystem;
    end
    % merge OrbitalEl object into a single element object
    %OrbEl = merge(Args.OrbEl);
    
    
    Nobj = numel(Obj);
    SourcesWhichAreMP = AstroCatalog(size(Obj));
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

        if Args.CreateNewObj
            Cat = Cat.copyObject;
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

        

        % merge ResultNear - output is table
        ResultNear = merge(ResultNear,'IsTable',true);

        % NOTE: Obj may be modified and returned sorted
        ResInd = imProc.match.matchReturnIndices(Cat, ResultNear, 'CooType','sphere',...
                                                                        'Radius',Args.SearchRadius,...
                                                                        'RadiusUnits',Args.SearchRadiusUnits);
        % we are inside Iobj loop, so there is only one ResInd:
        SourcesWhichAreMP(Iobj) = selectRows(Obj(Iobj), ResInd.Obj2_IndInObj1, 'IgnoreNaN',true, 'CreateNewObj',true);

        LinesNN = ~isnan(ResInd(Iobj).Obj2_IndInObj1);
        % add columns: Dist, Nmatch, Designation
        if Args.AddColDist
            Dist = convert.angular('rad', Args.ColDistUnits, ResInd.Obj2_Dist(LinesNN));
            SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), Dist, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
        end
        
        if Args.AddColNmatch
            SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), ResInd.Obj2_NmatchObj1(LinesNN), Args.ColNmatchPos, Args.ColNmatchName, '');
        end

        if Args.AddColDesignation
            Desig = getCol(ResultNear(Iobj), 'Designation', 'SelectRows',LinesNN);
            SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), Desig, Args.ColDesigPos, Args.ColDesigName, '');
        end

        % adding a column to Obj(Iobj) indicating if there is a match to a
        % minor planet
        if nargout>1

            Tmp=ResInd.Obj1_IndInObj2;
            IsnanTmp = isnan(Tmp);
            if all(IsnanTmp)
                % no asteroid - add nan column
                Obj_DistCol = nan(size(ResInd.Obj1_FlagNearest));
            else
                Tmp(IsnanTmp) = 1;   
                Obj_DistCol = ResInd.Obj2_Dist(Tmp);
                Obj_DistCol = convert.angular('rad', Args.SourcesColDistUnits, Obj_DistCol);
            end
            insertCol(Cat, Obj_DistCol, Args.SourcesColDistPos, Args.SourcesColDistName, Args.SourcesColDistUnits);

            % return the Cat into the original input object
            if isa(Obj, 'AstroImage')
                Obj(Iobj).CatData = Cat;
            elseif isa(Obj, 'AstroCatalog')
                Obj(Iobj) = Cat;
            else
                error('Unknwon first input object type (must be AstroImage or AstroCatalog)');
            end
        end
    end

end