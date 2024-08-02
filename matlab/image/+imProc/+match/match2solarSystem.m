function [SourcesWhichAreMP, AstCat, Obj] = match2solarSystem(Obj, Args)
    % Match sources in AstroCatalog object to Solar System objects.
    %       This is done using the celestial.OrbitalEl class.
    %       For each source in the AstroCatalog, search for minor planets
    %       at the same position and epoch.
    %       Returns the lines from the AstroCatalog that are matched with
    %       minor planets, and add the angular distance of the match and
    %       the minor planet designation.
    %       The code works in two iterations. In the 1st iteration, we
    %       solve the positions of all the asteroids in the 'OrbEl'
    %       argument based on the Kepler Equation. This search may be not
    %       accurate, especially if the |epoch-JD|>100 days. For all the
    %       asteroids found within the search radius + buffer, an exact
    %       orbital integration is performed.
    % Input  : - An AstroCatalog, or AstroImage object (multi-elements supported).
    %            Note that unless 'CreateNewObj'=true, this ibject may be
    %            modified by the function.
    %          * ...,key,val,...
    %            'JD' - Scalar Julian Day (JD), corresponding to the epoch of the
    %                   catalog. If empty, then
    %                   will attempt to read the JD from the image header,
    %                   or AstroCatalog JD property.
    %                   Default is [].
    %            'OrbEl' - An OrbitalEl class with the orbital elements of
    %                   all the minor planets to match against the sources.
    %                   If empty, (and 'AstCat' is empty) then will load the JPL orbital elements
    %                   from disk. For instellation of the JPL orbital
    %                   elements file see the Installer class.
    %                   If Integration=true, then all bodies must have the
    %                   same epoch.
    %                   Default is [].
    %             'AstCat' - An AstroCatalog containing the catalog of
    %                   asteroids. If given, then OrbEl and searchMinorPlanetsNearPosition
    %                   will be skipped (provide for speed up).
    %                   Default is [].
    %             'INPOP' - A populated celestial.INPOP object.
    %                   If empty, then will be loaded. Default is [].
    %
    %         Coordinates of Image/Catalog center:
    %             'RA' - If 'RA','Dec','FOV_Radius' are given then they will
    %                    be use instead of retrievied from the
    %                    image/catalog.
    %                    Default is [].
    %             'Dec' - Like 'RA', but for Declination.
    %             'FOV_Radius' - Like 'RA', but for FoV radius.
    %             'InCooUnits' - Input Coordinates units.
    %                   This must be provided along with RA, Dec,
    %                   FOV_Radius. Default is [].
    %
    %        Observer position:
    %           'GeoPos' - Geodetic position of the observer (on
    %                   Earth). [Lon (rad), Lat (rad), Height (m)].
    %                   If empty and input is AstroImage then will attempt
    %                   to read from header. If empty, and no header info,
    %                   then will use geocentric position.
    %                   Default is [].
    %            'RefEllipsoid' - Reference ellipsoid for the
    %                   geodetic positions. Default is 'WGS84'.
    %            'KeyLon' - Obs. Longitude main keyword dictionary
    %                   name. Default is 'OBSLON'.
    %            'KeyLat' - Obs. Latitude main keyword dictionary
    %                   name. Default is 'OBSLAT'.
    %            'KeyAlt' - Obs. Altitude main keyword dictionary
    %                   name. Default is 'OBSALT'.
    %
    %            'MagLimit' - Magnitude limit for minor planets. Default is Inf.
    %            'Integration' - A logical indicating if to use orbital integration
    %                   including planetary perturbations (true), or solve
    %                   Kepler equation (false).
    %                   Default is true.
    %
    %            'AddPlanets' - Match sources also against planets.
    %                   THIS OPTION IS NOT YET AVAILABLE.
    %                   Default is false.
    %            'SearchRadius' - Matching search radius between source and
    %                   minor planets. Deafult is 5.
    %            'SearchRadiusUnits' - Units for the SearchRadius argument.
    %                   Default is 'arcsec'.
    %
    %       Columns to be added to SourcesWhichAreMP (first input):
    %            'AddColDist' - Adding match angular distance column to
    %                   output catalog. Default is true.
    %            'ColDistPos' - Position in which to add ang. dist column.
    %                   Default is Inf.
    %            'ColDistName' - Name of ang. dist column.
    %                   Default is 'DistMP'.
    %            'ColDistUnits' - Units of ang. dist column.
    %                   Default is 'arcsec'.
    %            'AddColNmatch' - Adding number of matches column to
    %                   output catalog. Default is true.
    %            'ColNmatchPos' - Position in which to add Nmatch column.
    %                   Default is Inf.
    %            'ColNmatchName' - Name of Nmatch column.
    %                   Default is 'NmatchMP'.
    %            'AddColDesignation' -Adding minor planet designation column to
    %                   output catalog. If true, then the output 'Catalog'
    %                   field in the AStroCatalog will be of table class.
    %                   Default is true.
    %            'ColDesigPos' - Position in which to add designation column.
    %                   Default is Inf.
    %            'ColDesigName' - Name of designation column.
    %                   Default is 'Designation'.
    %            'AddColMag' - A logical indicating if to add predicted
    %                   mag to SourcesWhichAreMP. Default is false.
    %            'ColMag' - Column name for predicted mag.
    %                   Default is 'PredMag'.
    %
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
    %
    %            'AddMag2Obj' - A logical indicating if to add to the
    %                   original input AstroCatalog object also a column
    %                   (named ObjColMag) containing the asteroid
    %                   magnitude.
    %                   Default is false.
    %            'ObjColMag' - The name of the asteroid magnitude column
    %                   name that will be added to the input AstroCatalog object.
    %                   Default is 'MagMP'.
    %
    % Output : - An AstroCatalog object containing only the sources in the
    %            input AstroCatalog that are matched with minor planets.
    %            Possibly adding ang. dist, Nmatch, and minor planet
    %            designation to the output.
    %          - The AstroCatalog of all minor planets in searched radius.
    %          - If nargout>2, then the input 
    %            AstroCatalog object will be modified (see 'CreateNewObj'
    %            argument). The modified object may be sorted and may
    %            include additional information on the angular distance to
    %            the nearest minor planet.
    % Author : Eran Ofek (Sep 2021)
    % Example: OrbEl= celestial.OrbitalEl.loadSolarSystem('num');
    %          OrbEl.propagate2commonEpoch;
    %           IN = celestial.INPOP;
    %           IN.populateAll;
    %           JD = OrbEl.Epoch(1) + 500;
    %           % select some asteroids from JPL:    
    %           T = celestial.SolarSys.getJPL_ephem('600000;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.1); 
    %           Cat = [T.RA, T.Dec] + [[0, 0]; rand(1000,2)-0.5];
    %           AC  = AstroCatalog({Cat}, 'ColNames',{'RA','Dec'}, 'ColUnits',{'deg','deg'});
    %           AC.JD = JD;
    %           % search using OrbitalEl object
    %           AC1 = AC.copy;
    %           [OnlyMP, AstCat, AC1] = imProc.match.match2solarSystem(AC1, 'JD',JD, 'GeoPos',[], 'OrbEl',OrbEl, 'SearchRadius',1, 'INPOP',IN);
    
    arguments
        Obj                                              % AstroCatalog | AstroImage
        Args.JD                            = [];         % [] - take from header
        Args.OrbEl                         = [];
        Args.AstCat                        = [];   % catalog of asteroids
        Args.INPOP                         = [];
        
        Args.RA                            = [];
        Args.Dec                           = [];
        Args.FOV_Radius                    = [];
        Args.InCooUnits                    = [];        
        Args.UseWCS logical                = true;  % 
        
        Args.GeoPos                        = [];
        Args.RefEllipsoid                  = 'WGS84';
        Args.KeyLon                        = 'OBSLON';
        Args.KeyLat                        = 'OBSLAT';
        Args.KeyAlt                        = 'OBSALT';
        
        Args.MagLimit                      = Inf;
        Args.Integration logical           = true;
              
        %Args.AddPlanets(1,1) logical       = false;
        Args.SearchRadius                  = 10;
        Args.SearchRadiusUnits             = 'arcsec';
        
        Args.AddColDist(1,1) logical       = true;
        Args.ColDistPos                    = Inf;
        Args.ColDistName                   = 'DistMP';
        Args.ColDistUnits                  = 'arcsec';

        Args.AddColNmatch(1,1) logical     = true;
        Args.ColNmatchPos                  = Inf;
        Args.ColNmatchName                 = 'NmatchMP';

        Args.AddColDesignation(1,1) logical = true;
        Args.ColDesigPos                    = Inf;
        Args.ColDesigName                   = 'Desig';

        Args.AddColMag logical              = false;
        Args.ColMag                         = 'PredMag';
        
        %Args.CreateNewObj(1,1) logical      = false;

        Args.SourcesColDistPos              = Inf;
        Args.SourcesColDistName             = 'DistMP';
        Args.SourcesColDistUnits            = 'arcsec';
        
        Args.AddMag2Obj                     = false;
        Args.ObjColMag                      = 'MagMP';


    end
    RAD = 180./pi;
    QuickSearchBuffer = 500;  % arcsec

    % read orbital elements from disk
    AstCat = [];
    if isempty(Args.AstCat)
        if isempty(Args.OrbEl) 
            Args.OrbEl= celestial.OrbitalEl.loadSolarSystem('merge');
            if numel(unique(Args.OrbEl.Epoch))>1
                error('OrbEl contains multiple Epochs');
            end
        end
    end
    
    if ~isempty(Args.RA) && ~isempty(Args.Dec) && ~isempty(Args.FOV_Radius)
        Factor          = convert.angular(Args.InCooUnits, 'rad');
        Args.RA         =  Args.RA .* Factor;
        Args.Dec        =  Args.Dec .* Factor;
        Args.FOV_Radius = Args.FOV_Radius .* Factor;
        Args.InCooUnits = 'rad';

    end
    
    Nobj = numel(Obj);
    SourcesWhichAreMP = AstroCatalog(size(Obj));
    for Iobj=1:1:Nobj
        % Get catalog with populated JD
        Cat = imProc.cat.getCat(Obj(Iobj), 'JD',Args.JD);
        
        if ~Cat.isemptyCatalog
            if isempty(Args.AstCat) 
                % Get image/catalog coordinates
                CatCoo = imProc.astrometry.getCooCenter(Obj(Iobj), 'RA',Args.RA,...
                                                               'Dec',Args.Dec,...
                                                               'FOV_Radius',Args.FOV_Radius,...
                                                               'InCooUnits',Args.InCooUnits,...
                                                               'UseWCS',Args.UseWCS,...
                                                               'OutCooUnits','rad');
            
                % Get image/catalog obs position
                if isempty(Args.GeoPos) && isa(Obj, 'AstroImage')
                    [Lon, Lat, Alt] = getObsCoo(Obj(Iobj).HeaderData, 'KeyLon',Args.KeyLon, 'KeyLat',Args.KeyLat, 'KeyAlt',Args.KeyAlt);
                    Args.GeoPos     = [Lon, Lat, Alt]; 
                end
            
                % Generate catalog of asteroids around search coordinates
                [AstCat] = searchMinorPlanetsNearPosition(Args.OrbEl, Cat.JD, CatCoo(1), CatCoo(2), CatCoo(3),...
                                                                             'INPOP',Args.INPOP,...
                                                                             'CooUnits','rad',...
                                                                             'SearchRadiusUnits','rad',...
                                                                             'QuickSearchBuffer',QuickSearchBuffer,...
                                                                             'MagLimit',Args.MagLimit,...
                                                                             'GeoPos',Args.GeoPos,...
                                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                                             'OutUnitsDeg',true,...
                                                                             'Integration',Args.Integration);
                
            else
                AstCat = Args.AstCat;
            end % if isempty(Args.AstCat) 
            
            if isemptyCatalog(AstCat) || isemptyCatalog(Cat)
                % No Asteroids in search radius - skip
                SourcesWhichAreMP(Iobj)    = AstroCatalog;
                SourcesWhichAreMP(Iobj).JD = Cat.JD;
                

                % Fixing issue 410:
                if nargout>2
                    Nline = Cat.sizeCatalog;
                    Obj_DistCol = nan(Nline,1);
                    insertCol(Cat, Obj_DistCol, Args.SourcesColDistPos, Args.SourcesColDistName, Args.SourcesColDistUnits);
                end

            else
    
                % Match AstCat with Cat
                
                % NOTE: Obj may be modified and returned sorted
                ResInd = imProc.match.matchReturnIndices(Cat, AstCat, 'CooType','sphere',...
                                                                      'Radius',Args.SearchRadius,...
                                                                      'RadiusUnits',Args.SearchRadiusUnits);
             
                % we are inside Iobj loop, so there is only one ResInd:
                SourcesWhichAreMP(Iobj) = selectRows(Cat, ResInd.Obj2_IndInObj1, 'IgnoreNaN',true, 'CreateNewObj',true);
                %SourcesWhichAreMP(Iobj) = selectRows(Cat, ResInd.Obj1_IndInObj2, 'IgnoreNaN',true, 'CreateNewObj',true);
        
    
    
                LinesNN = ~isnan(ResInd.Obj2_IndInObj1);
                % add columns: Dist, Nmatch, Designation
                if Args.AddColDist
                    Dist = convert.angular('rad', Args.ColDistUnits, ResInd.Obj2_Dist(LinesNN));
                    SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), Dist, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
                end
        
                if Args.AddColNmatch
                    SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), ResInd.Obj2_NmatchObj1(LinesNN), Args.ColNmatchPos, Args.ColNmatchName, '');
                end
        
                if Args.AddColDesignation
                    Desig = getCol(AstCat, 'Desig', 'SelectRows',LinesNN);
                    SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), Desig, Args.ColDesigPos, Args.ColDesigName, '');
                end
    
                % ADding predicted magnitude column:
                if Args.AddColMag
                    % Args.ColMag
                    PredMag = getCol(AstCat, 'Mag', 'SelectRows',LinesNN);
                    SourcesWhichAreMP(Iobj) = insertCol(SourcesWhichAreMP(Iobj), PredMag, Inf, Args.ColMag, '');
                end
    
    
                % adding a column to Obj(Iobj) indicating if there is a match to a
                % minor planet
                if nargout>2
        
                    Tmp=ResInd.Obj1_IndInObj2;
                    IsnanTmp = isnan(Tmp);
                    if all(IsnanTmp)
                        % no asteroid - add nan column
                        Obj_DistCol = nan(size(ResInd.Obj1_FlagNearest));
                    else
                        Tmp(IsnanTmp) = 1;   
                        Obj_DistCol = ResInd.Obj2_Dist(Tmp);
                        Obj_DistCol = convert.angular('rad', Args.SourcesColDistUnits, Obj_DistCol);
                        % return the NaNs to asources with no counterparts
                        Obj_DistCol(IsnanTmp) = NaN;
                    end
                    insertCol(Cat, Obj_DistCol, Args.SourcesColDistPos, Args.SourcesColDistName, Args.SourcesColDistUnits);
        
                    %Adding Mag column to object:
                    if Args.AddMag2Obj
                        Tmp=ResInd.Obj1_IndInObj2;
                        IsnanTmp = isnan(Tmp);
                        Obj_MagCol = nan(size(ResInd.Obj1_FlagNearest));
                        if all(IsnanTmp)
                            % no asteroid - add nan column
                            Obj_MagCol = nan(size(ResInd.Obj1_FlagNearest));
                        else
                            Tmp(IsnanTmp) = 1;   
                            
                            MagInd  = find(~isnan(ResInd.Obj2_IndInObj1));
                            MagPred = AstCat.getCol(Args.ColMag, false, false, 'SelectRows',MagInd);
                            Obj_MagCol(ResInd.Obj2_IndInObj1(MagInd)) = MagPred;
                            
                        end
                        Cat.insertCol(Obj_MagCol, Inf, {Args.ObjColMag}, {'mag'});
                    end


                    % return the Cat into the original input object
                    if isa(Obj, 'AstroImage')
                        Obj(Iobj).CatData = Cat;
                    elseif isa(Obj, 'AstroCatalog')
                        Obj(Iobj) = Cat;
                    else
                        error('Unknwon first input object type (must be AstroImage or AstroCatalog)');
                    end
                end
            end % isemptyCatalog(AstCat) || isemptyCatalog(Cat)
        else
            SourcesWhichAreMP(Iobj).JD = NaN;
        end % if ~Cat.isemptyCatalog
    end % for Iobj=1:1:Nobj
    
   
        
end

% internal functions
% Geodetic position
function GeoPos = getGeoPos(Obj, Args)
    RAD = 180./pi;
    if isempty(Args.GeoPos)
        if isa(Obj, 'AstroImage')
            [Lon, Lat, Alt] = getObsCoo(Obj.HeaderData, 'KeyLon',Args.KeyLon,...
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
            error('GeoPos must be provided');
        end
    else
        if ischar(Args.GeoPos)
            % geocentric position
            GeoPos = [];
        else
            GeoPos = Args.GeoPos;
        end
    end
end

