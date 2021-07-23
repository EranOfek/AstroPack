function astrometryRefine(Obj, Args)
    % Refine an astrometric solution of an AstroCatalog object
    %   
    
    
    arguments
        Obj AstroCatalog
        Args.Header         = []; % If given convert to AstroWCS
        Args.WCS            = []; % If given generate RA/Dec for sources
        
        
        
        Args.CatColNamesX                 = AstroCatalog.DefNamesX;
        Args.CatColNamesY                 = AstroCatalog.DefNamesY;
        Args.CatColNamesMag               = AstroCatalog.DefNamesMag;
        Args.RefColNamesRA                = AstroCatalog.DefNamesRA;
        Args.RefColNamesDec               = AstroCatalog.DefNamesDec;
    end
    
    % Case 1. AstroCatalog contains RA/Dec
    % Case 2. Use AstroWCS
    % Case 3. Use AstroHeader
    
    % get X/Y columns from catalog
    [Xcat,~,IndCatX] = getColDic(MatchedCat, Args.CatColNamesX);
    [Ycat,~,IndCatY] = getColDic(MatchedCat, Args.CatColNamesY);
                
    if ~isempty(Args.Header)
        % convert AstroHeader to AstroWCS
        % populate Args.WCS
        Args.WCS = AstroWCS.header2wcs();
    end
    if ~isempty(Args.WCS)
        % Convert X/Y to RA/Dec using AstroWCS
        [SrcRA, SrcDec] = Args.WCS.xy2sky(Xcat, Ycat, 'rad');
    end
    
    % Get astrometric catalog / incluidng proper motion
    % RA and Dec output are in radians
    % If CatName is an AstroCatalog, then will retun as is, but RA and Dec
    % will be converted to OutUnits
    [AstrometricCat, RA, Dec] = imProc.cat.getAstrometricCatalog(Args.RA, Args.Dec, 'CatName',Args.CatName,...
                                                                                    'CatOrigin',Args.CatOrigin,...
                                                                                    'Radius',Args.CatRadius,...
                                                                                    'RadiusUnits',Args.CatRadiusUnits,...
                                                                                    'CooUnits',Args.CooUnits,...
                                                                                    'OutUnits','rad',...
                                                                                    'Con',Args.Con,...
                                                                                    'EpochOut',Args.EpochOut,...
                                                                                    'argsProperMotion',Args.argsProperMotion,...
                                                                                    'ColNameMag',Args.RefColNameMag,...
                                                                                    'RangeMag',Args.RefRangeMag,...
                                                                                    'ColNamePlx',Args.RefColNamePlx,...
                                                                                    'RangePlx',Args.RefRangePlx);
          
    % RA/Dec in [deg]
    RAdeg  = RA.*RAD;
    Decdeg = Dec.*RAD;
    
    % estimate plate scale
    ProjectionScale = (180./pi) .* 3600 ./ mean(Args.Scale);
    
    % projection
    ProjAstCat = imProc.trans.projection(AstrometricCat, RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                   'AddNewCols',{RefColNameX,RefColNameY},...
                                                                                   'CreateNewObj',true);
    % filter Cat - remove sources with neighboors
    
    % match the RA/Dec against an external catalog
    [MatchedCat,UM,TUM] = imProc.match.match(FilteredCat, TransformedProjAstCat,...
                                                 'Radius',Args.SearchRadius,...
                                                 'CooType','pix',...
                                                 'ColCatX',Args.CatColNamesX,...
                                                 'ColCatY',Args.CatColNamesY,...
                                                 'ColRefX',RefColNameX,...
                                                 'ColRefY',RefColNameY);
                                                 
    % fit
    
end