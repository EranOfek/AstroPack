function Result = astrometryRefine(ObjAC, Args)
    % Refine an astrometric solution of an AstroCatalog object
    %   
    % Example: 
    
    
    arguments
        ObjAC AstroCatalog
        Args.Header         = []; % If given convert to AstroWCS
        Args.WCS            = []; % If given generate RA/Dec for sources
        
        Args.RA             = [];
        Args.Dec            = [];
        Args.CooUnits       = 'deg';
        Args.Scale          = [];
        
        Args.ProjType                     = 'TPV';
        Args.TranMethod                   = 'TPV';
        Args.Tran                         = Tran2D;
        Args.ErrPos                       = 0.01;
        Args.ExtraData          = [];
        Args.Niter              = 2;
        Args.FitMethod          = 'lscov';
        Args.MaxResid           = 0.5;
        Args.MagRange           = [13 19];
        Args.BinMethod          = 'bin';
        Args.PolyDeg            = 3;
        Args.BinSize            = 1;
        Args.FunMean            = @nanmedian;
        Args.FunStd             = @imUtil.background.rstd;
        Args.InterpMethod       = 'linear';
        Args.ThresholdSigma     = 3;
        
        Args.CatName                      = 'GAIAEDR3';  % or AstroCatalog
        Args.CatOrigin                    = 'catsHTM';
        Args.CatRadius                    = 1400;
        Args.CatRadiusUnits               = 'arcsec'
        Args.Con                          = {};
        
        Args.RefColNameMag                = {'Mag_BP','Mag'};
        Args.RefRangeMag                  = [12 19.5];
        Args.RefColNamePlx                = {'Plx'};
        Args.RefRangePlx                  = [-Inf 50];
        
        Args.EpochOut                     = [];
        Args.argsProperMotion cell        = {};
        
        Args.flagSrcWithNeighborsArgs cell      = {};
        Args.ReuseAstrometricCat(1,1) logical   = true;
                
        Args.RemoveNeighboors(1,1) logical      = true;
        
        Args.SearchRadius                       = 3;    
        
        Args.IncludeDistortions(1,1) logical    = true;
        
        Args.CreateNewObj(1,1) logical          = true;
        
        Args.CatColNamesX                   = AstroCatalog.DefNamesX;
        Args.CatColNamesY                   = AstroCatalog.DefNamesY;
        Args.CatColNamesMag                 = AstroCatalog.DefNamesMag;
        Args.RefColNamesRA                  = AstroCatalog.DefNamesRA;
        Args.RefColNamesDec                 = AstroCatalog.DefNamesDec;
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    % The name of the projected X/Y coordinates in the Reference astrometric catalog
    RefColNameX   = 'X';
    RefColNameY   = 'Y';
    CatColNameRA  = 'RA';
    CatColNameDec = 'Dec';
    
    if Args.CreateNewObj
        Obj = ObjAC.copyObject;
    else
        Obj = ObjAC;
    end
    
    
    % Case 1. AstroCatalog contains RA/Dec
    % Case 2. Use AstroWCS
    % Case 3. Use AstroHeader
    
    Nobj  = numel(Obj);
    Nhead = numel(Args.Header);
    Nwcs  = numel(Args.WCS);
    CooFromBoundingCircle = false;
    AstrometricCat        = [];
    for Iobj=1:1:Nobj
        % for each element in AstroCatalog
        
        % get X/Y columns from catalog
        [Xcat,~,IndCatX] = getColDic(Obj(Iobj), Args.CatColNamesX);
        [Ycat,~,IndCatY] = getColDic(Obj(Iobj), Args.CatColNamesY);

        if ~isempty(Args.Header)
            % convert AstroHeader to AstroWCS
            % populate Args.WCS
            Ihead = min(Iobj, Nhead);
            Args.WCS = AstroWCS.header2wcs(Args.Header(Ihead));
            Nwcs     = 1;
        end
        if isempty(Args.WCS)
            % assume RA/Dec are available in AstroCatalog
            [SrcRA, SrcDec] = getLonLat(Obj(Iobj), 'rad');
        else
            % Convert X/Y to RA/Dec using AstroWCS
            Iwcs = min(Iobj, Nwcs);
            [SrcRA, SrcDec] = Args.WCS(Iwcs).xy2sky(Xcat, Ycat, 'rad', Args.IncludeDistortions);
            % add approximate RA, Dec to new copy of catalog
            Obj = insertCol(Obj, [SrcRA, SrcDec], Inf, {CatColNameRA, CatColNameDec}, {'rad', 'rad'});
                
        end
    
        if CooFromBoundingCircle || isempty(Args.RA) || isempty(Args.Dec)
            if isempty(Args.WCS)
                % estimate RA/Dec of center of catalog from catalog itself
                CircleUnits         = 'deg';
                [Args.RA, Args.Dec, Args.CatRadius] = boundingCircle(Obj(Iobj),'CooType','pix','OutUnits',CircleUnits); 

                Args.CooUnits       = CircleUnits;
                Args.CatRadiusUnits = CircleUnits;
            else
                % estimate from image center and WCS
                error('not yet available');
                
            end
            CooFromBoundingCircle = true;
        else
            CooFromBoundingCircle = false;
        end
            
        if Args.ReuseAstrometricCat && ~isempty(AstrometricCat) 
            Args.CatName = AstrometricCat;
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
        if isempty(Args.Scale)
            % estimate scale based on distances between sources
            SrcDistRad = celestial.coo.sphere_dist_fast(SrcRA, SrcDec, SrcRA(1), SrcDec(1));
            SrcDistPix = sqrt((Xcat - Xcat(1)).^2 + (Ycat - Ycat(1)).^2);
            Scale = median(SrcDistRad.*RAD.*ARCSEC_DEG./SrcDistPix,'all','omitnan');
        else
            Scale = Args.Scale;
        end
        
        ProjectionScale = RAD .* ARCSEC_DEG ./ Scale;
    
        % projection
        ProjAstCat = imProc.trans.projection(AstrometricCat, RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                       'AddNewCols',{RefColNameX,RefColNameY},...
                                                                                       'CreateNewObj',true);
     
        % match the RA/Dec against an external catalog
        % sources in MatchedCat corresponds to sources in ProjAstCat
                
        [MatchedCat,UM,TUM] = imProc.match.match(Obj(Iobj), ProjAstCat,...
                                                     'Radius',Args.SearchRadius,...
                                                     'RadiusUnits','arcsec',...
                                                     'CooType','sphere',...
                                                     'AddIndInRef',false,...
                                                     'ColCatX',CatColNameRA,...
                                                     'ColCatY',CatColNameDec,...
                                                     'ColRefX',CatColNameRA,...
                                                     'ColRefY',CatColNameDec);
          
        % Debug: check that the matching is working
        % F=~isnan(MatchedCat.Catalog(:,1));
        % [ProjAstCat.Catalog(F,1:2), MatchedCat.Catalog(F,40:41)].*RAD
        
        
        % Count the number of matches
        Flag = ~isnan(MatchedCat.Catalog(:,1));
        Nmatches = sum(Flag);
                
        % filter Cat - remove sources with neighboors
        if Args.RemoveNeighboors
            UseFlag = ~imProc.match.flagSrcWithNeighbors(MatchedCat, Args.flagSrcWithNeighborsArgs{:}, 'CooType','sphere');
        else
            Nsrc    = sizeCatalog(AstrometricCat);
            UseFlag = true(Nsrc,1);
        end
        
                
        [Xcat,~,IndCatX] = getColDic(MatchedCat, Args.CatColNamesX);
        [Ycat,~,IndCatY] = getColDic(MatchedCat, Args.CatColNamesY);
        Xref = getColDic(ProjAstCat, RefColNameX);
        Yref = getColDic(ProjAstCat, RefColNameY);
        Mag  = getColDic(ProjAstCat, Args.RefColNameMag);
        
        
        % fit
        
        % why do we need ImageCenterXY ?  Args.WCS.CRPIX <<---?????????
        
        [Tran, ParWCS, ResFit] = imProc.astrometry.fitWCS(Xcat, Ycat, Xref, Yref, Mag, RAdeg, Decdeg,...
                                                       'ImageCenterXY',Args.WCS.CRPIX,...
                                                       'Scale',Scale,...
                                                       'ProjType',Args.ProjType,...
                                                       'TranMethod',Args.TranMethod,...
                                                       'Tran',Args.Tran,...
                                                       'UseFlag',UseFlag,...
                                                       'ExtraData',[],...
                                                       'ErrPos',Args.ErrPos,...
                                                       'Niter',Args.Niter,...
                                                       'FitMethod',Args.FitMethod,...
                                                       'MaxResid',Args.MaxResid,...
                                                       'MagRange',Args.MagRange,...
                                                       'BinMethod',Args.BinMethod,...
                                                       'PolyDeg',Args.PolyDeg,...
                                                       'BinSize',Args.BinSize,...
                                                       'FunMean',Args.FunMean,...
                                                       'FunStd',Args.FunStd,...
                                                       'InterpMethod',Args.InterpMethod,...
                                                       'ThresholdSigma',Args.ThresholdSigma);
        
        %
        Result(Iobj).ParWCS     = ParWCS;
        % store transformations
        Result(Iobj).Tran       = Tran;
        Result(Iobj).ResFit     = ResFit;
    end
               
                                                      
                

                
end