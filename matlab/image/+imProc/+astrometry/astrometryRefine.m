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
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    % The name of the projected X/Y coordinates in the Reference astrometric catalog
    RefColNameX = 'X';
    RefColNameY = 'Y';
    
    
    % Case 1. AstroCatalog contains RA/Dec
    % Case 2. Use AstroWCS
    % Case 3. Use AstroHeader
    
    Nobj  = numel(Obj);
    Nhead = numel(Args.Header);
    Nwcs  = numel(Args.WCS);
    for Iobj=1:1:Nobj
        % for each element in AstroCatalog
        
        % get X/Y columns from catalog
        [Xcat,~,IndCatX] = getColDic(Obj, Args.CatColNamesX);
        [Ycat,~,IndCatY] = getColDic(Obj, Args.CatColNamesY);

        if ~isempty(Args.Header)
            % convert AstroHeader to AstroWCS
            % populate Args.WCS
            Ihead = min(Iobj, Nhead);
            Args.WCS = AstroWCS.header2wcs(Args.Header(Ihead)   );
            Nwcs     = 1;
        end
        if ~isempty(Args.WCS)
            % Convert X/Y to RA/Dec using AstroWCS
            Iwcs = min(Iobj, Nwcs);
            [SrcRA, SrcDec] = Args.WCS(Iwcs).xy2sky(Xcat, Ycat, 'rad');
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
            Scale = 
        else
            Scale = Args.Scale;
        end
        
        ProjectionScale = (180./pi) .* 3600 ./ mean(Scale);
    
        % projection
        ProjAstCat = imProc.trans.projection(AstrometricCat, RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                       'AddNewCols',{RefColNameX,RefColNameY},...
                                                                                       'CreateNewObj',true);
        % filter Cat - remove sources with neighboors
        [Flag, Obj] = imProc.match.flagSrcWithNeighbors(Obj, Args)
        
        
        
        % match the RA/Dec against an external catalog
        [MatchedCat,UM,TUM] = imProc.match.match(FilteredCat, TransformedProjAstCat,...
                                                     'Radius',Args.SearchRadius,...
                                                     'CooType','pix',...
                                                     'ColCatX',Args.CatColNamesX,...
                                                     'ColCatY',Args.CatColNamesY,...
                                                     'ColRefX',RefColNameX,...
                                                     'ColRefY',RefColNameY);
                                                 
        % fit
        [Tran, ParWCS, ResFit] = imProc.astrometry.fitAstrometry(Xcat, Ycat, Xref, Yref, Mag, RAdeg, Decdeg,...
                                                       'ImageCenterXY',Result(Iobj).ImageCenterXY,...
                                                       'Scale',ResPattern.Sol.Scale(Isol),...
                                                       'ProjType',Args.ProjType,...
                                                       'TranMethod',Args.TranMethod,...
                                                       'Tran',Args.Tran,...
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
        Result(Iobj).ParWCS(Isol) = ParWCS;
        % store transformations
        Result(Iobj).Tran(Isol)       = Tran;
        Result(Iobj).ResFit(Isol)     = ResFit;
    end
               
                                                      
                

                
end