function Result = astrometryCore(Obj, Args)
    %
    % Example: 
   
    arguments
        Obj(1,1) AstroCatalog
        Args.RA
        Args.Dec
        Args.CooUnits                     = 'deg';
        Args.CatName                      = 'GAIAEDR3';  % or AstroCatalog
        Args.CatOrigin                    = 'catsHTM';
        Args.CatRadius                    = 1400;
        Args.CatRadiusUnits               = 'arcsec'
        Args.Con                          = {};
        
        Args.RefColNameMag                = {'Mag_BP','Mag'};
        Args.RefRangeMag                  = [12 19];
        Args.RefColNamePlx                = {'Plx'};
        Args.RefRangePlx                  = [-Inf 10];
        
        Args.EpochOut                     = [];
        Args.argsProperMotion cell        = {};
        Args.argsFilterForAstrometry cell = {};
        Args.argsFitPattern cell          = {};
        
        Args.ProjType                     = 'TAN';
        
        Args.Scale                        = 1.0;      % range or value [arcsec/pix]
        Args.RotationRange(1,2)           = [-90, 90];
        Args.RotationStep(1,1)            = 0.2;
        
        Args.RangeX(1,2)                  = [-1000 1000]; 
        Args.RangeY(1,2)                  = [-1000 1000]; 
        Args.StepX(1,1)                   = 4;
        Args.StepY(1,1)                   = 4;
        Args.Flip(:,2)                    = [1 -1]; %[1 1; 1 -1;-1 1;-1 -1];
        Args.SearchRadius(1,1)            = 5;
        
        Args.Tran                         = Tran2D;
        
        Args.CatColNamesX                 = AstroCatalog.DefNamesX;
        Args.CatColNamesY                 = AstroCatalog.DefNamesY;
        Args.CatColNamesMag               = AstroCatalog.DefNamesMag;
        Args.RefColNamesRA                = AstroCatalog.DefNamesRA;
        Args.RefColNamesDec               = AstroCatalog.DefNamesDec;
        
        
    end
    RefColNameX = 'X';
    RefColNameY = 'Y';
    
    RotationEdges = (Args.RotationRange(1):Args.RotationStep:Args.RotationRange(2));
    % mean value of projection scale:
    ProjectionScale = (180./pi) .* 3600 ./ mean(Args.Scale);
    
    % Get astrometric catalog / incluidng proper motion
    % RA and Dec output are in radians
    [AstrometricCat, RA, Dec] = imProc.cat.getAstrometricCatalog(Args.RA, Args.Dec, 'CatName',Args.CatName,...
                                                                                    'CatOrigin',Args.CatOrigin,...
                                                                                    'Radius',Args.CatRadius,...
                                                                                    'RadiusUnits',Args.CatRadiusUnits,...
                                                                                    'CooUnits',Args.CooUnits,...
                                                                                    'OutUnits','rad',...
                                                                                    'Con',Args.Con,...
                                                                                    'EpochOut',Args.EpochOut,...
                                                                                    'argsProperMotion',Args.argsProperMotion);
        
    % Addtitional constraints on astrometric catalog
    % mag and parallax constraints
    AstrometricCat = queryRange(AstrometricCat, Args.RefColNameMag, Args.RefRangeMag,...
                                                Args.RefColNamePlx, Args.RefRangePlx);
   
    % Project astrometric catalog
    ProjAstCat = imProc.trans.projection(AstrometricCat, RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                   'AddNewCols',{RefColNameX,RefColNameY},...
                                                                                   'CreateNewObj',false);
    % ProjAstCat.plot({'X','Y'},'.')   
    
    % filter astrometric catalog
    % FFU: need to add column names...
    [FilteredCat, FilteredProjAstCat, Summary] = imProc.cat.filterForAstrometry(Obj, ProjAstCat,...
                                                                                'ColCatX',Args.CatColNamesX,...
                                                                                'ColCatY',Args.CatColNamesY,...
                                                                                'ColCatMag',Args.CatColNamesMag,...
                                                                                'ColRefX',RefColNameX,...
                                                                                'ColRefY',RefColNameY,...
                                                                                'ColRefMag',Args.RefColNameMag,...
                                                                                Args.argsFilterForAstrometry{:});
    
    
    % The Ref catalog is projected around some center that should coincide
    % with the center of Cat.
    % Therefore, we should shift Cat to its own center
    imProc.trans.tranAffine(FilteredCat,[-500 -500],true); %[-1024 -2048],true);
    
   
    
    
    % Match pattern catalog to projected astrometric catalog
    % FFU: CatColNamesX/Y are for both Cat and Ref!!
    [ResPattern] = imProc.trans.fitPattern(FilteredCat, FilteredProjAstCat, Args.argsFitPattern{:},...
                                                                      'Scale',Args.Scale,...
                                                                      'HistRotEdges',RotationEdges,...
                                                                      'RangeX',Args.RangeX,...
                                                                      'RangeY',Args.RangeY,...
                                                                      'StepX',Args.StepX,...
                                                                      'StepY',Args.StepY,...
                                                                      'Flip',Args.Flip,...
                                                                      'SearchRadius',Args.SearchRadius,...
                                                                      'ColNamesX',Args.CatColNamesX,...
                                                                      'ColNamesY',Args.CatColNamesY);
    
    % go over possible solutions:
    Nsolutions = numel(ResPattern.Sol.SN);   % number of candidate solutions
          
    % Apply affine transformation to Reference
    TransformedProjAstCat = imProc.trans.tranAffine(FilteredProjAstCat, ResPattern.Sol.AffineTran{1}, true,...
                                                    'ColX',RefColNameX,...
                                                    'ColY',RefColNameY);

    % match sources based on X/Y positions:
    [MatchedCat,UM,TUM] = imProc.match.match(FilteredCat, TransformedProjAstCat,...
                                     'Radius',Args.SearchRadius,...
                                     'CooType','pix',...
                                     'ColCatX',Args.CatColNamesX,...
                                     'ColCatY',Args.CatColNamesY,...
                                     'ColRefX',RefColNameX,...
                                     'ColRefY',RefColNameY);
    % Count the number of matches
    Flag = ~isnan(MatchedCat.Catalog(:,1));
    Nmatches = sum(Flag);
    
    % DEBUGING
    % show table of [X, Y, RA, Dec] of matched sources
    
    
    % Note that XPEAK/YPEAK and X/Y are different because of the shift
    % applied (shift coo to image center)
    
    XY = getCol(MatchedCat,{'XPEAK','YPEAK','X','Y'});   % X/Y in image
    XY = XY(Flag,:);
    RF = celestial.coo.convertdms(TransformedProjAstCat.Catalog(Flag,1),'r','SH');
    DF = celestial.coo.convertdms(TransformedProjAstCat.Catalog(Flag,2),'r','SD');
    T  = table(XY(:,1),XY(:,2), XY(:,3), XY(:,4), RF, DF)
    
    % Fit transformation
    %[Param, Res] = imProc.trans.fitTransformation(TransformedCat, FilteredProjAstCat, 'Tran',Args.Tran);
    % MatchedRef has the same number of lines as in Ref,
    % but it is affine transformed to the coordinate system of Cat
    
    % note that the matching is against the FilteredProjAstCat
    % and not the TransformedProjAstCat 
    [Param, Res, Tran] = imProc.trans.fitTransformation(MatchedCat, FilteredProjAstCat,...
                                                  'Tran',Args.Tran,...
                                                  'Norm',NaN,...
                                                  'ColCatX',Args.CatColNamesX,...
                                                  'ColCatY',Args.CatColNamesY,...
                                                  'ColRefX',RefColNameX,...
                                                  'ColRefY',RefColNameY);
    
    
    % Generate WCS
    Result = [];
    
end