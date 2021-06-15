function Result = astrometryCore(Obj, Args)
    %
   
    arguments
        Obj(1,1) AstroCatalog
        Args.RA
        Args.Dec
        Args.CooUnits                     = 'deg';
        Args.CatName                      = 'GAIAEDR3';
        Args.CatOrigin                    = 'catsHTM';
        Args.CatRadius                    = 1800;
        Args.CatRadiusUnits               = 'arcsec'
        Args.Con                          = {};
        
        Args.MagColName                   = {'Mag_BP','Mag'};
        Args.MagRange                     = [12 19];
        Args.PlxColName                   = {'Plx'};
        Args.PlxRange                     = [-Inf 10];
        
        Args.EpochOut                     = [];
        Args.argsProperMotion cell        = {};
        Args.argsFilterForAstrometry cell = {};
        Args.argsFitPattern cell          = {};
        
        Args.ProjType                     = 'TAN';
        
        Args.Scale
        Args.RotationRange(1,2)           = [-90, 90];
        Args.RotationStep(1,1)            = 0.2;
        
        Args.RangeX(1,2)                  = [-1000 1000]; 
        Args.RangeY(1,2)                  = [-1000 1000]; 
        Args.StepX(1,1)                   = 4;
        Args.StepY(1,1)                   = 4;
        Args.Flip(:,2)                    = [1 1; 1 -1;-1 1;-1 -1];
        Args.SearchRadius(1,1)            = 4;
        
        Args.
        
    end
    
    RotationEdges = (Args.RotationRange(1):Args.RotationStep:Args.RotationRange(2));
    
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
                                                                         'argsProperMotion',Args.argsProperMotion{:});
                                                                     
    % Addtitional constraints on astrometric catalog
    AstrometricCat = queryRange(AstrometricCat, Args.MagColName, Args.MagRange,...
                                                Args.PlxColName, Args.PlxRange);
   
    % Project astrometric catalog
    ProjAstCat = imProc.trans.projection(AstrometricCat, RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                                  'AddNewCols',{'X','Y'},...
                                                                                                  'CreateNewObj',false);
                                                                                             
    % filter astrometric catalog
    [FilteredCat, FilteredProjAstCat]=imProc.cat.filterForAstrometry(Obj, ProjAstCat, Args.argsFilterForAstrometry{:});
    
    % Match pattern catalog to projected astrometric catalog
    ResPattern = imProc.trans.fitPattern(FilteredCat, FilteredProjAstCat, Args.argsFitPattern{:},...
                                                                      'Scale',Args.Scale,...
                                                                      'HistRotEdges',RotationEdges,...
                                                                      'RangeX',Args.RangeX,...
                                                                      'RangeY',Args.RangeY,...
                                                                      'StepX',Args.StepX,...
                                                                      'StepY',Args.StepY,...
                                                                      'Flip',Args.Flip,...
                                                                      'SearchRadius',Args.SearchRadius);
                                                                      
        
    % Fit transformation
    
    
    
    % Calculate WCS
    
    
end