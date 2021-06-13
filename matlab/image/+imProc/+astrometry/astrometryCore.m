function Result = astrometryCore(Obj, Args)
    %
   
    arguments
        Obj(1,1) AstroCatalog
        Args.RA
        Args.Dec
        Args.CooUnits                = 'deg';
        Args.CatName                 = 'GAIAEDR3';
        Args.CatOrigin               = 'catsHTM';
        Args.CatRadius               = 1800;
        Args.CatRadiusUnits          = 'arcsec'
        Args.Scale
        Args.Rotation
        Args.
        
    end
    
    
    % Get astrometric catalog / incluidng proper motion
    Result = imProc.cat.getAstrometricCatalog(Args.RA, Args.Dec, );
    
    Args.CatName char             = 'GAIAEDR3';
        Args.CatOrigin                = 'catsHTM';
        Args.Radius                   = 1000;
        Args.RadiusUnits              = 'arcsec';
        Args.CooUnits                 = 'deg';
        Args.Shape
        Args.OutUnits                 = 'deg';
        Args.Con                      = {};
        Args.UseIndex(1,1) logical    = false;
        Args.EpochOut                 = [];  % if empty - don't apply proper motion
        Args.EpochIn                  = [];  % if given - don't use catalog Epoch
        Args.parsProperMotion cell    = {};
        
    % filter astrometric catalog
    
    % Project astrometric catalog
    
    % Match catalog to projected astrometric catalog
    
    % Fit transformation
    
    % Calculate WCS
    
    
end