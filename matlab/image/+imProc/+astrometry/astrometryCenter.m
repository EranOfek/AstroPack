function [Result, AI] = astrometryCenter(Image, Args)
    % 
    % Example: File = 'LAST.2.1.2_20200820.171228.648_clear_0_science.fits'
    %          imProc.astrometry.astrometryCenter(File);
    
    arguments
        Image
        
        Args.CropSize             = [1000 1000];
        Args.CropType             = 'center';
        Args.SameField logical    = false;
        
        Args.RA                           = 'RA';
        Args.Dec                          = 'DEC';
        Args.CooUnits                     = 'deg';
        Args.CatName                      = 'GAIAEDR3';  % or AstroCatalog
        Args.CatRadius                    = 1400.*2;
        Args.CatRadiusUnits               = 'arcsec'
        Args.Con                          = {};
        Args.RefRangeMag                  = [12 19]; %.5];
        Args.Scale                        = 1.0;      % range or value [arcsec/pix]
        Args.RotationRange(1,2)           = [-90, 90];
        Args.RotationStep(1,1)            = 0.2;
        Args.DistEdges                    = (12:3:300).';   % 12:3:300
        Args.HistDistEdgesRotScale        = [10 600 300];
        
        Args.RangeX(1,2)                  = [-1000 1000].*2;
        Args.RangeY(1,2)                  = [-1000 1000].*2;
        Args.StepX(1,1)                   = 2;
        Args.StepY(1,1)                   = 2;
        Args.Flip(:,2)                    = [1 1; 1 -1;-1 1;-1 -1]; % [1 -1]
        Args.SearchRadius(1,1)            = 6;   
        Args.FilterSigma                  = 3;
        Args.astrometryCoreArgs cell      = {};
    end
   
    if isnumeric(Image)
        Image = {Image};
    end
    
    % create an AstroImage array with the images
    AI = AstroImage(Image);
    
    % astrometryCore arguments:
    FunArgs = {'RA',Args.RA,...
               'Dec',Args.Dec,...
               'CooUnits',Args.CooUnits,...
               'Scale',Args.Scale,...
               'CatName',Args.CatName,...
               'CatRadius',Args.CatRadius,...
               'CatRadiusUnits',Args.CatRadiusUnits,...
               'Con',Args.Con,...
               'RefRangeMag',Args.RefRangeMag,...
               'RotationRange',Args.RotationRange,...
               'RotationStep',Args.RotationStep,...
               'DistEdges',Args.DistEdges,...
               'HistDistEdgesRotScale',Args.HistDistEdgesRotScale,...
               'RangeX',Args.RangeX,...
               'RangeY',Args.RangeY,...
               'StepX',Args.StepX,...
               'StepY',Args.StepY,...
               'Flip',Args.Flip,...
               'SearchRadius',Args.SearchRadius,...
               'FilterSigma',Args.FilterSigma,...
               Args.astrometryCoreArgs{:}};
    
    
    Nim = numel(AI);
    for Iim=1:1:Nim
        % for each image
        
        % get cropped image
        CroppedAI = AI(Iim).crop(Args.CropSize, 'Type',Args.CropType, 'CreateNewObj',true);
        
        if Iim==1 || ~Args.SameField
            [Result, CroppedAI, AstrometricCat] = astrometryCore(CroppedAI, FunArgs{:});
        else
            [Result, CroppedAI] = astrometryCore(CroppedAI, FunArgs{:}, 'CatName',AstrometricCat);
        end
        
   
    end
    
    
end