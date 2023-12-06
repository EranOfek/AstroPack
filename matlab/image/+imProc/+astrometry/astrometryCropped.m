function [Result, CroppedAI, Summary] = astrometryCropped(Image, Args)
    % Execute astrometry on a section of a full image.
    % Input  : - An Image name, or AstroImage, or a matrix.
    %          * ...,key,val,...
    %            See code
    % Output : - Structure array of fitting summary.
    %          - Cropped AstroImage with WCS.
    %          - A structure array of summary info regarding astrometry.
    %            .Rotation  - deg
    %            .Scale     - arcsec/pix
    %            .CenterRA  - deg
    %            .CenterDec - deg
    %            .Ngood     -
    %            .AssymRMS  - arcsec
    % Author : Eran Ofek (Dec 2021)
    % Example: File = 'LAST.2.1.2_20200820.171228.648_clear_0_science.fits'
    %          [R, CAI, S] = imProc.astrometry.astrometryCropped(File);
    
    arguments
        Image
        
        Args.CropSize             = []; %[1000 1000];
        Args.CropType             = 'center';
        Args.SameField logical    = false;
        
        Args.RA                           = 'RA';
        Args.Dec                          = 'DEC';
        Args.CooUnits                     = 'deg';
        Args.CatName                      = 'GAIADR3';  % or AstroCatalog
        Args.CatRadius                    = 3600.*2.5;
        Args.CatRadiusUnits               = 'arcsec'
        Args.Con                          = {};
        Args.RefRangeMag                  = [8 14]; %.5];
        Args.Scale                        = 1.25;      % range or value [arcsec/pix]
        Args.RotationRange(1,2)           = [-90, 90];
        Args.RotationStep(1,1)            = 0.2;
        Args.DistEdges                    = (12:3:300).';   % 12:3:300
        Args.HistDistEdgesRotScale        = [10 600 300];
        
        Args.RangeX(1,2)                  = [-1000 1000].*4.5;
        Args.RangeY(1,2)                  = [-1000 1000].*4.5;
        Args.StepX(1,1)                   = 2;
        Args.StepY(1,1)                   = 2;
        Args.Flip(:,2)                    = [1 1; 1 -1;-1 1;-1 -1]; % [1 -1]
        Args.SearchRadius(1,1)            = 6;   
        Args.FilterSigma                  = 3;
        Args.astrometryCoreArgs cell      = {};
        Args.Tran                         = Tran2D('poly1');     
        Args.TranMethod char              = 'TPV';
        
        Args.backgroundArgs cell          = {'SubSizeXY',[]};
        Args.Threshold                    = 20;
        Args.findMeasureSourcesArgs cell  = {'PsfFunPar',{[0.1; 1.5]}};
          
    end
    ARCSEC_DEG = 3600;
    
    if isnumeric(Image)
        Image = {Image};
    elseif ischar(Image)
        Image = {Image};
    else
        % do nothing
    end
    
    % create an AstroImage array with the images
    AI = AstroImage(Image);
    AI.cast('single');
    
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
               'Tran',Args.Tran,...
               'TranMethod',Args.TranMethod,...
               Args.astrometryCoreArgs{:}};
    
    
    Nim = numel(AI);
    for Iim=1:1:Nim
        % for each image
        
        % get cropped image
        CroppedAI(Iim) = AI(Iim).crop(Args.CropSize, 'Type',Args.CropType, 'CreateNewObj',true);
        CCDSEC = CroppedAI(Iim).ImageData.CCDSEC;
        
        % measure background
        CroppedAI(Iim) = imProc.background.background(CroppedAI(Iim), Args.backgroundArgs{:});
        % find sources
        CroppedAI(Iim) = imProc.sources.findMeasureSources(CroppedAI(Iim), 'Threshold',Args.Threshold, Args.findMeasureSourcesArgs{:});
        [Ysize, Xsize] = sizeImage(CroppedAI(Iim));
        Xcenter = Xsize.*0.5;
        Ycenter = Ysize.*0.5;
        
        if Iim==1 || ~Args.SameField
            [Result(Iim), CroppedAI(Iim), AstrometricCat] = imProc.astrometry.astrometryCore(CroppedAI(Iim), FunArgs{:});
        else
            [Result(Iim), CroppedAI(Iim)] = imProc.astrometry.astrometryCore(CroppedAI(Iim), FunArgs{:}, 'CatName',AstrometricCat);
        end
        
        %AI = CroppedAI;
        
        % delete distortions from header
        %AI(Iim).HeaderData.deleteDistortionsWCS;
        
        % update WCS in full image
        %AI(Iim).WCS = cropWCS(CroppedAI.WCS, -CCDSEC([1 3]), 'delDistortion',true);
        %AI(Iim).HeaderData = wcs2header(AI(Iim).WCS, AI(Iim).HeaderData);
   
        Summary(Iim).Rotation = atan2d(Result(Iim).WCS.CD(1,1), Result(Iim).WCS.CD(1,2));
        Summary(Iim).Scale    = sqrt(Result(Iim).WCS.CD(1,1).^2 + Result(Iim).WCS.CD(1,2).^2).*ARCSEC_DEG;   % arcsec
        [Summary(Iim).CenterRA, Summary(Iim).CenterDec] = CroppedAI(Iim).WCS.xy2sky(Xcenter, Ycenter);
        Summary(Iim).Ngood = Result(Iim).ResFit(Result(Iim).BestInd).Ngood;
        Summary(Iim).AssymRMS = Result(Iim).ResFit(Result(Iim).BestInd).AssymRMS.*ARCSEC_DEG;   % arcsec
        
    end
    
    
end