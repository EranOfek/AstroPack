function [SI, AstrometricCat, Result]=singleRaw2proc(File, Args)
    % Basic processing of a single raw image into a processed image
    %   Including:
    %       Reading the image
    %       Generate a mask image and mask saturated pixels
    %       Subtract bias/dark image
    %       Divide by flat image
    %       Remove Fringe image
    %       Break image to sub images
    %       Estimate background
    %       Basic source findinging
    %       Astrometry
    %       Update astrometry in catalog
    %       Photometric ZP
    %       Update photometric ZP in catalog
    %       Save products
    % Input  : -
    % Output : -
    % Author : Eran Ofek (Aug 2021)
    % Example: pipeline.generic.singleRaw2proc
    %          % generate CalibImages using example in CalibImages/unitTest
    %          File = 'LAST.2.1.2_20200821.020230.952_clear_0_science.fits';
    %          [SI, AstrometricCat,Result]=pipeline.generic.singleRaw2proc(File,'CalibImages',CI);
    
    arguments
        File                   % FileName+path / AstroImage
        Args.Dir                              = '';
        Args.CalibImages CalibImages          = [];
        Args.Dark                             = []; % [] - do nothing
        Args.Flat                             = []; % [] - do nothing
        Args.Fringe                           = []; % [] - do nothing
        Args.BlockSize                        = [1600 1600];  % empty - full image
        
        Args.MultiplyByGain logical           = true; % after fringe correction
        Args.MaskSaturated(1,1) logical       = true;
        Args.InterpOverSaturated(1,1) logical = true;
        Args.DoAstrometry(1,1) logical        = true;
        Args.DoPhotometry(1,1) logical        = true;
        Args.MatchExternal(1,1) logical       = false;
        Args.SaveProducts(1,1) logical        = true;
        
        Args.maskSaturatedArgs cell           = {};
        Args.debiasArgs cell                  = {};
        Args.SubtractOverscan logical         = false;
        Args.MethodOverScan                   = 'globalmedian';
        Args.deflatArgs cell                  = {};
        Args.CorrectFringing logical          = false;
        Args.image2subimagesArgs cell         = {};
        Args.OverlapXY                        = [64 64];
        Args.backgroundArgs cell              = {};
        Args.BackSubSizeXY                    = [128 128];
        Args.interpOverNanArgs cell           = {};
        Args.findMeasureSourcesArgs cell      = {};
        Args.astrometrySubImagesArgs cell     = {};
        Args.CatName                          = 'GAIAEDR3';  % or AstroCatalog
        Args.addCoordinates2catalogArgs cell  = {'OutUnits','deg'};
        Args.CreateNewObj logical             = false;
    end
    
    % Get Image
    if ischar(File)
        if ~isempty(Args.Dir)
            File = sprintf('%s%s%s',Args.Dir, filesep, File);
        end
        
        AI = AstroImage(File);
    elseif isa(File, 'AstroImage')
        AI = File;
    else
        error('Unsupported File type option');
    end
    
    % createNewObj
    if Args.CreateNewObj
        AI = AI.copy;
    end
    
    % set CalibImages
    if isempty(Args.CalibImages)
        CI = CalibImages;
        CI.Dark   = Args.Dark;
        CI.Flat   = Args.Flat;
        CI.Fringe = Args.Fringe;
    else
        CI = Args.CalibImages;
    end
        
    
    AI.setKeyVal('FILTER','clear');
    AI.setKeyVal('SATURVAL',55000);
    
    AI = CI.processImages(AI, 'SubtractOverscan',false, 'InterpolateOverSaturated',true,...
                              'MaskSaturated',Args.MaskSaturated,...
                              'maskSaturatedArgs',Args.maskSaturatedArgs,...
                              'debiasArgs',Args.debiasArgs,...
                              'SubtractOverscan',Args.SubtractOverscan,...
                              'MethodOverScan',Args.MethodOverScan,...
                              'deflatArgs',Args.deflatArgs,...
                              'CorrectFringing',Args.CorrectFringing,...
                              'MultiplyByGain',Args.MultiplyByGain);
                              
   % crop overscan
   AI.crop([1 6354 1 9600]);
   
    
%     % Mask Saturated - mask saturated and non-lin pixels
%     if Args.MaskSaturated
%         [AI] = imProc.mask.maskSaturated(AI, Args.maskSaturatedArgs{:},...
%                                              'CreateNewObj',false);
%     end
    
%     % Subtract Dark
%     if ~isempty(Args.Dark)
%         AI = imProc.dark.debias(AI, Args.Dark, Args.debiasArgs{:},...
%                                                'CreateNewObj',false);
%     end
    
%     % Subtract overscan & trim
%     [AI] = imProc.dark.overscan(AI, Args.overscanArgs{:},...
%                                     'CreateNewObj',false);
%     
%     % Divide by Flat
%     if ~isempty(Args.Flat)
%         AI = imProc.flat.deflat(AI, Args.Flat, Args.deflatArgs{:},...
%                                                'CreateNewObj',false);
%     end
%     
%     % Fringing
%     if ~isempty(Args.Fringe)
%         % FFU
%         error('Fringe removal is not implemented yet');
%     end
    
    % Sub Images - divide the image to multiple sub images
    % Set UpdatCat to false, since in this stage there is no catalog
    [SI, InfoCCDSEC] = imProc.image.image2subimages(AI, Args.BlockSize, 'UpdateCat',false, Args.image2subimagesArgs{:}, 'OverlapXY',Args.OverlapXY);
    clear AI;
    
    % Background 
    SI = imProc.background.background(SI, Args.backgroundArgs{:}, 'SubSizeXY',Args.BackSubSizeXY);
    
%     if Args.InterpOverSaturated
%         % Motivation: Saturated pixels may cause problems and it is better
%         % to interpolate over such pixels (i.e., it may remove some of the
%         % theta functions that may be problematoc for convolution)
%         
%         % Replac saturated pixels by NaN
%         SI = imProc.mask.replaceMaskedPixVal(SI,  {'Saturated','NonLin'}, NaN, 'Method','any', 'CreateNewObj',false);
% 
%         % Interpolate over NaN
%         SI = interpOverNan(SI, Args.interpOverNanArgs{:},...
%                                'CreateNewObj',false);
%     end
    
    % Source finding
    SI = imProc.sources.findMeasureSources(SI, Args.findMeasureSourcesArgs{:},...
                                               'CreateNewObj',false);
    
    % Astrometry
    if Args.DoAstrometry
        Tran = Tran2D;
        [Result.AstrometricFit, SI, AstrometricCat] = imProc.astrometry.astrometrySubImages(SI, Args.astrometrySubImagesArgs{:},...
                                                                                        'Scale',1.25,...
                                                                                        'CatName',Args.CatName,...
                                                                                        'CCDSEC', InfoCCDSEC.EdgesCCDSEC,...
                                                                                        'Tran',Tran,...
                                                                                        'CreateNewObj',false);
                                                                                    
    
        % Update Cat astrometry
        SI = imProc.astrometry.addCoordinates2catalog(SI, Args.addCoordinates2catalogArgs{:},'UpdateCoo',true);
    end
    
    % Photometric ZP
    if Args.DoPhotometry
        % [Result, ZP, PhotCat] = imProc.calib.photometricZP(Obj, Args)
    
        % Update Cat photometry
    end
    
    % match known solar system objects
    %[SourcesWhichAreMP, Obj] = match2solarSystem(Obj, Args)
    
    % match against external catalogs
    if Args.MatchExternal
        % 1. Add columns for matched sources
        
        % 2. generate a new catalog of only matched sources
        
    end
    
    % Save products
    if Args.SaveProducts
        
    end
    
    
end
