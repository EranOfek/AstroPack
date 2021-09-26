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
    
    arguments
        File                   % FileName+path / AstroImage
        Args.Dir                              = '';
        Args.Dark                             = []; % [] - do nothing
        Args.Flat                             = []; % [] - do nothing
        Args.Fringe                           = []; % [] - do nothing
        Args.BlockSize                        = [];  % empty - full image
        
        Args.MaskSaturated(1,1) logical       = true;
        Args.InterpOverSaturated(1,1) logical = true;
        Args.DoAstrometry(1,1) logical        = true;
        Args.DoPhotometry(1,1) logical        = true;
        Args.MatchExternal(1,1) logical       = false;
        Args.SaveProducts(1,1) logical        = true;
        
        Args.maskSaturatedArgs cell           = {};
        Args.debiasArgs cell                  = {};
        Args.overscanArgs cell                = {};
        Args.deflatArgs cell                  = {};
        Args.image2subimagesArgs cell         = {};
        Args.backgroundArgs cell              = {};
        Args.interpOverNanArgs cell           = {};
        Args.findMeasureSourcesArgs cell      = {};
        Args.astrometrySubImagesArgs cell     = {};
        Args.addCoordinates2catalogArgs cell  = {'OutUnits','deg'};
        Args.CreateNewObj                     = [];
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
    [AI, CreateNewObj] = AI.createNewObj(Args.CreateNewObj, nargout, 0);
    
    % Mask Saturated - mask saturated and non-lin pixels
    if Args.MaskSaturated
        [AI] = imProc.mask.maskSaturated(AI, Args.maskSaturatedArgs{:},...
                                             'CreateNewObj',false);
    end
    
    % Subtract Dark
    if ~isempty(Args.Dark)
        AI = imProc.dark.debias(AI, Args.Dark, Args.debiasArgs{:},...
                                               'CreateNewObj',false);
    end
    
    % Subtract overscan & trim
    [AI] = imProc.dark.overscan(AI, Args.overscanArgs{:},...
                                    'CreateNewObj',false);
    
    % Divide by Flat
    if ~isempty(Args.Flat)
        AI = imProc.flat.deflat(AI, Args.Flat, Args.deflatArgs{:},...
                                               'CreateNewObj',false);
    end
    
    % Fringing
    if ~isempty(Args.Fringe)
        % FFU
        error('Fringe removal is not implemented yet');
    end
    
    % Sub Images - divide the image to multiple sub images
    SI = imProc.image.image2subimages(AI, Args.BlockSize, Args.image2subimagesArgs{:});
    clear AI;
    
    % Background 
    SI = imProc.background.background(SI, Args.backgroundArgs{:});
    
    if Args.InterpOverSaturated
        % Motivation: Saturated pixels may cause problems and it is better
        % to interpolate over such pixels (i.e., it may remove some of the
        % theta functions that may be problematoc for convolution)
        
        % Replac saturated pixels by NaN
        SI = imProc.mask.replaceMaskedPixVal(SI,  {'Saturated','NonLin'}, NaN, 'Method','any', 'CreateNewObj',false);

        % Interpolate over NaN
        SI = interpOverNan(SI, Args.interpOverNanArgs{:},...
                               'CreateNewObj',false);
    end
    
    % Source finding
    SI = imProc.sources.findMeasureSources(SI, Args.findMeasureSourcesArgs{:},...
                                               'CreateNewObj',false);
    
    % Astrometry
    if Args.DoAstrometry
        [SI, Result.AstrometricFit, AstrometricCat] = imProc.astrometry.astrometrySubImages(SI, Args.astrometrySubImagesArgs{:},...
                                                                                        'CreateNewObj',false);
    
        % Update Cat astrometry
        SI = imProc.astrometry.addCoordinates2catalog(SI, Args.addCoordinates2catalogArgs{:});
    end
    
    % Photometric ZP
    if Args.DoPhotometry
        % [Result, ZP, PhotCat] = imProc.calib.photometricZP(Obj, Args)
    
        % Update Cat photometry
    end
    
    % match against external catalogs
    if Args.MatchExternal
        % 1. Add columns for matched sources
        
        % 2. generate a new catalog of only matched sources
        
    end
    
    % Save products
    if Args.SaveProducts
        
    end
    
    
end
