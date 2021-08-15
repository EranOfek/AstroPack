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
    % Example: 
    
    arguments
        File                   % FileName+path / AstroImage
        Args.Dir                            = '';
        Args.Dark                           = []; % [] - do nothing
        Args.Flat                           = []; % [] - do nothing
        Args.Fringe                         = []; % [] - do nothing
        Args.BlockSize                      = [];  % empty - full image
        
        Args.maskSaturatedArgs cell         = {};
        Args.debiasArgs cell                = {};
        Args.deflatArgs cell                = {};
        Args.image2subimagesArgs cell       = {};
        Args.backgroundArgs cell            = {};
        Args.findMeasureSourcesArgs cell    = {};
        Args.astrometrySubImagesArgs cell   = {};
        Args.CreateNewObj                   = [];
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
            
    % Mask Saturated
    AI = imProc.astrometry.maskSaturated(AI, Args.maskSaturatedArgs{:},...
                                             'CreateNewObj',false);
    
    % Subtract Dark
    if ~isempty(Args.Dark)
        AI = imProc.dark.debias(AI, Args.Dark, Args.debiasArgs{:},...
                                               'CreateNewObj',false);
    end
    
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
    
    % Sub Images
    SI = imProc.image.image2subimages(AI, Args.BlockSize, Args.image2subimagesArgs{:});
    clear AI;
    
    % Background 
    SI = imProc.background.background(SI, Args.backgroundArgs{:});
    
    % Source finding
    SI = imProc.sources.findMeasureSources(SI, Args.findMeasureSourcesArgs{:},...
                                               'CreateNewObj',false);
    
    % Astrometry
    [SI, Result.AstrometricFit, AstrometricCat] = imProc.astrometry.astrometrySubImages(SI, Args.astrometrySubImagesArgs{:},...
                                                                                        'CreateNewObj',false);
    
    % Update Cat astrometry
    
    % Photometric ZP
    
    % Update Cat photometry
    
    % Save products
    
    
    
end
