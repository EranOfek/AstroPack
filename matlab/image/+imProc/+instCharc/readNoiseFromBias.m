function Result = readNoiseFromBias(Obj, Args)
    % Estimate the read noise from bias mage/s in native units.
    %   The read noise is estimated by calculating the rms in small blocks (sub
    %   images) of a bias image, or by calculating the rms per pixel (in
    %   multiple images).
    % Input  : - An AstroImage object with bias images.
    %          * ...,key,val,...
    %            'Method'   - One of the following estimation methods:
    %                   ['block'] - Estimate the RN based on a single image by
    %                       calculating the rms in each blck (sub image).
    %                   'pix' - Estimate the RN based on the rms of multiple
    %                       images in each pixel. The result is RN per pixel
    %                       image.
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of the image
    %                   section on which to estimate the read noise.
    %            Parameters for the 'block' method:
    %            'BlockSize' - Block size in which to calc readnoise.
    %                   Default is [64 64].
    %            'OverlapXY' - Overlap in sub images. Default is [0 0].
    %            'BlockStdFun' - Function handle for calculating the std in
    %                   the blck. Default is @imProc.stat.rstd.
    %            'BlockMeanFun' - Function handle for calculating the global
    %                   readnoise from the blocks readnoise.
    %                   Default is @tools.math.stat.nanmedian.
    %            'BlockMeanStdFun' - Function handle for calculating the rms
    %                   of the global readnoise.
    %                   Default is @imUtil.background.rstd.
    %            Parameters for the 'pix method:
    %            'PixStdFun' - Function handle for calculating the std per
    %                   pixel. Default is @imUtil.background.rstd.
    %            'PixStdFunArgs' - A cell array of arguments to pass to the
    %                   PixStdFun function. Default is {3}.
    % Output : - A structure array of results. The returned fields depands
    %            on the method. For the 'block' method return an element
    %            per image with the following fields:
    %               'AllStd' - Std as measured in each block.
    %               'ReadNoise' - Estimated read noise in each single image.
    %               'ReadNoiseRMS' - Std of estimated read noise per image.
    %               'ReadNoiseErr' - Error of estimated read noise per image.
    %            For the 'pix' method, return a single element with the
    %            following fields:
    %                'ReadNoise' - An image of read noise per pixel.
    %                   Read noise is returned in native units.
    % Author : Eran Ofek (Jun 2021)
    % Example: AI = AstroImage({randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100});
    %          Result = imProc.instCharc.readNoiseFromBias(AI)
    %          Result = imProc.instCharc.readNoiseFromBias(AI, 'Method','pix')
    
    arguments
        Obj AstroImage
        Args.Method                          = 'block';
        Args.CCDSEC                          = [];
        
        % Block method args:
        Args.BlockSize                       = [64 64];
        Args.OverlapXY                       = [0 0];
        Args.BlockStdFun function_handle     = @imProc.stat.rstd;  % Std = Fun(AstroImage)
        Args.BlockMeanFun function_handle    = @tools.math.stat.nanmedian;
        Args.BlockMeanStdFun function_handle = @imUtil.background.rstd;
        
        % pix method args:
        Args.PixStdFun function_handle       = @imUtil.background.rstd;
        Args.PixStdFunArgs cell              = {3};
        
    end

    
    switch lower(Args.Method)
        case 'block'
            % the block method - build sub images and calc rms
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % for each image
                SubImages = imProc.image.image2subimages(Obj(Iobj), Args.BlockSize, 'CCDSEC',Args.CCDSEC, 'OverlapXY',Args.OverlapXY);
                StdBlock  = Args.BlockStdFun(SubImages);
                % calc RN
                Result(Iobj).AllStd       = StdBlock;
                Result(Iobj).ReadNoise    = Args.BlockMeanFun(StdBlock(:));
                Result(Iobj).ReanNoiseRMS = Args.BlockMeanStdFun(StdBlock(:));
                Result(Iobj).ReanNoiseErr = Result(Iobj).ReanNoiseRMS./sqrt(numel(SubImages));
            end
        case 'pix'
            % the pix method - RN per pix from std of stack of images
            CubeImage = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC);
            Result.ReadNoise = Args.PixStdFun(CubeImage, Args.PixStdFunArgs{:});
        otherwise
            error('Unknown Method option');
    end
    


end