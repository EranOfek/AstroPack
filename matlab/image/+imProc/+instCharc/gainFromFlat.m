function Result = gainFromFlat(Obj, Args)
    % Estimate the gain from flat image/s in native units.
    %   Estimated by dividing the mean by the variance.
    % Input  : - An AstroImage object with flat images.
    %          * ...,key,val,...
    %            'Method'   - One of the following estimation methods:
    %                   ['block'] - Estimate the gain based on a single image by
    %                       calculating the mean and var in each blck (sub image).
    %                   'pix' - Estimate the gain based on the mean and var of multiple
    %                       images in each pixel. The result is gain per pixel
    %                       image.
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of the image
    %                   section on which to estimate the gain.
    % Output : - A structure array of results. The returned fields depands
    %            on the method. For the 'block' method return an element
    %            per image with the following fields:
    %               'Gain' - Estimated gain in each single image.
    %               'GainRMS' - Std of estimated gain per image.
    %               'GainErr' - Error of estimated gain per image.
    %            For the 'pix' method, return a single element with the
    %            following fields:
    %                'Gain' - An image of gain per pixel.
    %                'GlobalGain' - median of image gain.
    %                   gain is returned in native units.
    % Author : Eran Ofek (Jun 2021)
    % Example: Image = poissrnd(ones(1000,1000).*1e4)./2.2;
    %          AI = AstroImage({Image});
    %          Result = imProc.instCharc.gainFromFlat(AI)
    %          for I=1:1:10, Cell{I} = poissrnd(ones(1000,1000).*1e4)./2.2; end
    %          AI = AstroImage(Cell);
    %          Result = imProc.instCharc.gainFromFlat(AI, 'Method','pix')
    
    arguments
        Obj AstroImage
        Args.Method                          = 'block';
        Args.CCDSEC                          = [];
        
        % Block method args:
        Args.BlockSize                       = [32 32];
        
    end

    
    switch lower(Args.Method)
        case 'block'
            % the block method - build sub images and calc rms
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % for each image
                SubImages = imProc.image.image2subimages(Obj(Iobj), Args.BlockSize, 'CCDSEC',Args.CCDSEC);
                
                Mean = imProc.stat.median(SubImages);
                Var  = imProc.stat.var(SubImages);
                
                Gain = Mean./Var;
                
                Result(Iobj).Gain    = median(Gain);
                Result(Iobj).GainRMS = std(Gain);
                Result(Iobj).GainErr = Result(Iobj).GainRMS./sqrt(numel(SubImages));
                
            end
        case 'pix'
            % the pix method - RN per pix from std of stack of images
            CubeImage = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC);
            % The image index in the cube is 1
            Dim   = 1;
            Mean  = median(CubeImage,Dim,'omitnan');
            Var   = var(CubeImage,[],Dim,'omitnan');
            
            Result.Gain       = Mean./Var;
            Result.GlobalGain = median(Result.Gain(:),1,'omitnan');
            
        otherwise
            error('Unknown Method option');
    end
    


end