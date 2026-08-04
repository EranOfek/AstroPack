function [Flag, FracPix, Med] = backgroundLevel(Image, Args)
    % Check the quality of the image background to identify images with an excessive number of high-value pixels
    % Input  : - An array.
    %          * ...,key,val,... 
    %            'DiluteFactor' - Dilute the array by this factor.
    %                   If empty, no dilution and the result will be exact.
    %                   Default is 101.
    %            'UseMex' - A logical indicating if to use mex functions:
    %                   tools.array.mex.diluteArray
    %                   tools.array.mex.countAboveVal
    %                   tools.math.stat.mex.median
    %                   Default is true.
    %            'MaxPixFraction' - Max fraction of pixels above threshold
    %                   to define a bad image. Default is 0.4.
    %            'ThresholdBack' - Threshold vale. Default is 4000.
    %
    % Output : - Flag indicating if the image is ok.
    %            I.e., the fraction of pixels above Args.ThresholdBack is
    %            smaller than Args.MaxPixFraction.
    %            Will also return false if image is empty.
    %          - Fraction of pixels above threshold.
    %          - Median of image.
    % Author : Eran Ofek (2025 Sep) 
    % Example: [IsGoodImage, FracPixAboveThreshold, Med]= imUtil.quality.backgroundLevel(Image)

    arguments
        Image
        Args.DiluteFactor      = 101;
        Args.UseMex            = true;
        Args.MaxPixFraction    = 0.4;
        Args.ThresholdBack     = 4000;
    end

    if isempty(Image)
        Flag = false;
        FracPix = NaN;
        Med     = NaN;
    else
        if ~isempty(Args.DiluteFactor)
            if Args.UseMex
                ImageW = tools.array.mex.diluteArray(Image, Args.DiluteFactor);
            else
                ImageW = Image(1:Args.DiluteFactor:end);
            end
        else
            ImageW = Image;
        end
        
        if Args.UseMex
            Npix = tools.array.mex.countAboveVal(ImageW, Args.ThresholdBack);
        else
            Npix = sum(ImageW(:)>Args.ThresholdBack);
        end
    
        FracPix = Npix./numel(ImageW);
        Flag    = FracPix<Args.MaxPixFraction;
    
        if nargout>2
            if Args.UseMex
                % the mex median supports single/double only
                if isfloat(ImageW)
                    MedInput = ImageW(:);
                else
                    MedInput = double(ImageW(:));
                end
                Med = tools.math.stat.mex.median(MedInput,1);
            else
                Med = median(ImageW(:),1);
            end
        end
    end
end
