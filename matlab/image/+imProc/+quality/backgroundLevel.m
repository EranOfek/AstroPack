function [Flag, FracPixAboveThreshold, Med] = backgroundLevel(AI, varargin)
    % Check the quality of the image background to identify images with an excessive number of high-value pixels
    % Input  : - An array of AstroImage object.
    %          * ...,key,val,... 
    %            'DiluteFactor' - Dilute the array by this factor.
    %                   If empty, no dilution and the result will be exact.
    %                   Default is 101.
    %            'UseMex' - A logical indicating if to use mex functions:
    %                   tools.array.mex.diluteArray
    %                   tools.array.mex.countAboveVal
    %                   Default is true.
    %            'MaxPixFraction' - Max fraction of pixels above threshold
    %                   to define a bad image. Default is 0.4.
    %            'ThresholdBack' - Threshold vale. Default is 4000.
    %
    % Output : - Array of Flags indicating if the image is ok.
    %            I.e., the fraction of pixels above Args.ThresholdBack is
    %            smaller than Args.MaxPixFraction.
    %          - Array of Fraction of pixels above threshold.
    % Author : Eran Ofek (2025 Sep) 
    % Example: [IsGoodImage, FracPixAboveThreshold]= imProc.quality.backgroundLevel(AI)

    Size    = size(AI);
    Flag    = true(Size);
    FracPixAboveThreshold = zeros(Size);
    Med                   = nan(Size);

    Nim = numel(AI);
    for Iim=1:1:Nim
        [Flag(Iim), FracPixAboveThreshold(Iim), Med(Iim)]= imUtil.quality.backgroundLevel(AI(Iim).ImageData.Image, varargin{:});
    end

end
