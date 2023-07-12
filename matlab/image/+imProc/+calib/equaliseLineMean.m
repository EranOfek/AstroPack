function Defects= equaliseLineMean(AstroImg,Args)
% equalise an image by compensating, line by line, the average of the line
%  background. This is useful notably for PTF images, which have been
%  incorrectly debiased, causing a striped background disomogeneity.
% Algorithm: for each line, background pixels are selected as those with
%  values below some defined quantile level. The average of their
%  intensities is computed, and subtracted from the line values.
%  The global mean of all background pixels is added to the whole image,
%  to preserve approximately the image mean.
% Inputs:
%     AstroImg    - an AstroImage object
%     key,val arguments:
%        Level     - quantile level for cutoff [default 0.4]
%        StripeDim - 1 to equalise column background, 2 for row [1]
%        Plot      - show debug plots [false]
%      -- classification parameters --
%        JumpAmpltiude - empirical threshold to declare a jump [0.05]
%        Gradient      - slope threshold to declare an intensity gradient [0.01]
%        LightBackground - threshold intensity to declare the image
%                          "bright" (ADU) [500]
% Outputs:
%     Defects - a strutcure reporting whether some common problems
%               have been found (e.g. intensity jumps, negative values,
%               vignetting), based on empiric thresholds
% Example:
%
%   Im=uint16(rand(120,160)*2^15);
%   Im(40:75,:)=Im(40:75,:)+uint16(3301);
%   AI=AstroImage(Im);
%   Defects=imProc.calib.equaliseLineMean(AI,'Plot',true);
%
% Author: Enrico Segre, June 2023
    arguments
        AstroImg AstroImage = [];
        Args.Level =0.4;
        Args.StripeDim= 2;
        Args.Plot = false;
        Args.JumpAmplitude = 0.05;
        Args.Gradient = 0.01;
        Args.LightBackground = 500;
    end

    Defects=cell(1,numel(AstroImg));
    for k=1:numel(AstroImg)
        [Defects{k},AstroImg(k).Image]=imUtil.calib.equaliseLineMean(AstroImg(k).Image,...
            "Gradient",Args.Gradient,"JumpAmplitude",Args.JumpAmplitude,...
            "Level",Args.Level,"LightBackground",Args.LightBackground,...
            "Plot",Args.Plot,"StripeDim",Args.StripeDim);
    end
