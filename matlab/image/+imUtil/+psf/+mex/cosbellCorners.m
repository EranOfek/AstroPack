% Fast mex fir corner-base cosine-bell taper to a full image of a PSF in which the PSF is in the corner.
%       Apply a corner-based cosine-bell taper to a full
%       image or image cube in which the PSF is located in the four corners.
%       The taper is defined relative to each of the four corners of the image.
%       Pixels with radius smaller than or equal to the inner radius are kept
%       unchanged. Pixels with radius larger than or equal to the outer radius
%       are set to zero, unless they are within the outer radius of another
%       corner. Between the inner and outer radii, the taper decreases from 1
%       to 0 following a cosine law between 0 and 90 degrees.
%       The output image is multiplied by a weighting function constructed from
%       the four image corners. For each corner:
%       1. Pixels inside the inner radius are multiplied by 1.
%       2. Pixels outside the outer radius are multiplied by 0.
%       3. Pixels between the inner and outer radii are multiplied by
%           cos((pi./2).*T), where T runs linearly from 0 at the inner radius to
%           1 at the outer radius.
% Input  : - (FullCube) A 2-D image or 3-D image cube. The image index, if
%            present, is always in the 3rd dimension. The PSF is
%            assumed to be located in the four corners of each image.
%          - (AnnulusRadii) A two-element vector [InnerRadius, OuterRadius]
%            giving the inner and outer radii of the cosine-bell
%            taper, measured from each of the four corners.
%
% Output : - (NewFullCube) Image or image cube of the same size and class as
%            FullCube after multiplication by the corner cosine-
%            bell taper.
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' cosbellCorners.cpp
% Example: KK=imUtil.kernel2.gauss(4);                                                                               
%          q=cosbellCorners(fftshift(KK),[3 5]);             
%          surface(q);     