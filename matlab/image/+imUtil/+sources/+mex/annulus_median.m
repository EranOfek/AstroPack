% Calculate median and std in annulus around center of images in a cube, and subtract the background from each slice.
% Input  : - A cube of images (single or double)
%          - Either [inner, outer] annulus radius, or
%            [annulus_width], where the outer radius equal to the maximum radius.
%          - A logical: 0 - The 3rd output argument will be a simple
%            std in the annulus.
%            1- the 3rd output argument is the std divided by Npix-1.
% Output : - A background-subtracted cube of images.
%          - A vector of median in the annulus of each image slice.
%          - A vector of std (or std/sqrt(Npix-1)) in the annulus of
%            each image slice.
%          - A scalar with the number of pixels in the annulus.
% Author : ChatGPT + Eran Ofek (Jan 2026)
% Compliation: mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" annulus_median.cpp
% Example: [Out,Bg,St,Npix] = imUtil.sources.mex.annulus_median(Cube, [4 7], 0);