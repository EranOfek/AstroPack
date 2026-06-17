% Measure the median flux at annuli around selected positions in an image
% Input  : - (Image) 2D single or double image.
%          - (XY) N x 2 matrix of [X,Y] positions.
%          - (Radii) [InnerRadius, OuterRadius].
%            Default is [10 12].
%
% Output : - MedAnnulusFlux: N x 1 median annulus flux values.
%
% Notes  : X is column coordinate, Y is row coordinate.
%          NaN values are ignored.
% Author : ChatGPT + Eran Ofek (Jun 2026)
% Compilation: mex CXXFLAGS="$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" fluxAtRadius.cpp
% Example: Fr=imUtil.sources.fluxAtRadius(Image,XY)