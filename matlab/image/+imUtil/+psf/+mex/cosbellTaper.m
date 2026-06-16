% Apply a circular cosine-bell taper to a 2D image or 3D cube.
% Description: Apply a circular cosine-bell radial taper to an image/cube
% Input  : - A 2D image or 3D cube. The input must be either single or
%            double precision. If a cube is given, the third dimension is
%            interpreted as the image-slice index.
%          - A two element vector [InnerRadius, OuterRadius].
%            Pixels with radius R <= InnerRadius are unchanged.
%            Pixels with radius R >= OuterRadius are set to zero.
%            Pixels with InnerRadius < R < OuterRadius are multiplied by
%            a cosine-bell taper:
%              W = 0.5.*(1 + cos(pi.*(R - InnerRadius)./(OuterRadius - InnerRadius)))
%            where the radius is measured relative to the image center:
%              Xc = (Ncol + 1)./2
%              Yc = (Nrow + 1)./2
% Output : - A tapered image/cube, with the same size and class as the
%            input Cube.
% Author : ChatGPT + Eran Ofek (Jun 2026)
% Compilation: mex CXXFLAGS="$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" cosbellTaper.cpp
% Example: Cube    = rand(2048,2048,10,'single');
%          Annulii = [500 900];
%          NewCube = imUtil.psf.mex.cosbellTaper(Cube, Annulii);
% See also: imUtil.psf
% This is a MEX function. The MATLAB m-file is provided for help only.