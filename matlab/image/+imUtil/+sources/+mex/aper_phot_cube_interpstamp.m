% Fast mex for aperture photometry with shifted PSF to a constant mask
%   See also: imUtil.sources.mex.aper_phot_cube_interp
%             imUtil.sources.mex.aper_phot_cube_simple
%   Perform aperture photometry for sources present in a cube of
%   images, where the center of each source is slightly different and
%   provided by the user. The aperture mask is pixelaized, but it is
%   the same for all sources.
%   This is achived by lanczos3 interpolaing
%   the data to the position of the mask.
%   The sums are calculated directly during the lanczos3 interpolation, and
%   in a box that bound the mask.
% Input  : - A cube of images. The image index is in the 3rd dim.
%          - A vector of background (per image slice). This background
%            will be subtracted from the corresponding images.
%          - A vector of X positions of sources (per image slice).
%          - A vector of Y positions of sources (per image slibe).
%          - A vector of aperture radii in which to calculate the aperture
%            photometry.
% Output : - A matrix of aperture flux per image slice (rows) and per
%            aperture radius (columns).
%          - A matrix of aperture area per image slice (rows) and per
%            aperture radius (columns).
% Compilation: mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" aper_phot_cube_interpstamp.cpp
% Author : Eran Ofek (Feb 2026)
% Example: [AperPhot3, AperArea3]=imUtil.sources.mex.aper_phot_cube_interpstamp(Cube, Bck, X1, Y1);