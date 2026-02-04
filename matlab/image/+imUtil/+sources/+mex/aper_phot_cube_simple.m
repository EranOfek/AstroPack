% A fast mex for a simple pixelized aperture photometry around position
%   The function creates a pixelized aperture around the source position.
%   The aperture radius are in ascending order and the mex use this fact to
%   speed up the calculations.
% Input  : - A cube of images (single or double).
%          - A vector of background (one per image slice) that will be
%            subtracted prior to aperture photometry.
%          - Vector of X positions of source in the corresponding image.
%          - Vector of Y positions of source in the corresponding image.
%          - A vector of aperture radii [pix] in ascenbding order.
% Output : - A matrix with background subtracted flux for each image (row)
%            and each aperture (column).
%          - A matrix with aperture area [pix] per image (row) and aperture
%            (column).
% Author : Eran Ofek (Feb 2026)
% Example: [AperPhot2, AperArea2] = imUtil.sources.mex.aper_phot_cube_simple(Cube, Bck, X1, Y1, [2 4 6]);