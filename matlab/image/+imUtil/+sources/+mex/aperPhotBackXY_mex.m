% (MEX) Given a cube in which each stamp ccontains centered source, perform aperture phot and back estimation around specified XY position.
%   Description: Fast aperture photometry and scalar background estimation for
%              non-centered image stamps (MxMxN cube). Aperture sums are computed
%              as simple pixel-center inclusions (hard edges). Background is
%              estimated per stamp from an annulus using the *median* of the
%              annulus pixels; the reported standard deviation is the
%              population std computed *around the median* (i.e., sqrt(mean((x - median)^2))).
%   Syntax: [AperFlux, Back, BackStd, NpixAper, NpixBack] = aperPhotBack_mex(Cube, X, Y, AperRadii, AnnulusRadii)
%
% Input : - MxMxN numeric array (single or double). Each MxM slice
%           is a stamp centered on a source. The geometric center
%           is assumed at ((M+1)/2, (M+1)/2) in MATLAB coordinates.
%         - A vector of X positions of the source center in each stamp.
%         - A vector of Y positions of the source center in each stamp.
%         - (AperRadii) A Kx1 vector of aperture radii [pixels], *ascending order*.
%           Aperture photometry is computed as the sum of all pixel
%           values whose pixel-center lies within radius <= r_k.
%         - (AnnulusRadii) [Rin Rout] (pixels). Background annulus inner and outer
%           radii. Pixels with Rin^2 < r^2 <= Rout^2 (by center) are used.
%	      - (SubBack) A logical flag indicating if to subtract background
%           from aperture flux. Default is true.
%
% Output: - AperFlux  : NxK matrix. Raw aperture sums per stamp (rows = stamps/third dim
%                       of Cube, columns = radii in AperRadii). No background subtraction.
%         - Back      : Nx1 vector. Per-stamp *median* of the annulus pixels (scalar background).
%         - BackStd   : Nx1 vector. Per-stamp *population* standard deviation of annulus
%                       pixels *around the median*, i.e. BackStd = sqrt(mean((x - median)^2)).
%         - NpixAper  : 1xK vector. Number of pixels included by geometry in each aperture
%                       (same for all stamps; depends only on M and radii).
%         - NpixBack  : Scalar. Number of pixels in the background annulus
%                       (same for all stamps; depends only on M and [Rin Rout]).
%
% Assumptions / Details:
%   * Stamps are centered on the source; center is taken as ((M+1)/2, (M+1)/2).
%   * Pixel inclusion uses **pixel centers** (hard-edge). No fractional pixel weighting.
%   * AperRadii must be sorted ascending. The implementation increments the aperture
%     sum from one radius to the next without re-summing inner pixels (O(1) amortized per pixel).
%   * Background:
%       - Back is the *median* of annulus pixels.
%       - BackStd is computed relative to the median (not the mean):
%         BackStd = sqrt( mean( (x - median)^2 ) ), i.e. population std (divide by N, not N-1).
%   * Geometry-only counts (NpixAper, NpixBack) are identical for all stamps.
%   * Works with single or double input; outputs AperFlux/Back/BackStd match the input class.
%     NpixAper and NpixBack are returned as double.
%
% Notes on Performance:
%   * Indices for all apertures and the annulus are computed once from geometry.
%   * Each stamp is processed in a single pass up to the largest radius; intermediate
%     radii reuse cumulative sums.
%   * OpenMP is used to parallelize over stamps when available (compile with -fopenmp).
%
% Authors : ChatGPT + Eran Ofek (Oct 2025)
%
% Example:
%   % Create a simple centered Gaussian on a grid and add background noise:
%   M = 31; N = 10;
%   [X,Y] = ndgrid(1:M,1:M);
%   cx = (M+1)/2; cy = (M+1)/2;
%   PSF = exp(-((X-cx).^2 + (Y-cy).^2)/(2*2^2));  % sigma=2 px
%   Cube = zeros(M,M,N);
%   for i=1:N
%       Cube(:,:,i) = 1000*PSF + 10 + randn(M,M); % flux + background + noise
%   end
%   AperRadii    = [2; 3; 4; 5];   % pixels (ascending)
%   AnnulusRadii = [7 10];         % background annulus
%   [AperFlux, Back, BackStd, NpixAper, NpixBack] = aperPhotBack_mex(Cube,X, Y, AperRadii, AnnulusRadii);
%   % If you need background-subtracted flux later:
%   k = 3;  % e.g., radius = 4 px
%   Flux_Bsub = AperFlux(:,k) - Back .* NpixAper(k);
%
% Compilation: mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -fopenmp -march=native" LDFLAGS="$LDFLAGS -fopenmp" aperPhotBackXY_mex.cpp
% See also: regionprops, medfilt2, prctile, accumarray
%           imUtil.sources.mex.aperPhotBack_mex