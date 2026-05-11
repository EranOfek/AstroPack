% Add bright-source radial profiles to a 2D image
%   Add to Image several sources with positions X,Y and flux Flux,
%   using a radial profile sampled at integer radii:
%   Radius = 0:numel(RadialProfile)-1.
%   The output image has the same class as Image.
% Description: The function uses two algorithm branches. For compact
%              profiles, it loops over sources and updates only pixels
%              inside each source radius. For broad profiles, it loops over
%              image columns/pixels, builds the list of active sources per
%              column, and updates only rows that may receive flux. The
%              branch is selected using max(MaxRadius)./sqrt(numel(Image)).
% Input  : - A 2D image (single or double).
%          - Vector of X positions, in MATLAB 1-based coordinates.
%          - Vector of Y positions, in MATLAB 1-based coordinates.
%          - Vector of source fluxes.
%          - Vector of maximum radii, one per source.
%          - RadialProfile vector. Element I+1 corresponds to radius I.
%            The radii are given in integer pixels from 0 to
%            numel(RadialProfile)-1.
%          - Threshold for selecting the algorithm branch.
%            If max(MaxRadius)./sqrt(numel(Image)) < Threshold, use
%            source-centered loops; otherwise use pixel-centered loops.
%            Default is 0.1.
%          - InPlace logical flag. Default is false.
%            Currently the input image is duplicated for safety.
% Output : - New image with the source profiles added.
% Compilation : mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" addBrightSourceProfile.cpp
% Author : ChatGPT, Eran Ofek (2026 Apr)
% Example: Image = zeros(4096,4096,'single');
%          X = single([2000.3 2100.7]);
%          Y = single([1900.2 2200.5]);
%          Flux = single([1e5 5e4]);
%          MaxRadius = single([500 700]);
%          R = 0:1000;
%          RadialProfile = single(exp(-0.5.*(R./100).^2));
%          NewImage = imUtil.art.mex.addBrightSourceProfile(Image, X, Y, Flux, MaxRadius, RadialProfile);