% Calculate the meand and std radial profile around a point in an image
%   See also: imUtil.psf.radialProfile
% Input  : - 2-D image (single or double).
%          - [X, Y] of position around to which to calculate the radial
%            profile.
%          - Maximum radius to which to calculate the profile
%          - Step size of radial profile
%          * ...,key,val,... 
% Output : - Vector of Radius 
%          - Vector of mean of pixels in annulus.
%          - Vector of std of pixels in annulus 
% Author : Eran Ofek (2024 Dec) 
% Example: [R,M,S]=imUtil.psf.mex.radialProfile(Image,[3001 3001],100,1);
%          K=randn(6001,6001)+10000.*imUtil.kernel2.gauss(8,[6001 6001]);
%          [R,M,S]=imUtil.psf.mex.radialProfile_mex(K,[3001 3001],500,1);