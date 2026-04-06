% Fast mex for calculating the radial profile of a 2-D image or a cube around a given center.
%   Done in a single pass over each image slice.
%   The radial profile is calculated in linear radial bins of width Step. For
%   each annulus the function returns the annulus center radius, mean value,
%   standard deviation, minimum value, and maximum value.
%
% Input  : - (Cube) A 2-D image or a 3-D cube. If Cube is a cube then the
%            image index is in the 3rd dimension. Cube can be
%            single or double precision.
%          - (X0) X center position in pixel units. X corresponds to the
%            2nd dimension (J). Default is the image center.
%          - (Y0) Y center position in pixel units. Y corresponds to the
%            1st dimension (I). Default is the image center.
%          - (MaxR) Maximum radius in pixels. Default is 100.
%          - (Step) Radial step size in pixels. Default is 1.
%          - (IgnoreNaN) Logical/scalar flag. If true, ignore NaN values in the
%            calculation. Default is false.
%
% Output : - (R) Column vector of radial bin centers. The first radius
%            is 0.5.*Step.
%          - (Mean) Mean value in each annulus. For cube input the output
%            size is [Nbin, Nimage].
%          - (Std) Sample standard deviation in each annulus. For cube
%            input the output size is [Nbin, Nimage].
%          - (Min) Minimum value in each annulus. For cube input the
%            output size is [Nbin, Nimage].
%          - (Max) Maximum value in each annulus. For cube input the
%            output size is [Nbin, Nimage].
%
% Notes :
%       The annuli are defined by:
%       [R-0.5.*Step, R+0.5.*Step)
%       where R starts at 0.5.*Step and increases linearly by Step.
%
%       The function avoids unnecessary work by scanning only the bounding box
%       around MaxR, rejecting pixels with radius larger than MaxR, and using a
%       separate optimized code path depending on the value of IgnoreNaN.
%
%       X0, Y0, MaxR, and Step are cast internally to the same class as Cube
%       before the radial-bin calculation is performed.
%
%       For cube input, the calculation is parallelized over image slices when
%       beneficial, and otherwise over spatial columns using OpenMP.
%
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Example: [R,M,S,Min,Max]=imUtil.psf.mex.radialProfile_mex(K,3001, 3001,500,1);