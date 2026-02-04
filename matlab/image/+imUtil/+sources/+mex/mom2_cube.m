% Fast mex for calculating 2nd central moment for each slice in a cube of images
%   Single iteration, no weight, calculated in a circular support.
%   The second moment is calculated using "RESPONSIBILITY-WEIGHTED SCHEME"
%   (no PSF model, no iteration):"
%   For observed pixel value n and background B (same units):\n"
%     r = max(1 - B/n, 0) for n>0, else r=0\n"
%     s = n*r = max(n - B, 0)\n"
%   This is a fast soft-assignment of counts to 'source' vs 'background' that avoids\n"
%   negative weights / cancellation from direct background subtraction.\n"
% Input  : - A cube of images (single or double)
%          - A vector of background (one per slice) to subtract prior to
%            the 2nd moment calculation.
%          - A vector of X positions (for each image slice) around to calculate
%            the 2nd central moment.
%          - A vector of Y positions (for each image slice) around to calculate
%            the 2nd central moment.
%          - (MaxRadius), radius around X,Y. Pixels within this radius will be used
%            in the calculation of the 2nd central moment.
%            If NaN, then use all pixels.
% Output : - A vector of X^2 2nd central moment in X.
%          - A vector of Y^2 2nd central moment in Y.
%          - A vector of X*Y 2nd central moment in X*Y.
% Example: [X2,Y2,XY] = imUtil.sources.mex.mom2_cube(Cube, X,Y, NaN);
