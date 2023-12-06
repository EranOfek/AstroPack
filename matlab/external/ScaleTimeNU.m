function Yi = ScaleTimeNU(X, Y, Xi, Extrapolate)
% Fast linear interpolation, non-uniformly spaced input
% This function converts an interpolation job with non-uniformly spaced data to
% a job with uniformly spaced data, such that the fast C-Mex ScaleTime can be
% applied for the calculations. This is usually faster than INTERP1 of Matlab.
%
% Yi = ScaleTimeNU(X, Y, Xi, Extrapolate)
% INPUT:
%   X:  Coordinates of the Y values. X must be monotonically increasing.
%   Y:  Real double vector or array. For arrays the interpolation is performed
%       along the first dimension.
%   Xi: Coordinates for interpolation.
%   Extrapolate: String, for 'extrap' Yi is set to NaN, if Xi is outside the
%       original bins X. Optional, default: 'inside'.
%
% OUTPUT:
%   Yi: Interpolated values at the coordinates Xi. The size of Yi is
%       [length(Xi) x size(Y, 2) x ...] or a row vector.
%
% If the input data are uniformly spaced and all Xi are inside the limits, call
% the Mex function ScaleTime directly.
% Operating on a matrix is much faster than processing the single columns
% separately.
%
% EXAMPLES:
%   % Create time points with random distances and a SIN/COS waves:
%   x = sort(rand(1, 100));
%   x = 2 * pi * (x-x(1)) / (x(end)-x(1));  % Scale to [0, ..., 2*pi]
%   y = [sin(x'), cos(x')];
%   % Interpolate at e.g. uniformly spaced times t:
%   xi = linspace(0, 2*pi, 50);
%   yi = ScaleTimeNU(x, y, xi);
%   figure; plot(x, y, 'ob', xi, yi, 'xr'); axis('tight');
%
% Tested: Matlab 2009a, 2015b(32/64), 2016b, 2018b, Win7/10
% Author: Jan Simon, Heidelberg, (C) 2010-2020 j@n-simon.de
%
% See also ScaleTime, INTERP1, INTERP2.

% $JRev: R-l V:011 Sum:b/3KS3OVDQpk Date:18-Oct-2020 17:21:25 $
% $License: NOT_RELEASED $
% $File: Tools\GLMath\ScaleTimeNU.m $
% History:
% 001: 25-Aug-2010 12:13, Interpolate XI at first for non-uniform X.

% Initialize: ==================================================================
% Global Interface: ------------------------------------------------------------
% Initial values: --------------------------------------------------------------
% Program Interface: -----------------------------------------------------------
X  = X(:);
Xi = Xi(:);

% Reshape row vector input:
sizeY       = size(Y);
doTranspose = (sizeY(1) == 1 && length(sizeY) == 2);
if doTranspose
   Y     = Y(:);
   sizeY = [length(Y), 1];
end

% User Interface: --------------------------------------------------------------
% Do the work: =================================================================
% Get the indices of Xi in relation to the non-uniformly spaced X: -------------
% This is the fat-free core of INTERP1 for unevenly spaced data: Bin is the
% index of elements in Xi corresponding to elements of X:
[dummy, Bin] = histc(Xi, X);  %#ok<ASGLU>
H            = diff(X);       % Original step size

% Extrapolation wanted?
if nargin > 3 && strncmpi(Extrapolate, 'extrap', 6)  % Short-circuiting
   Bin(Xi == X(sizeY(1))) = sizeY(1) - 1;
   inside = (Bin ~= 0);
   inBin  = Bin(inside);
   
   % Call the fast Mex interpolation for the indices Xj, which are related to
   % the indices of Y:
   Xj = inBin + (Xi(inside) - X(inBin)) ./ H(inBin);
   
   sizeY(1)          = length(Xi);
   Yi(1:prod(sizeY)) = NaN;
   Yi                = reshape(Yi, sizeY);
   Yi(inside, :)     = ScaleTime(Y, Xj);
   
else  % No extrapolation, faster:
   % Extra treatment if last element is on the boundary:
   lastBin = Bin(length(Bin));
   if lastBin == sizeY(1)
      Bin(length(Bin)) = sizeY(1) - 1;
   end
   if Bin(1) < 1 || lastBin > sizeY(1)
      error(['JSimon:', mfilename, ':OutOfRange'], ...
         'Xi is out of the range specified by X.');
   end
   
   Xj = Bin + (Xi - X(Bin)) ./ H(Bin);
   Yi = ScaleTime(Y, Xj);
end

% Reshape to a row vector on demand:
if doTranspose
   Yi = reshape(Yi, 1, []);
end

end

