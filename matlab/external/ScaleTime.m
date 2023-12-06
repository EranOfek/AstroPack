function Yi = ScaleTime(Y, Ti, Tf, Tn)
% Linear interpolation of an array [MEX]
% The input data is an [M x N x ...] array and the columns are related to the
% index [1:M]. The output is an [Mi x N x ...] array with all 1 <= [Mi] <= M and
% [Mi] can be non-integer. This is equivalent to, but faster than:
%   Yi = interp1(1:size(Y, 1), Y, Mi, '*linear')
%
% Y must be a real double or single array. Extrapolation is not supported. The
% interpolation is applied in the 1st non-singelton dimension.
% The output Yi has the same type as the input.
%
% METHOD 1: Fast step mode
%   Yi = ScaleTime(Y, initialT, finalT, numberT)
%   Y:  Real double or single array.
%   initialT, finalT, numberT: 3 scalar numbers for equidistant interpolation
%       steps. This is much faster than using a vector Ti created explicitely
%       by LINSPACE(initialT, finalT, numberT).
%   Yi: Interpolated values as [numberT x size(Y, 2) x ...] array.
%
% METHOD 2: More general, but slower index mode
%   Yi = ScaleTime(Y, Ti)
%   Y:  Real double or single array.
%       Y must have at least 2 elements in the first dimension.
%       For a row vector, the interpolation is performed over the 2nd dimension.
%   Ti: Time steps for interpolation as vector of doubles.
%       The corresponding Yi will be:
%         Yi = Y(floor(Ti))     * (1 - Ti + floor(Ti)) + ...
%              Y(floor(Ti) + 1) * (Ti - floor(Ti))
%       All Ti must be inside the interval [1, size(Y, 1)].
%       Ti need not be monotonically increasing, but just the first and last
%       frame are checked to be inside the limits.
%   Yi: Interpolated values as [length(Ti) x size(Y, 2) x ...] array.
%
% EXAMPLES:
% Interpolate SIN and COS:
%   T  = linspace(0, 2*pi, 20).';
%   Ti = linspace(1, length(T), length(T) * 3);
%   Y  = [T, sin(T), cos(T)];
%   Y2 = ScaleTime(Y, Ti);
%   figure; plot(T, Y(:, 2:3), 'ob', Y2(:, 1), Y2(:, 2:3), 'xr');
%
% This function can be used also to interpolate data with non-equidistant time
% steps. For large arrays this is still much faster than INTERP1:
%   % Create time points with random distances and a SIN wave:
%   x  = sort(rand(1, 100));
%   x  = 2 * pi * (x-x(1)) / (x(end)-x(1));  % [0, ..., 2*pi]
%   y  = sin(x);
%   % Interpolate at times t (need not be evenly spaced also):
%   t = linspace(0, 2*pi, 50);
%   s = interp1(x, 1:100, t);    % Get bins related to x
%   y2 = ScaleTime(y, s);
%   figure; plot(x, y, 'ob', t, y2, 'xr');
%
% SPEED:
% - Usually ScaleTime.m is 2 to 3 times faster than Matlab's INTERP1('*linear'),
%   but for large Y (e.g. [100000 x 10]) and short Ti (e.g. 1:10) the speed gain
%   can reach the factor 30.
% - The MEX is 10 to 125 times faster than INTERP1. The index method (4 inputs)
%   is faster than the vector method (2 inputs), except for Y with much columns
%   (about > 100).
%   E.g. Y := [1000 x 1], Ti := [10000 x 1], numbers are iterations/sec on a
%   1.5GHz Pentium-M, WinXP, Matlab 2009a, 32bit, MSVC2008:
%                            Matlab6/OWC1.8   Matlab7.8/LCC3.8
%     ScaleTime.mex(index):       4900              3715
%     ScaleTime.mex(vector):       550              1150
%     ScaleTime.m:                 230               383
%     INTERP1:                     100               174
%     lininterp1f:                  29                30
%     qinterp1:                    104               174
% - Look at the speed table created by uTest_ScaleTime.
%
% COMPILE: see ScaleTime.c for details.
%   mex -O ScaleTime.c
%
% TEST: Run uTest_ScaleTime to verify the results and check the speed.
%
% Tested: Matlab 2009a, 2015b(32/64), 2016b, 2018b, Win7/10
% Author: Jan Simon, Heidelberg, (C) 2009-2020 j@n-simon.de
%
% Inspired by INTERP1 of Matlab 5.3 written by Clay M. Thompson 7-4-91
%
% See also ScaleTimeNU, INTERP1, INTERP2.

% $JRev: R-E V:056 Sum:9qzZL9AMhy9o Date:20-Oct-2020 11:52:23 $
% $License: BSD (use/copy/change/redistribute on own risk, mention the author) $
% $UnitTest: uTest_ScaleTime $
% $File: Tools\GLMath\ScaleTime.m $
% History:
% 038: 20-Aug-2010 01:23, TestScaleTime fixed: isEqualTol included.
% 052: 12-Jul-2020 22:45, Accept SINGLE as input.

% ==============================================================================
% = This is an M-implementation, which is faster than Matlab's INTERP1.        =
% = But prefer the Mex-version!                                                =
% ==============================================================================

% ******************************************************************************
% Delete this if you want to use the pure Matlab version:
persistent warnOnce hasAutoExpand
if isempty(warnOnce)
   warnOnce = true;

   matlabV       = [100, 1] * sscanf(version, '%d.%d', 2);
   hasAutoExpand = (matlabV > 901);
   
   whichMex = which([mfilename, '.', mexext]);
   if isempty(whichMex)
      warning(['JSimon:', mfilename, ':NoMex'], ...
         'Cannot find MEX function. Trying a compilation...');
      
      try
         InstallMex('ScaleTime.c', 'uTest_ScaleTime');
      catch ME
         fprintf(2, '%s\n', ME.message);
      end
   end
   
   % After the compilation future calls are processed by the MEX function.
   warning(['JSimon:', mfilename, ':OverrideMex'], ...
      '%s: MATLAB version is used instead of faster MEX.', mfilename);
end

% ******************************************************************************
% Get input as matrix or column vector:
if nargin > 1
   sizeY     = size(Y);
   doReshape = ~ismatrix(Y);
   if doReshape
      Y = reshape(Y, sizeY(1), []);
   end
   
   [nRow, nCol] = size(Y);
   doTranspose  = (nRow == 1 && nCol > 1);
   if doTranspose
      nRow = nCol;
      nCol = 1;
      Y    = Y(:);
   end
end

% Get interpolation steps:
switch nargin
   case 4
      % Check for out of range values of Ti:
      if and(Ti < 1, Tf > nRow)
         error(['JSimon:', mfilename, ':BadFrameNumber'], ...
            '*** %s: Interpolation frames out of range.', mfilename);
      end
      
      if Tn < 1.0
         Yi = Y([]);
         return;
      elseif Tn > 1
         Ti = Ti:((Tf - Ti) / (Tn - 1)):Tf;
      elseif Tf < Ti
         Ti = [];
      end
      
   case 2
      % Check for out of range values of Ti:
      if any(Ti < 1)  % Faster to do 2 tests then using OR - surprising
         error(['JSimon:', mfilename, ':Ti_lt_1'], ...
            '*** %s: All [Ti] must be >= 1.', mfilename);
      end
      if any(Ti > nRow)
         error(['JSimon:', mfilename, ':Ti_gt_nFrame'], ...
            '*** %s: All [Ti] must be <= number of rows of Y.', mfilename);
      end
      
   otherwise
      error(['JSimon:', mfilename, ':BadNInput'], ...
         '*** %s: 2 or 4 inputs required.', mfilename);
end

% Return the empty matrix, if no interpolation steps are wanted:
if isempty(Ti)
   Yi = Y([]);  % Same class as input
   return;
end

% The Matlab method needs at least 2 original time points. Although the MEX
% implementation could handle a scalar input also, it is not worth to consider
% this pathological case:
if nRow < 2
   error(['JSimon:', mfilename, ':SingleFrame'], ...
      '*** %s: Y must have at least 2 rows.', mfilename);
end

% Shape Ti:
Ti = Ti(:);

% Interpolation parameters:
Si = Ti - floor(Ti);
Ti = floor(Ti);

% Shift frames on boundary:
edge     = (Ti == nRow);
Ti(edge) = Ti(edge) - 1;
Si(edge) = 1;           % Was: Si(d) + 1;

% Now interpolate:
if nCol > 1  % && ~hasAutoExpand
   Si = Si(:, ones(1, nCol));  % Expand Si
end
Yi = Y(Ti, :) .* (1 - Si) + Y(Ti + 1, :) .* Si;

%Slower:
%Yi = Y(Ti, :) + (Y(Ti + 1, :) - Y(Ti, :)) .* Si(:, ones(1, nCol));

if doTranspose
   Yi = reshape(Yi, 1, []);
end
if doReshape
   Yi = reshape(Yi, [numel(Ti), sizeY(2:end)]);
end

end
