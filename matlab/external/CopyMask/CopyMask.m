function Y = CopyMask(X, M, Dim)
% CopyMask - Logical indexing
% Y = CopyMask(X, Mask, Dim)
% INPUT:
%   X:    Array of type: DOUBLE, SINGLE, (U)INT8/16/32/64), LOGICAL, CHAR.
%         X can be complex.
%   Mask: Mask as LOGICAL vector.
%   Dim:  Specify dimension for masking. If omitted or the empty matrix []
%         linear indexing is applied. Optional, default: [].
% OUTPUT:
%   Y:    Array of same type as X. For linear indexing Y is a [N x 1] vector.
%
% NOTES:
% - Equivalent Matlab code: Y = X(Mask)
% - Difference to Matlab's logical indexing:
%   * 2 to 3 times faster.
%   * A column vector is replied in every case.
%   * Mask cannot be longer than the array, while Matlab allows additional
%     trailing values, when they are FALSE.
% - If Dim is specified and X is not small, this function is only some percent
%   faster than the equivalent Matlab code. See output of the unit test.
%
% EXAMPLES:
%   X = rand(2,3,4);
%   Y = CopyMask(X, X > 0.2);    % Matlab: X(X > 0.2)
%   M = [true, false, true];
%   Z = CopyMask(X, M, 2);       % Matlab: X(:, M, :)
%
% COMPILATION:
%   Call CopyMask without inputs for an automatic compilation.
%   See CopyMask.c for details or a manual compilation.
%
% Tested: Matlab 6.5, 7.7, 7.8, 7.13, WinXP/32, Win7/64
% Assumed Compatibility: higher Matlab versions, Mac, Linux
% Author: Jan Simon, Heidelberg, (C) 2015 j@n-simon.de

% $JRev: R-c V:002 Sum:9CaRc05SBUM4 Date:25-Jan-2015 20:20:43 $
% $License: BSD (use/copy/change/redistribute on own risk, mention the author) $
% $UnitTest: uTest_CopyMask $
% $File: Tools\GLSets\CopyMask.m $
% History:
% 001: 25-Jan-2015 18:24, First version.

% Initialize: ==================================================================
persistent FirstRun
if isempty(FirstRun)
   InstallMex('CopyMask.c', 'uTest_CopyMask');
   FirstRun = false;
end

% Global Interface: ------------------------------------------------------------
% Initial values: --------------------------------------------------------------
% Program Interface: -----------------------------------------------------------
if nargin == 0
   return;
end

% User Interface: --------------------------------------------------------------
% Do the work: =================================================================
% This code works, but the Mex is faster:
if nargin < 3 || isempty(Dim)  % Short-circuting!
   Y      = X(M);
else
   C      = cell(1, ndims(X));
   C(:)   = {':'};
   C{Dim} = M;
   Y      = X(C{:});
end

% return;
