function varargout = Cell2Vec(varargin)
% CELL2VEC - Concatenate cell elements to a vector
% The elements of all elements of a cell array are concatenated to one vector.
% This equals CELL2MAT, when the cell elements are vectors, but it is up to 5
% times faster.
%
% V = Cell2Vec(C)
% INPUT:
%   C: Cell array of any size and classes:
%        DOUBLE, SINGLE, (U)INT8/16/32/64, LOGICAL, CHAR.
%      All non-empty cell elements must be the same class.
% OUTPUT:
%   V: [1 x N] vector of all elements.
%
% NOTES:
% - The larger the cell array, the higher is the speedup compared to CAT.
% - It is not likely, that the concatenation of the contents of a cell is a
%   bottleneck for the runtime of a program. But it was so easy to implement
%   this faster than with Matlab's CAT or CELL2MAT.
%
% COMPILATION:
% The C-file is compiled automatically when the function is called the first
% time. For a manual compilation see: Cell2Vec.c -> Compile.
%
% Tested: Matlab 6.5, 7.7, 7.8, 7.13, WinXP/32, Win7/64
%         Compiler: LCC2.4/3.8, BCC5.5, OWC1.8, MSVC2008
% Assumed Compatibility: higher Matlab versions, Mac, Linux, 64bit
% Author: Jan Simon, Heidelberg, (C) 2010-2015 matlab.2010(a)n(MINUS)simon.de
%
% See also CELL2MAT, CStr2String.

% $JRev: R-e V:004 Sum:OQiphu/VEJ8t Date:03-May-2015 20:17:17 $
% $License: BSD (use/copy/change/redistribute on own risk, mention the author) $
% $File: Tools\GLSets\Cell2Vec.m $

% Dummy code, which calls the auto-compilation only: ---------------------------
persistent FirstRun
if isempty(FirstRun)
   ok = InstallMex('Cell2Vec.c', 'uTest_Cell2Vec');
   if ok
      FirstRun = false;
   end
end

% Try to call the MEX file: ----------------------------------------------------
if nargin ~= 0 && ~FirstRun
   [varargout{1:nargout}] = Cell2Vec(varargin{:});
end

% return;
