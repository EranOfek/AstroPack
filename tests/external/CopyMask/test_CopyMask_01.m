function uTest_CopyMask(doSpeed)
% Automatic test: CopyMask
% This is a routine for automatic testing. It is not needed for processing and
% can be deleted or moved to a folder, where it does not bother.
% While CopyMask works under Matlab 6.5, this unit test function does not:
%   CAST, RANDI.
%
% uTest_CopyMask(doSpeed)
% INPUT:
%   doSpeed: Optional logical flag to trigger time consuming speed tests.
%            Default: TRUE. If no speed test is defined, this is ignored.
%
% OUTPUT:
%   On failure the test stops with an error.
%
% Tested: Matlab 7.7, 7.8, 7.13, WinXP/32, Win7/64
% Author: Jan Simon, Heidelberg, (C) 2015 j@n-simon.de

% $JRev: R-d V:003 Sum:v4M8g8SOttaO Date:25-Jan-2015 20:20:43 $
% $License: BSD (see Docs\BSD_License.txt) $
% $File: Tools\UnitTests_\uTest_CopyMask.m $
% History:
% 001: 18-Jan-2015 18:41, First version.

% Initialize: ==================================================================
% This function is for testing only, so efficiency and unreachable code is no
% problem:
%#ok<*UNRCH,*AGROW,*MSNU>

ErrID = ['JSimon:', mfilename];  % ID for error messages

if nargin == 0                   % Test speed as default
   doSpeed = true;
end

if doSpeed
   TestTime = 0.5;
else                             % At least some tests with poor precision
   TestTime = 0.1;
end
drawDiagram = false;             % Draw dependency from problem size

nRandTest = 1e4;                 % Number of random tests

% Hello: -----------------------------------------------------------------------
whichCopyMask = which('CopyMask');
fprintf('==== Test CopyMask:  %s\nVersion: %s\n\n', ...
   datestr(now, 0), whichCopyMask);

% No CAST for Matlab 6:
if sscanf(version, '%d', 1) < 7
   warning([ErrID, ':NoMatlab6'], ...
      'CopyMask works with Matlab 6, but this unit-test function does not.');
   return;
end

% Do the work: =================================================================
% Known answer test: -----------------------------------------------------------
fprintf('== Known answer tests:\n');
ErrTag = [ErrID, ':KAT'];

NumClassList = {'double', 'single', 'int64', 'int32', 'int16', 'int8'};
ClassList    = cat(2, NumClassList, {'char', 'logical'});
for iClass = 1:length(ClassList)
   aClass = ClassList{iClass};
   disp(['  ', aClass]);
   
   emptyIndex = logical([]);
   R = CopyMask(cast([], aClass), emptyIndex);
   if ~isequal(R, cast([], aClass))
      error(ErrTag, 'CopyMask failed for: [], []');
   end
   
   R = CopyMask(cast(1, aClass), emptyIndex);
   if ~isequal(R, cast([], aClass))
      error(ErrTag, 'CopyMask failed for: [1], []');
   end
   
   R = CopyMask(cast(0:1, aClass), emptyIndex);
   if ~isequal(R, cast([], aClass))
      error(ErrTag, 'CopyMask failed for: [0, 1], []');
   end
   
   first1 = true;
   first2 = logical([1, 0]);
   second = logical([0, 1]);
   third  = logical([0, 0, 1]);
   data   = cast([0, 1, 0], aClass);
   
   R = CopyMask(data, first1);
   if ~isequal(R, cast(0, aClass))
      error(ErrTag, 'CopyMask failed for: [0,1,0], [1]');
   end
   
   R = CopyMask(data, first2);
   if ~isequal(R, cast(0, aClass))
      error(ErrTag, 'CopyMask failed for: [0,1,0], [1, 0]');
   end
   
   R = CopyMask(data, second);
   if ~isequal(R, cast(1, aClass))
      error(ErrTag, 'CopyMask failed for: [0,1,0], [0, 1]');
   end
   
   R = CopyMask(data, third, []);
   if ~isequal(R, cast(0, aClass))
      error(ErrTag, 'CopyMask failed for: [0,1,0], [0, 0, 1]');
   end
   
   % Test with different numbers not for LOGICAL type:
   if ~strcmp(aClass, 'logical')
      data = reshape(cast(1:24, aClass), [2, 3, 4]);
      for k = 1:24
         mask    = false(1, 24);
         mask(k) = true;
         R = CopyMask(data, mask);
         if ~isequal(R, cast(k, aClass))
            error(ErrTag, 'CopyMask failed for element %d', k);
         end
      end
      
      R = CopyMask(data, [false, true], 1);
      E = cast(reshape(2:2:24, [1,3,4]), aClass);
      if ~isequal(R, E)
         error(ErrTag, 'CopyMask failed for dimension 1');
      end
      
      R = CopyMask(data, [false, true], 2);
      E = cast(reshape([3,4,9,10,15,16,21,22], [2,1,4]), aClass);
      if ~isequal(R, E)
         error(ErrTag, 'CopyMask failed for dimension 2');
      end
      
      R = CopyMask(data, [false, true, false, true], 3);
      E = cast(reshape([7:12, 19:24], [2,3,2]), aClass);
      if ~isequal(R, E)
         error(ErrTag, 'CopyMask failed for dimension 3');
      end
   end
   
   % Test complex values for numerical types only:
   if any(strcmp(aClass, NumClassList))
      data = reshape(cast((1:24) + 1i * (25:48), aClass), [2, 3, 4]);
      for k = 1:24
         mask    = false(1, 24);
         mask(k) = true;
         R = CopyMask(data, mask);
         if ~isequal(R, cast(k+1i*(k+24), aClass))
            error(ErrTag, 'CopyMask failed for element %d (complex)', k);
         end
      end
      
      R = CopyMask(data, [false, true], 1);
      E = cast(reshape((2:2:24) + 1i*(26:2:48), [1,3,4]), aClass);
      if ~isequal(R, E)
         error(ErrTag, 'CopyMask failed for dimension 1 (complex)');
      end
      
      R = CopyMask(data, [false, true], 2);
      x = [3,4,9,10,15,16,21,22];
      E = cast(reshape(x + 1i * (x + 24), [2,1,4]), aClass);
      if ~isequal(R, E)
         error(ErrTag, 'CopyMask failed for dimension 2 (complex)');
      end
      
      R = CopyMask(data, [false, true, false, true], 3);
      x = [7:12, 19:24];
      E = cast(reshape(x + 1i * (x + 24), [2,3,2]), aClass);
      if ~isequal(R, E)
         error(ErrTag, 'CopyMask failed for dimension 3 (complex)');
      end
   end
end
disp('  ok.');

% Random input tests: ----------------------------------------------------------
fprintf('\n== Random input test:\n  ');
dispDot = ceil(nRandTest / 20);
for k = 1:nRandTest
   if mod(k, dispDot) == 0
      fprintf('.');
   end
   
   % Create an array with random dimensions:
   nDim = randi([2, 5]);
   Dim  = randi(10, 1, nDim);
   if Dim(nDim) == 1 && nDim > 2   % Trailing singleton dimensions vanish
      Dim(nDim) = 2;
   end
   
   % Let one dimension be big sometimes:
   if rand < 0.05
      Dim2 = Dim;
      Dim2(randi([2, nDim])) = randi(1e5);
      if prod(Dim2) < 1e7
         Dim = Dim2;
      end
   end
   
   Data = rand(Dim) * 256;
   if rand > 0.5
      Data = Data + 0.5i * Data;
   end
   aClass = NumClassList{randi(length(NumClassList))};
   Data = cast(Data, aClass);
   
   if rand > 0.5  % Linear indexing:
      lenMask = randi(numel(Data));
      Mask    = rand(1, lenMask) > rand;
      
      A = CopyMask(Data, Mask);
      B = Data(Mask);
      B = B(:);
   else           % Dimension specified:
      opDim   = randi(nDim);
      lenMask = randi(Dim(opDim));
      Mask    = rand(1, lenMask) > rand;
      
      A = CopyMask(Data, Mask, opDim);
      
      c    = cell(1, nDim);
      c(:) = {':'};
      c{opDim} = Mask;
      B    = Data(c{:});
   end
   
   % Compare results from Matlab and Mex:
   if ~(isequal(A, B) || (isempty(A) && isempty(B)))
      fprintf('\n');
      error([ErrID, ':Rand'], 'CopyMask differs from Data(Mask).');
   end
end
fprintf('\n  ok: %d random tests\n', nRandTest);

% Provoke errors: --------------------------------------------------------------
fprintf('\n== Catching errors:\n');

tooLazy = 0;
try
   r = CopyMask(1:10, 1.5);
   tooLazy = 1;
catch
   disp('  ok: Mask of oher type than logical rejected.');
end
if tooLazy
   error(ErrID, 'Mask other than logical accepted.');
end

try
   r = CopyMask({1,2}, [true, false]);
   tooLazy = 1;
catch
   disp('  ok: Cell rejected.');
end
if tooLazy
   error(ErrID, 'Cell accepted.');
end

try
   r = CopyMask(1:10, true, 3);
   tooLazy = 1;
catch
   disp('  ok: Dimension out of bounds rejected.');
end
if tooLazy
   error(ErrID, 'Dimension out of bounds accepted.');
end

try
   r = CopyMask(1:3, [true, true, true, false]);
   tooLazy = 1;
catch
   disp('  ok: Mask longer than array rejected.');
end
if tooLazy
   error(ErrID, 'Mask longer than array accepted.');
end

try
   r = CopyMask(1:3, [true, true, false], 2, 'anyArgument');
   tooLazy = 1;
catch
   disp('  ok: Too many inputs rejected.');
end
if tooLazy
   error(ErrID, 'Too many inputs accepted.');
end

try
   r = CopyMask(rand(2,3,4), [true, true, true, false], 2);
   tooLazy = 1;
catch
   disp('  ok: Mask longer than dimension rejected.');
end
if tooLazy
   error(ErrID, 'Mask longer than dimension accepted.');
end

% Speed: -----------------------------------------------------------------------
fprintf('\n== Compare speed:\n');

if drawDiagram
   AxesH = axes('XScale', 'log', 'YScale', 'log', 'NextPlot', 'add');
end

fprintf('-- Linear indexing:\n');
fprintf('                M [msec]:   Mex [msec]:   Ratio:\n');
for MaskDensity = [0.1, 0.25, 0.5, 0.75, 0.9]
   fprintf('Mask density: %.2f\n', MaskDensity);
   
   if drawDiagram
      XData    = [];
      mYData   = [];
      mexYData = [];
      mLineH   = line('XData', XData, 'YData', [], 'Parent', AxesH, ...
         'Color', [1, 0, MaskDensity]);
      mexLineH = line('XData', XData, 'YData', [], 'Parent', AxesH, ...
         'Color', [0, 1, MaskDensity]);
   end
   
   for DataLen = [1e2, 1e3, 1e4, 1e5, 1e6, 1e7]
      Data = rand(1, DataLen);
      Mask = Data < MaskDensity;
      
      % Determine number of loops:
      iTime = cputime;
      iLoop = 0;
      while cputime - iTime < TestTime
         v = CopyMask(Data, Mask);  %#ok<*NASGU>
         clear('v');   % Suppress JIT acceleration for realistic times
         iLoop = iLoop + 1;
      end
      nDigit = max(1, floor(log10(max(1, iLoop))) - 1);
      nLoop  = max(4, round(iLoop / 10 ^ nDigit) * 10 ^ nDigit);
      pause(0.02);
      
      tic;
      for i = 1:nLoop
         v = CopyMask(Data, Mask);
         clear('v');
      end
      mexTime = toc / nLoop;
      
      tic;
      for i = 1:nLoop
         v = Data(Mask);
         clear('v');
      end
      mTime = toc / nLoop;
      
      fprintf('[1 x 1e%d]: ', log10(DataLen));
      fprintf('   %10.6f    %10.6f     %4.1f%%\n', ...
         mTime * 1e3, mexTime * 1e3, mexTime / mTime * 100);
      
      if drawDiagram
         XData    = [XData, DataLen];
         mYData   = [mYData, mTime];
         mexYData = [mexYData, mexTime];
         set(mLineH,   'XData', XData, 'YData', mYData);
         set(mexLineH, 'XData', XData, 'YData', mexYData);
      end
      drawnow;
   end
   fprintf('\n');
end

% Dimensions specified:
fprintf('-- Dimension specified:\n');
fprintf('Mask density: %.2f\n', MaskDensity);
fprintf('                       M [msec]:  Mex [msec]:   Ratio:\n');

MaskDensity = 0.5;
DataLen     = 1e4;
for Lead = [2,4,8,16,32,64]
   for Trail = [1,2,4,8,16,32,64]
      fprintf('[%3d x %5d x %3d]: ', Lead, DataLen, Trail);
      
      Data = rand(Lead, DataLen, Trail);
      Mask = Data(1, :, 1) < MaskDensity;
      
      % Determine number of loops:
      iTime = cputime;
      iLoop = 0;
      while cputime - iTime < TestTime
         v = CopyMask(Data, Mask, 2);  %#ok<*NASGU>
         clear('v');   % Suppress JIT acceleration for realistic times
         iLoop = iLoop + 1;
      end
      nDigit = max(1, floor(log10(max(1, iLoop))) - 1);
      nLoop  = max(4, round(iLoop / 10 ^ nDigit) * 10 ^ nDigit);
      pause(0.02);
      
      tic;
      for i = 1:nLoop
         v = CopyMask(Data, Mask, 2);
         clear('v');
      end
      mexTime = toc / nLoop;
      
      tic;
      for i = 1:nLoop
         v = Data(:, Mask, :);
         clear('v');
      end
      mTime = toc / nLoop;
      
      fprintf('  %8.3f     %8.3f    %5.1f%%\n', ...
         mTime * 1e3, mexTime * 1e3, mexTime / mTime * 100);
      
      drawnow;
   end
   fprintf('\n');
end

% Goodbye:
disp('== CopyMask passed the tests.')

% return;
