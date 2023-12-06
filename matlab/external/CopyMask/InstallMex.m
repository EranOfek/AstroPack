function Ok = InstallMex(SourceFile, varargin)
% INSTALLMEX - Compile and install Mex file
% The C,C++ or FORTRAN mex file is compiled and additional installation routines
% are started. Advanced users can call MEX() manually instead, but some
% beginners are overwhelmed by instructions for a compilation sometimes.
% Therefore this function can be called automatically from an M-function, when
% the compiled Mex-Function does not exist already.
%
% Ok = InstallMex(SourceFile, ...)
% INPUT:
%   SourceFile: Name of the source file, with or without absolute or partial
%               path. The default extension '.c' is appended on demand.
%   Optional arguments in arbitrary order:
%     Function name: Function is started after compiling, e.g. a unit-test.
%     Cell string:   Additional arguments for the compilation, e.g. libraries.
%     '-debug':      Enabled debug mode.
%     '-force32':    Use compatibleArrayDims under 64 bit Matlab.
%     '-replace':    Overwrite existing mex file without confirmation.
%
% OUTPUT:
%   Ok: Logical flag, TRUE if compilation was successful. Optional.
%
% COMPATIBILITY:
% - A compiler must be installed and setup before: mex -setup
% - For Linux and MacOS the C99 style is enabled for C-files.
% - The optimization flag -O is set.
% - The compiler directive -DMATLABVER<XYZ> is added to support pre-processor
%   switches, where <XYZ> is the current version, e.g. 708 for v7.8.
%
% EXAMPLES:
% Compile func1.c with LAPACK libraries:
%   InstallMex('func1', {'libmwlapack.lib', 'libmwblas.lib'})
% Compile func2.cpp, enable debugging and call a test function:
%   InstallMex('func2.cpp', '-debug', 'Test_func2');
% These lines can be appended after the help section of an M-file, when the
% compilation should be started automatically, if the compiled MEX is not found.
%
% NOTES:
% Suggestions for improvements and comments are welcome!
% Feel free to add this function to your FEX submissions, when you change the
% URL in the variable "Precompiled" accordingly.
%
% Tested: Matlab 6.5, 7.7, 7.8, 7.13, WinXP/32, Win7/64
% Author: Jan Simon, Heidelberg, (C) 2012-2015 matlab.THISYEAR(a)nMINUSsimon.de

% $JRev: R-z V:025 Sum:2XDFgCWTHUu4 Date:25-Jan-2015 20:20:43 $
% $License: BSD (use/copy/change/redistribute on own risk, mention the author) $
% $File: Tools\GLSource\InstallMex.m $
% History:
% 001: 27-Jul-2012 09:06, First version.
% 005: 29-Jul-2012 17:11, Run the unit-test instead of showing a link only.
% 006: 11-Aug-2012 23:59, Inputs are accepted in free order.
% 020: 30-Dec-2013 01:48, Show a question dialog if mex is existing already.

% Initialize: ==================================================================
% Global Interface: ------------------------------------------------------------
% URL to file or folder containing pre-compiled files, or the empty string if
% pre-compiled files are not offered:
Precompiled = 'http://www.n-simon.de/mex';

% Initial values: --------------------------------------------------------------
bakCD   = cd;
matlabV = [100, 1] * sscanf(version, '%d.%d', 2);  % Numerical Matlab version

% Program Interface: -----------------------------------------------------------
% Parse inputs:
Param       = {};
UnitTestFcn = '';
doDebug     = false;
debugFlag   = {};
force32     = false;
replace     = false;

% First input is the name of the source file:
if ~ischar(SourceFile)
   error_L('BadTypeInput1', '1st input must be a string.');
end

% Additional inputs are identified by their type:
% String:      unit-test function or the flag to enable debugging.
% Cell string: additional parameters for the MEX command
for iArg = 1:numel(varargin)
   Arg = varargin{iArg};
   if ischar(Arg)
      if strcmpi(Arg, '-debug')
         doDebug     = true;
         debugFlag   = {'-v'};
      elseif strcmpi(Arg, '-force32')
         force32     = true;
      elseif strcmpi(Arg, '-replace')
         replace     = true;
      elseif exist(Arg, 'file') == 2
         UnitTestFcn = Arg;
      else
         error_L('MissFile', 'Unknown string or missing file: %s', Arg);
      end
   elseif iscellstr(Arg)  % As row cell:
      Param = Arg(:).';
   else
      error_L('BadInputType', 'Bad type of input.');
   end
end

% User Interface: --------------------------------------------------------------
hasHRef = usejava('jvm');   % Hyper-links in the command window?

% Do the work: =================================================================
% Search the source file, solve partial or relative path, get the real
% upper/lower case:
[dummy, dummy, Ext] = fileparts(SourceFile);  %#ok<ASGLU>
if isempty(Ext)
   SourceFile = [SourceFile, '.c'];
end

whichSource = which(SourceFile);
if isempty(whichSource)
   error_L('NoSource', 'Cannot find the source file: %s', SourceFile);
end
[SourcePath, SourceName, Ext] = fileparts(whichSource);
Source                        = [SourceName, Ext];
mexFile                       = [SourceName, '.', mexext];
mexPath                       = SourcePath;

fprintf('== Compile: %s\n', fullfile(SourcePath, Source));

% Check if the compiled file is existing already:
whichMex = which(mexFile);
if ~isempty(whichMex)
   fprintf('  Existing already:  %s\n', whichMex);
   
   if ~replace
      % Ask the user if a new compilation is wanted:
      QuestReply = questdlg({['\bf', mfilename, ': ', SourceName, '\rm'], ...
         '', 'The function is existing already:', ...
         ['  ', TeXFirm(whichMex)], '', ...
         'Do you want to compile it here:', ...
         ['  ', TeXFirm(fullfile(mexPath, mexFile))], ''}, ...
         mfilename, 'Compile', 'Cancel', ...
         struct('Default', 'Cancel', 'Interpreter', 'tex'));
      
      % User does not want to recompile:
      if strcmp(QuestReply, 'Cancel')
         if nargout
            Ok = false;
         end
         return;
      end
   end
   fprintf('  Recompile in: %s\n\n', mexPath);
end

if ~ispc && strcmpi(Ext, '.c')
   % C99 for the GCC and XCode compilers.
   % Note: 'CFLAGS="\$CFLAGS -std=c99"' must be separated to 2 strings!!!
   Flags = {'-O', 'CFLAGS="\$CFLAGS', '-std=c99"'};
else
   Flags = {'-O'};
end

% Large array dimensions under 64 bit, possible since R2007b:
matlabVDef = {sprintf('-DMATLABVER=%d', matlabV)};
if matlabV >= 705
   if any(strfind(computer, '64')) && ~force32
      Flags = cat(2, Flags, {'-largeArrayDims'});
   else
      Flags = cat(2, Flags, {'-compatibleArrayDims'});
   end
end

% Compile: ---------------------------------------------------------------------
% Display the compilation command:
Flags  = cat(1, Flags(:), debugFlag, matlabVDef, Param(:), {Source});
cmdStr = textwrap({['mex', sprintf(' %s', Flags{:})]}, 72);
fprintf('%s\n', cmdStr{:});

cd(SourcePath);
try    % Start the compilation:
   mex(Flags{:});
   compiled = true;
   fprintf('Compiled successfully:\n  %s\n', which(mexFile));
   
catch  % Compilation failed - no MException to support Matlab 6.5:
   compiled = false;
   err      = lasterror;
   fprintf(2, '\n*** Compilation failed:\n%s\n', err.message);
   if ~doDebug  % Compile again in debug mode if not done already:
      try
         mex(Flags{:}, '-v');
      catch  % Empty
      end
   end
   
   % Show commands for manual compilation and download pre-compiled files:
   fprintf('\n== The compilation failed! Possible solutions:\n');
   fprintf('  * Install and set up a compiler on demand:\n');
   if hasHRef
      fprintf('    <a href="matlab:mex -setup">mex -setup</a>\n');
      fprintf('  * Try to compile manually:\n    cd(''%s'')\n    %s -v\n', ...
         SourcePath, cmd);
      if ~isempty(Precompiled)
         fprintf('  * Or download the pre-compiled file %s:\n', mexFile);
         fprintf('    <a href="matlab:web(''%s#%s'',''-browser'')">%s</a>\n', ...
            Precompiled, mexFile, Precompiled);
      end
   else  % No hyper-references in the command window without Java:
      fprintf('  * mex -setup\n');
      fprintf('  * Try to compile manually:\n  cd(''%s'')\n  %s -v\n', ...
         SourcePath, cmd);
      if ~isempty(Precompiled)
         fprintf('  * Or download the pre-compiled file %s:\n  %s\n', ...
            mexFile, Precompiled);
      end
   end
   fprintf('  * Or send this report to the author\n');
end

% Restore original directory and check precedence: -----------------------------
cd(bakCD);
if compiled
   allWhich = which(SourceName, '-all');
   if ~strcmpi(allWhich{1}, fullfile(mexPath, mexFile))
      Spec  = sprintf('  %%-%ds   ', max(cellfun('length', allWhich)));
      fprintf(2, '\n*** Failed: Compiled function is shadowed:\n');
      fprintf(2, [Spec, '*USED*\n'],     allWhich{1});
      fprintf(2, [Spec, '*SHADOWED*\n'], allWhich{2:end});
      
      compiled = false;
   end
   
   allWhichMex = which(mexFile, '-all');
   if length(allWhichMex) > 1
      fprintf(2, '\n::: Multiple instances of compiled file:\n');
      fprintf(2, '  %s\n', allWhich{:});
   end
end

% Run the unit-test: -----------------------------------------------------------
if ~isempty(UnitTestFcn) && compiled
   fprintf('\n\n== Post processing:\n');
   [dum, UnitTestName] = fileparts(UnitTestFcn);  %#ok<ASGLU> % Remove extension
   if ~isempty(which(UnitTestName))
      fprintf('  Call: %s\n\n', UnitTestName);
      feval(UnitTestName);
   else
      fprintf(2, '??? Cannot find function: %s\n', UnitTestFcn);
   end
end

% Return success of compilation: -----------------------------------------------
if nargout
   Ok = compiled;
end
fprintf('\n%s: ready.\n', mfilename);

% end

% ******************************************************************************
function error_L(ID, Msg, varargin)
% Automatic error ID and mfilename in the message:
error(['JSimon:', mfilename, ':', ID], ...
   ['*** %s: ', Msg], mfilename, varargin{:});

% end

% ******************************************************************************
function S = TeXFirm(S)
% Escape special characters for the TeX interpreter:
S = strrep(strrep(S, '\', '\\'), '_', '\_');

% end
