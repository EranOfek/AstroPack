% build_mex.m
% Build the array_stats MEX function.
% Run this from the mex_poc/build/ directory, or from anywhere after adjusting paths.

fprintf('\n=== array_stats MEX Build Script ===\n\n');

% --- Locate this script's directory to build relative paths ---------------
this_dir  = fileparts(mfilename('fullpath'));   % .../mex_poc/build
root_dir  = fileparts(this_dir);               % .../mex_poc
src_file  = fullfile(root_dir, 'src', 'array_stats.cpp');
out_dir   = fullfile(root_dir, 'matlab');      % place .mexw64 with the wrapper

% --- Sanity checks --------------------------------------------------------
if ~isfile(src_file)
    error('build_mex:srcNotFound', ...
          'Source file not found:\n  %s\nCheck that you are running from the correct directory.', src_file);
end

if ~isfolder(out_dir)
    mkdir(out_dir);
end

% --- Report environment ---------------------------------------------------
fprintf('MATLAB root   : %s\n', matlabroot);
fprintf('MEX extension : %s\n', mexext);
fprintf('Source file   : %s\n', src_file);
fprintf('Output dir    : %s\n\n', out_dir);

% --- Build options --------------------------------------------------------
% Uncomment -g to build a debug MEX (includes debug symbols, disables optimisation)
% build_flags = {'-R2018a', '-g', '-v'};
%
% -R2018a enables the interleaved complex API (mxGetDoubles, mxGetComplexDoubles, etc.)
% Without it, MATLAB uses the legacy R2017b API (mxGetPr only), and mxGetDoubles is undefined.
build_flags = {'-R2018a', '-v'};   % -v = verbose: shows exact compiler/linker commands

% --- Construct and display the mex command --------------------------------
cmd_parts = ['mex', build_flags, src_file, '-outdir', out_dir];
fprintf('Running command:\n  mex');
for k = 2:numel(cmd_parts)
    fprintf(' %s', cmd_parts{k});
end
fprintf('\n\n');

% --- Compile --------------------------------------------------------------
try
    mex(build_flags{:}, src_file, '-outdir', out_dir);
    mex_file = fullfile(out_dir, ['array_stats.' mexext]);
    if isfile(mex_file)
        fprintf('\nBUILD SUCCESS: array_stats.%s\n', mexext);
        fprintf('Output: %s\n\n', mex_file);
    else
        fprintf('\nWARNING: mex returned without error but output file not found:\n  %s\n\n', mex_file);
    end
catch ME
    fprintf('\nBUILD FAILED\n');
    fprintf('Error ID : %s\n', ME.identifier);
    fprintf('Message  : %s\n\n', ME.message);
    fprintf('Common fixes:\n');
    fprintf('  1. No C++ compiler configured  -> run: mex -setup C++\n');
    fprintf('  2. MSVC not on PATH            -> launch MATLAB from a VS Developer Prompt\n');
    fprintf('     or set the compiler via: mex -setup C++\n');
    fprintf('  3. Syntax error in .cpp        -> check the error line number above\n');
    fprintf('  4. Missing mex.h               -> confirm MATLAB root is correct: %s\n', matlabroot);
    rethrow(ME);
end
