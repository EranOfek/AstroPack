%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function out = build_mex(varargin)
%MATLAB_TABLE_DUMP.BUILD_MEX  Compile matlab/mex/matlab_table_dump_mex.c into the matlab/ folder.
%
%   matlab_table_dump.build_mex              % MATLAB: mex -R2018a ; Octave: mkoctfile --mex
%   matlab_table_dump.build_mex('-v')        % extra arguments are passed to mex
%
%   Needs a C compiler that MATLAB supports (Linux: gcc; Windows: MinGW-w64 or
%   Visual Studio, configured once with "mex -setup C"). Returns the MEX path.

here = fileparts(fileparts(mfilename('fullpath')));      % .../matlab
src = fullfile(here, 'mex', 'matlab_table_dump_mex.c');
if exist('OCTAVE_VERSION', 'builtin') ~= 0
    old = pwd;
    cleanup = onCleanup(@() cd(old));
    cd(here);
    [status, output] = system(sprintf('mkoctfile --mex -DMATLAB_TABLE_DUMP_OCTAVE -O2 -o matlab_table_dump_mex "%s"', src));
    if status ~= 0
        error('matlab_table_dump:build', 'mkoctfile failed:\n%s', output);
    end
else
    mex('-R2018a', '-O', '-outdir', here, varargin{:}, src);
end
clear('matlab_table_dump_mex');
out = which('matlab_table_dump_mex');
fprintf('built %s (%s)\n', out, matlab_table_dump_mex('version'));
end
