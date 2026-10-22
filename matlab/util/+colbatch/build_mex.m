%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function out = build_mex(varargin)
%COLBATCH.BUILD_MEX  Compile matlab/mex/colbatch_mex.c into the matlab/ folder.
%
%   colbatch.build_mex              % MATLAB: mex -R2018a ; Octave: mkoctfile --mex
%   colbatch.build_mex('-v')        % extra arguments are passed to mex
%
%   Needs a C compiler that MATLAB supports (Linux: gcc; Windows: MinGW-w64 or
%   Visual Studio, configured once with "mex -setup C"). Returns the MEX path.

here = fileparts(fileparts(mfilename('fullpath')));      % .../matlab
src = fullfile(here, 'mex', 'colbatch_mex.c');
if exist('OCTAVE_VERSION', 'builtin') ~= 0
    old = pwd;
    cleanup = onCleanup(@() cd(old));
    cd(here);
    [status, output] = system(sprintf('mkoctfile --mex -DCOLBATCH_OCTAVE -O2 -o colbatch_mex "%s"', src));
    if status ~= 0
        error('colbatch:build', 'mkoctfile failed:\n%s', output);
    end
else
    mex('-R2018a', '-O', '-outdir', here, varargin{:}, src);
end
clear('colbatch_mex');
out = which('colbatch_mex');
fprintf('built %s (%s)\n', out, colbatch_mex('version'));
end
