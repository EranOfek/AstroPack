function build(Blosc2Dir)
    % Build io.blosc2.mex.mex_blosc2_read against the C-Blosc2 library.
    %   C-Blosc2 is linked statically, so the MEX file needs nothing at run time. The library and headers come with
    %   the python-blosc2 package (pip install blosc2), or with conda's c-blosc2 package.
    % Input  : - Directory holding include/blosc2.h, include/b2nd.h and lib/libblosc2.a, e.g.
    %            '<venv>/lib/python3.12/site-packages/blosc2'.
    % Author : Dana Kovaleva (Nov 2026)
    % Example: io.blosc2.mex.build('/Data/dana/zc/venv/lib/python3.12/site-packages/blosc2')

    arguments
        Blosc2Dir {mustBeTextScalar}
    end

    Blosc2Dir = char(Blosc2Dir);
    Here      = fileparts(mfilename('fullpath'));
    Inc       = fullfile(Blosc2Dir, 'include');
    Lib       = fullfile(Blosc2Dir, 'lib', 'libblosc2.a');
    if ~isfile(fullfile(Inc,'b2nd.h')) || ~isfile(Lib)
        error('io:blosc2:build', 'b2nd.h or libblosc2.a not found under %s', Blosc2Dir);
    end
    mex('-R2018a', '-O', ['-I' Inc], fullfile(Here,'mex_blosc2_read.c'), Lib, '-lpthread', '-lm', '-ldl', ...
        '-outdir', Here);
end
