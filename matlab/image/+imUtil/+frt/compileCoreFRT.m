frtpath=fileparts(mfilename('fullpath'));
mex(fullfile(frtpath,'coreFRT.cpp'),...
    [' -outdir ' frtpath ' CXXFLAGS="$CXXFLAGS -O4 -std=c++11"'])
clear frtpath