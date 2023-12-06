frtpath = fileparts(mfilename('fullpath'));
mex_frtpath = fullfile(frtpath, '+mex');

original_dir = pwd();  % Get the current working directory

% Change to the +mex subdirectory
cd(mex_frtpath);

% Compile the C++ source files
mex('coreFRT_single.cpp', 'CXXFLAGS="$CXXFLAGS -O4 -std=c++11"');
mex('coreFRT_double.cpp', 'CXXFLAGS="$CXXFLAGS -O4 -std=c++11"');

% Revert back to the original directory
cd(original_dir);

clear frtpath mex_frtpath original_dir;
