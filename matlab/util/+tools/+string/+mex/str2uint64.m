% Convert char or cell of chars to uint64.
% Input  : - Char or cell of chars.
% Output : - Array of uint64.
% Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" str2uint64.cpp
% Author : Eran Ofek (2024 Oct) 
% Example: tools.string.mex.str2uint64('12345');
%          tools.string.mex.str2uint64({'12345'; '67890'})