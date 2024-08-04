% Find values in an array (fast mex version of find)
% Input  : - An array (double, single, uint32)
%          - Value to search (double, single, uint32)
%            Type must be consistent with first input.
% Output : - Column vector of indices of found elements.
% Compilation: mex -O -largeArrayDims CXXFLAGS="\$CXXFLAGS -fopenmp -march=native" LDFLAGS="\$LDFLAGS -fopenmp" find_mex.cpp
% Author : Eran Ofek (Aug 2024)
% Example: tic;for J=1:1:1e3, IM=tools.find.mex.find_mex(A,V); end, toc
