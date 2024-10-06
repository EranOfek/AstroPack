% Fast median of a vector without ignoring NaNs, using mex.
%   Note that if NaN re present in the input vector it will be modified.
% Input  : - A vector (single or double). NaNs are not ignored.
% Output : - The median.
% Compilation : mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" median1.cpp
% Author : Eran Ofek (2024 Aug) 
% Example: tools.math.stat.mex.median1(V)