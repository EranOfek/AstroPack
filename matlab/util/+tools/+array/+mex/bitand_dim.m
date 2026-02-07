% Fast mex for bit-wise and operation along a specific dimension in an array
%   See also: tools.array.bitand_array.m
% Input  : - An array (any int, uint type).
%          - Dimension Default is 1.
% Output : - Squeezed array with the bit-wise and operation.
% Author : Eran Ofek (2026 Jan) 
% Compilation: mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -DNDEBUG" bitand_dim.cpp
% Example: A=tools.array.mex.bitand_dim(B);