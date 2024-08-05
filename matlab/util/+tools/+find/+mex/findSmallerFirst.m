% Find the index of the first element in an array which is smaller then some value (fast mex).
% Input  : - An array of any dimension (single or double).
%          - Value (the same class as the first input argument).
% Output : - The index (double) of the first element in the Array, which
%            satisfy: Array(Ind)<Value.
% Author : Eran Ofek (Aug 2024)
% Compilation: mex CXXFLAGS="\$CXXFLAGS -mavx2" LDFLAGS="\$LDFLAGS" findSmallerFirst.cpp
% Example: tic;for J=1:1:1e3, IM=tools.find.mex.findSmallerFirst(A,V); end, toc