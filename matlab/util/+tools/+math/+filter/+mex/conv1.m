% 1D convolution (fast mex implementation)
% Input  : - A vector. Double or single.
%        : - A kernel (odd size). Type must be as the first input.
% Output : - The 1D convolution result with the same length and type as the input
%            vector.
% Compilation: mex -O CXXFLAGS="\$CXXFLAGS -fopenmp -mavx" LDFLAGS="\$LDFLAGS -fopenmp" conv1.cpp
% Author : Eran Ofek (Aug 2024)