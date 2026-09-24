% Sum of array.^2 over one of the dimensions, ignore NaNs.
%   Eqivalent to: sum(Array.^2, Dim, 'omitnan')
% Input  : - An array.
%          - Dim. Default is 1.
% Output : - sum of squares.
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -Ofast -ffast-math -fno-math-errno -fno-trapping-math -funroll-loops -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' sum2_mex.cpp
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Example: tools.sum.sum2_mex(Array,1);