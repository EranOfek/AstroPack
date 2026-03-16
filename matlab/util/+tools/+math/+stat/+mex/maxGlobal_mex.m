% Fast mex for global max of an array over all dim.
%   The code uses SIMD/AVX2 and is faster than matlab min when only one
%   output is requested.
% Input  : - An array
% Output : - Max value.
%          - Optional index of min value (will make the code slower).
% Author : Eran Ofek (2026 Mar) 
% Cmpilation: mex CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' maxGlobal_mex.cpp
% Example: Min=tools.math.stat.mex.maxGlobal_mex(Array);