% Fast mex for global min and max of an array over all dim.
% Input  : - An array
% Output : - Min value.
%          - Max value.
% Author : Eran Ofek (2026 Mar) 
% Cmpilation: mex CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' minmaxGlobal_mex.cpp
% Example: [Min,Max]=tools.math.stat.mex.minmaxGlobal_mex(Array);