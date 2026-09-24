% Fast MEX quantile over all dimensions in the array, for a scalar quantile fraction
% Input  : - An array
%          - Scalar quantile fraction (0 to 1).
% Output : - Quantile value
% Author : ChatGPT + Eran Ofek (2025 Sep) 
% Compilation: mex -O CXXFLAGS="-std=c++17 -O3 -march=native -fno-exceptions -fno-rtti" quantile1.cpp
% Example: V=tools.math.stat.mex.quantile1(rand(1700,1700),0.95);