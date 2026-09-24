% Calculate sin and cos simultaneously using a fast mex function
% Input  : - An array of angles (radians).
% Output : - Sin of the input (single or double).
%          - Cos of the input (single or double).
% Author : Eran Ofek (2025 Sep) 
% Compilations: mex -O CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -ffast-math -fno-math-errno -fno-trapping-math -ffinite-math-only -funroll-loops -march=native -mtune=native -fopenmp -fno-exceptions -fno-rtti' LDFLAGS='$LDFLAGS -fopenmp -lm' sincos.cpp
% Example: [S,C]=tools.math.fun.mex.sincos(Ang);