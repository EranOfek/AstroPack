% Fast mex function for dilute array like: Array(1:Step:end).
% Input  : - Array of any size or dimension.
%          - Step size.
% Output : - A column vector of the diluted array.
% Author : Eran Ofek (2025 Sep) 
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -march=native -fopenmp -fno-math-errno -fno-trapping-math" LDFLAGS="$LDFLAGS -fopenmp" diluteArray.cpp
% Example: V=tools.array.mex.diluteArray(Array, Step);