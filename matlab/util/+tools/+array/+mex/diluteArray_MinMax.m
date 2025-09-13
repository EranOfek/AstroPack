% Fast mex function for dilute array like: Array(1:Step:end) and returning min and max.
% Input  : - Array of any size or dimension.
%          - Step size.
% Output : - A column vector of the diluted array.
%          - Min of diluted array.
%          - Max of diluted array.
% Author : Eran Ofek (2025 Sep) 
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -march=native -fopenmp -fno-math-errno -fno-trapping-math" LDFLAGS="$LDFLAGS -fopenmp" diluteArray_MinMax.cpp
% Example: [V,Min,Max]=tools.array.mex.diluteArray_MinMax(Array, Step)