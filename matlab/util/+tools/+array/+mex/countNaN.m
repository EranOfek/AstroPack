% Count the number of NaN in an array (fast mex function)
%   Faster then matlab for arrays with >1e4 elements.
% Input  : - An array (single or double).
% Output : - Numbre of NaN in array.
% Author : Eran Ofek (2024 Aug) 
% Compilation: mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native" countNaN.cpp
% Example: R = rand(1000,1000); R(1)=NaN;
%          tools.array.mex.countNaN(R)