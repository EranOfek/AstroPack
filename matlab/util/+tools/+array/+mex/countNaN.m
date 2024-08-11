% Count the number of NaN in an array (fast mex function)
%   Faster then matlab for arrays with >1e4 elements.
% Input  : - An array (single or double).
% Output : - Numbre of NaN in array.
% Author : Eran Ofek (2024 Aug) 
% Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" countNaN.cpp
% Example: R = rand(1000,1000); R(1)=NaN;
%          tools.array.mex.countNaN(R)