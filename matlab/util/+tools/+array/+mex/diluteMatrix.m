% Dilute an array. Select every Nth element from an arry into a column vector.
%   Analog to Array(1:StepSize:end), but about 25% times faster.
% Input  : - An array
%          - Step size.
% Output : - Colum vector of the diluted array.
% Compilation: mex diluteMatrix.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" 
% Author : Eran Ofek (2025 Jan) 
% Example: D=tools.array.mex.diluteMatrix(Array,5);
%          tic;for I=1:1e4, Out2=tools.array.mex.diluteMatrix(InS,5);end,toc          
%          tic;for I=1:1e4, Out1=InS(1:5:end);end,toc