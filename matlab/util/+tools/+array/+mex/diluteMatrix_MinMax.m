% Dilute an array and select values between Min and Max.
%   Select every Nth element >Min & <Max from an arry into a column vector.
%   Analog to Out=Array(1:StepSize:end); Out=Out(Out>Min & Out<Max); 
%   but x2 faster.
% Input  : - An array
%          - Step size.
%          - Min value.
%          - Max value.
% Output : - Colum vector of the diluted array which are between Min and Max.
% Compilation: mex diluteMatrix_MinMax.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp"   
% Author : Eran Ofek (2025 Jan) 
% Example: D=tools.array.mex.diluteMatrix_MinMax(Array,5,0.1,0.9);
%          tic;for I=1:1e3, Out2=tools.array.mex.diluteMatrix_MinMax(InS,5,0.1,0.9);end,toc
%          tic;for I=1:1e3, Out1=InS(1:5:end); Out1=Out1(Out1>0.1 & Out1<0.9); end,toc     