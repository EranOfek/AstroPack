% Fast mex for squeeze(mean(A,[1 2],'omitnan')) and squeeze(std(A,0,[1 2],'omitnan'))
%   This is an order of magnitude faster than matlab.
% Input  : - A 3D array.
% Output : - A column vector which length is equal to the 3rd dim of the
%            input, with the mean over first 2 dimensions, ignoring NaNs.
%          - Same but for the Std (normalized by N-1).
% Compilation: mex -R2018a CXXFLAGS="\$CXXFLAGS -fopenmp -mavx2 -std=c++17" LDFLAGS="\$LDFLAGS -fopenmp" squeezeStdCube_Dim12.cpp
% Author : Eran Ofek (2024 Aug) 
% Example: tic;for I=1:1:1000, [M,V]=tools.math.stat.mex.squeezeStdCube_Dim12(A);end,toc 
%          max(abs(squeeze(mean(A,[1 2],'omitnan'))-M))
%          max(squeeze(std(A,0,[1 2],'omitnan'))-V)