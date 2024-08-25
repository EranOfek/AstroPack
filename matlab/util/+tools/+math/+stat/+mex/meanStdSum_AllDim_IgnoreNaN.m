% Calculate mean std, and sum of an array, over all dimensions, ignoring NaNs (fast mex).
%     About x2 (x10) faster than matlab for small (large) arrays
% Input  : - An array.
% Output : - Mean of the array.
%          - Std of the array.
%          - Sum of the array.
% Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx -mavx2 -std=c++17" LDFLAGS="\$LDFLAGS -fopenmp" meanStdSum_AllDim_IgnoreNaN.cpp
% Author : Eran Ofek (2024 Aug) 
% Example: A=rand(100000,3);
%          tic; for I=1:1000, [Mean, Std, Sum]=tools.math.stat.mex.meanStdSum_AllDim_IgnoreNaN(A); end, toc
%          tic;for I=1:1000, Mean1=mean(A,'all','omitnan'); Std1=std(A,[],'all','omitnan'); Sum1=sum(A,'all','omitnan'); end,toc