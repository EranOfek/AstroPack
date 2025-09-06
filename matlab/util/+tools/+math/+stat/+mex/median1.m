% Fast median of a vector without ignoring NaNs, using mex.
%   Note that if NaN re present in the input vector it will be modified.
% Input  : - A vector (single or double). NaNs are not ignored.
% Output : - The median.
% Compilation : mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" median1.cpp
% Author : Eran Ofek (2024 Aug) 
% Example: tools.math.stat.mex.median1(V)
%          A=rand(1726,1726,20);                                   
%          tic;for i=1:5, tools.math.stat.mex.median1(A(:));end,toc  
%          % Elapsed time is 3.429818 seconds.
%          tic;for i=1:5, fast_median(A(:));end,toc                
%          % Elapsed time is 4.368703 seconds.
%          tic;for i=1:5, median(A(:));end,toc                     
%          % Elapsed time is 5.455333 seconds.