% Fast median function for arrays over a single dim
%   Faster than matlab median for larger arrays.
% Input  : - An array (single or double).
%          - Dimension (scalar) over which to calculate the median.
%          - 'omitnan'|'includenan' option. Default is 'includenan'.
% Output : - The median.
% Compilation : mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -Ofast -march=native -fopenmp -DNDEBUG" LDFLAGS="\$LDFLAGS -fopenmp" median.cpp
% Author : ChatGPT, Eran Ofek (2024 Aug) 
% Example: A=rand(1726,1726,20);                                   
%          tools.math.stat.mex.median(A);
%          
%          tic;for i=1:3, tools.math.stat.mex.median(A,1,'omitnan');end,toc
%          % Elapsed time is 1.051544 seconds.
%          tic;for i=1:3, median(A,1,'omitnan');end,toc                    
%          % Elapsed time is 2.486143 seconds.
%          tic;for i=1:3, tools.math.stat.mex.median(A,3,'omitnan');end,toc
%          % Elapsed time is 1.615232 seconds.
%          tic;for i=1:3, median(A,3,'omitnan');end,toc                    
%          Elapsed time is 4.354529 seconds.