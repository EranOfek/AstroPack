% Fast mex 1D histogram with regular step size.
%   x 2-4 faster than matlab internal 1D histogram
% Input  : - An array (of any dimension).
%            The histogram will be calculated over all dims.
%          - [Xmin Xmax] for histogram.
%          - Number of bins.
%          - Step size (skip elements). Default is 1.
%          - Ignore NaNs. Default is true.
% Output : - 1D histogram of array.
%          - Histogram edges.
%          - Histogram bin centers.
% Compilation: mex -R2018a CFLAGS="\$CFLAGS -O3 -march=native -DNDEBUG -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" hist1reg_mex.cpp
% Author : ChatGPT + Eran Ofek (Feb 2026)
% Example: [N1,E,C] = tools.hist.mex.hist1reg_mex(X, [0 1], 100, 1,0);