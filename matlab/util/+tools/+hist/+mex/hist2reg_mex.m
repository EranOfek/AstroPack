% Fast mex 2D histogram with regular step size.
%   For faster version use: tools.hist.histcounts2regular_mex
% Input  : - An array of X coordinates (of any dimension).
%          - An array of Y coordinates.
%          - [Xmin Xmax] for histogram.
%          - [Ymin Ymax] for histogram.
%          - Number of bins in X.
%          - Numbre of bins in Y.
%          - Step size (skip elements). Default is 1.
%          - Ignore NaNs. Default is true.
% Output : - 1D histogram of array.
%          - Histogram edges.
%          - Histogram bin centers.
% Compilation: mex -R2018a CFLAGS="\$CFLAGS -O3 -march=native -DNDEBUG -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" hist1reg_mex.cpp
% Author : ChatGPT + Eran Ofek (Feb 2026)
% Example: [N1,Ex,Ey,Bx,By] = tools.hist.mex.hist2reg_mex(X, Y, [0 1], [0 1], 100, 50, 1,0);