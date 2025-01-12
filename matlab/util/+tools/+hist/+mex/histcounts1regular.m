% 1D histogram with regular (evenly spaced) grid (mex)
%     x2.6 faster than matlab.internal.math.histcounts
% Input  : - Vector (single or double).
%          - Vector of edges (evenly spaced).
% Output : - Histogram
% Author : Eran Ofek (2025 Jan) 
% Example: tic; for I=1:3e4, N1= matlab.internal.math.histcounts(R,Edges); end,toc
%          tic; for I=1:3e4, N1=tools.hist.mex.histcounts1regular(R,Edges); end,toc