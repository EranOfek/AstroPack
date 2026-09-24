% 1D histogram of a single/double vector in a regular, equally spaced, grid (fast mex)
% Input  : - A vector (single, double).
%          - A vector of edges, in which to calculate the histogram.
%            The type of this argument must be identical to the type of the
%            first input argument.
% Output : - A vector of counts in each bin defined by two secssive edges.
% Author : Eran Ofek (Aug 2024)
% Example: tic;for I=1:1:1e4, N=tools.hist.mex.histcounts1regular_mex(V,E);end,toc