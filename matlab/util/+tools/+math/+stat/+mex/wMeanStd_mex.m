% Weighted mean, std, and error of the mean MEX function
%   See also: tools.math.stat.wmean
% Input  : - An array.
%          - An array of weights (e.g., inverse variance)
%          - Dimension over to calculate the weighted median.
%            Default is 1.
% Output : - Weighted mean.
%          - Weighted std.
%          - Weighted error of the mean.
% Author : Eran Ofek (2025 Oct)
% Compilation: mex -O -R2018a CXXFLAGS="\$CXXFLAGS -fopenmp -O3 -march=native" LDFLAGS="\$LDFLAGS -fopenmp" wMeanStd_mex.cpp
% Example: [M,S,E]=tools.math.stat.mex.wMeanStd_mex(R,W)