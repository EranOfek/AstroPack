% Weighted median and weighted std MEX function
%   See also: tools.math.stat.wmedian
%             tools.math.stat.mex.wmedian_mex
% Input  : - An array.
%          - An array of weights (e.g., inverse variance)
%          - Dimension over to calculate the weighted median.
%            Default is 1.
% Output : - Weighted median.
%          - Weighted std.
% Author : ChatGPT + Eran Ofek (2025 Oct)
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" wmedianStd_mex.cpp
% Example: [M,S]=tools.math.stat.mex.wmedianStd_mex(R,W)