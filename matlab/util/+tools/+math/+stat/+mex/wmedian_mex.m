% Weighted median MEX function
%   See also: tools.math.stat.wmedian
%             tools.math.stat.mex.wmedianStd_mex
% Input  : - An array.
%          - An array of weights (e.g., inverse variance)
%          - Dimension over to calculate the weighted median.
%            Default is 1.
% Output : - Weighted median.
% Author : ChatGPT + Eran Ofek (2025 Oct)
% Compilation: mex -O -R2018a CXXFLAGS="\$CXXFLAGS -fopenmp -O3 -march=native" LDFLAGS="\$LDFLAGS -fopenmp" wmedian_mex.cpp
% Example: [M]=tools.math.stat.mex.wmedian_mex(R,W)