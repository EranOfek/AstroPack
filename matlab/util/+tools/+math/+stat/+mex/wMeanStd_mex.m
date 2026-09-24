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
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -mavx2 -mfma -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" wMeanStd_mex.cpp
%              (with AVX512) mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -mavx512f -mavx512dq -mavx512vl -mfma -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" wmean_mex.cpp
% Example: [M,S,E]=tools.math.stat.mex.wMeanStd_mex(R,W)