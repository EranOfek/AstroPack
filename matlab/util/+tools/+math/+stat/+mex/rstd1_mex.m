% Fast mex for robust std using 0.25-0.75 quantiles
%   See also: tools.math.stat.rstd_nex
% Input  : - Array of and dim.
%          - Skip elements. For faster executation, increase this
%            parameter. Default is 1 (no skip).
% Output : - The robust std of the entire array of over all dims.
% Compilation : mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -ffast-math -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' rstd1_mex.cpp
% Author : Eran Ofek (2025 Oct) 
% Example: Rs=tools.math.stat.mex.rstd1_mex(Array,[],1);