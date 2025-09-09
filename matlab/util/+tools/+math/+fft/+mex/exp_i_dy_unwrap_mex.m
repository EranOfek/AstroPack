% Calculate: exp(1i.*DY.*unwrap(angle(OperY)))
%   Used by: imUtil.trans.shift_fft
% Input  : - DY (scalar or array).
%          - OperY (array).
% Output : - Result.
% Author : Eran Ofek (2025 Jan) 
% Compile: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" exp_i_dy_unwrap_mex.cpp
% Example: a=tools.math.fft.mex.exp_i_dy_unwrap_mex(DY, OperY);