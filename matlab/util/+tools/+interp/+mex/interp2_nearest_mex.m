% Fast mex for 2D nearest interpolation
%   With input/output similar to matlab (no extrapolation).
% Input  : - Input X grid (vector or matrix).
%          - Input Y grid (vector or matrix).
%          - 2D matrix to interpolate (any type).
%          - Output X grid (vector or matrix).
%          - Output Y grid (vector or matrix).
% Output : - Interpolated 2D matrix.
% Author : Eran Ofek (2026 Feb) 
% Compilation: mex -O CXXFLAGS="\$CXXFLAGS -O3 -fopenmp -mavx2 -mfma" LDFLAGS="\$LDFLAGS -fopenmp" interp2_nearest_mex.cpp
% Example: tools.interp.mex.interp2_nearest_mex(Xin,Yin,Z,Xout,Yout);