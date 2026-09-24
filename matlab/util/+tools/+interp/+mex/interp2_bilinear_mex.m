% Fast mex for 2D bilinear interpolation
%   With input/output similar to matlab (no extrapolation).
% Input  : - Input X grid (vector or matrix).
%          - Input Y grid (vector or matrix).
%          - 2D matrix to interpolate.
%          - Output X grid (vector or matrix).
%          - Output Y grid (vector or matrix).
% Output : - Interpolated 2D matrix.
% Author : Eran Ofek (2026 Feb) 
% Example: tools.interp.mex.interp2_biliear_mex(Xin,Yin,Z,Xout,Yout);