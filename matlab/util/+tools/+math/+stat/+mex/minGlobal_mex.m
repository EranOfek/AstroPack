% Fast mex for global min of an array over all dim.
%   The code uses SIMD/AVX2 and is faster than matlab min when only one
%   output is requested.
% Input  : - An array
% Output : - Min value.
%          - Optional index of min value (will make the code slower).
% Author : Eran Ofek (2026 Mar) 
% Example: Min=tools.math.stat.mex.minGlobal_mex(Array);