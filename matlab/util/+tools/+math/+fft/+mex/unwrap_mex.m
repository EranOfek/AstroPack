% unwraps radian phases P by changing absolute jumps greater than pi to their 2*pi complement.
%   Like matlab unwrap but about x20 times faster.
%   See matlab unwrap for details.
% Input  : - An array.
%          - Cutoff. Default is pi.
%          - Dimension. Default is 1.
%          * ...,key,val,... 
% Output : - Unwrap array.
% Author : Eran Ofek (2025 Jan) 
% Example: a=tools.math.fft.mex.unwrap_mex(R);