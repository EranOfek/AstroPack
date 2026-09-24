% unwraps radian phases P by changing absolute jumps greater than pi to their 2*pi complement.
%   Like matlab unwrap but about x5 times faster.
%   See matlab unwrap for details.
% Input  : - An array.
%          - Cutoff. Default is pi.
%          - Dimension. Default is 1.
%          * ...,key,val,... 
% Output : - Unwrap array.
% Author : Eran Ofek (2025 Jan) 
% Compile: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" unwrap_mex.cpp
% Notes  : When using this code not in package, it is x2 faster. Either
%          name resolution? or soemthing else? but still x5 compared than
%          matlab unwrap
% Example: a=tools.math.fft.mex.unwrap_mex(R);