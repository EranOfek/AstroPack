% Fast mex and approximate (better than 10^-5) log base-e function for double numbers
% Input  : - An array
% Output : - Log base-e of the array.
% Author : ChatGPT + Eran Ofek (Apr 2026)
%Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -Ofast -ffast-math -fno-math-errno -fno-trapping-math -funroll-loops' fastLogLUT_double.cpp              
% Example: tools.math.fun.mex.fastLogLUT_double(71);