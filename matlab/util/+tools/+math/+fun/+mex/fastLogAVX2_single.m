% Fast mex and approximate (better than 10^-5) log base-e function for single numbers
% Input  : - An array
% Output : - Log base-e of the array.
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -Ofast -ffast-math -fno-math-errno -fno-trapping-math -funroll-loops' fastLogAVX2_single.cpp
% Example: tools.math.fun.mex.fastLogAVX2_single(71);