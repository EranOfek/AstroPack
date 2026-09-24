% Returns the sum(X^n) for all n=1..MaxPower, NaN ignored.
%   Very efficient for MaxPower>=3.
% Input  : - An array of any dim. (single or double)
%          - Max power.
% Output : - [sum(X(:)), sum(X(:).^2, ...] to Max Power.
% Author : ChatGPT + Eran Ofek (2026 Apr) 
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -Ofast -ffast-math -fno-math-errno -fno-trapping-math -funroll-loops -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' sumPowers_mex.cpp
% Example: a1=tools.sum.mex.sumPowers_mex(R,3);