% Fast calculation of the power spectrum using MEX
%     The result is not normalized
% Input  : - A vector of times.
%          - A vector of measurments.
%          - A vector of frequencies for which to calculate the power
%            sperctrum.
% Output : - A vector of the power spectrum per frequency.
% Author : Eran Ofek (2025 Sep) 
% Compilation: mex -O CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -ffast-math -fno-math-errno -fno-trapping-math -funroll-loops -march=native -mtune=native -fopenmp -fno-exceptions -fno-rtti' LDFLAGS='$LDFLAGS -fopenmp -lm' ps_mex_md.cpp
% Example: PS=timeSeries.period.mex.powerspec_mex(T,M,FreqVec);