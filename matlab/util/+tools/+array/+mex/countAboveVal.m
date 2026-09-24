% Count the number of elements in array above some value (MEX).
%   Equivalent, but faster than sum(Array>Val)
% Input  : - Array of any dim and any type.
%          - Threshold value.
% Output : - Count of values above threshold.
% Author : Eran Ofek (2025 Sep) 
% Compilation: Linux (OpenMP + AVX2): mex -O CXXFLAGS="$CXXFLAGS -std=c++11 -O3 -march=native -mavx2 -mfma -fopenmp -ffast-math -funroll-loops" LDFLAGS="$LDFLAGS -fopenmp" countAboveVal.cpp
%              Windows (MSVC, OpenMP + AVX2): mex -O COMPFLAGS="$COMPFLAGS /O2 /GL /arch:AVX2 /openmp /fp:fast" LINKFLAGS="$LINKFLAGS /LTCG" countAboveVal.cpp
% Example: Count=tools.array.mex.countAboveVal(Array, Val)