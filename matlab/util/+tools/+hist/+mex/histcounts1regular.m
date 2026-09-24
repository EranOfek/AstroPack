% 1D histogram with regular (evenly spaced) grid (mex)
%     x2.6 faster than matlab.internal.math.histcounts
% Input  : - Vector (single | double | int32 | uint32).
%          - Lower edge.
%          - Bin size.
%          - Number of bins
% Output : - Histogram counts in uint32. For most application you may want
%            to convert this to single or double.
% Author : Eran Ofek (2025 Jan) 
% Compilation: Linux (OpenMP + AVX2): mex -O CXXFLAGS="$CXXFLAGS -O3 -march=native -mavx2 -mfma -fopenmp -ffast-math -funroll-loops" LDFLAGS="$LDFLAGS -fopenmp" hist1d_regular.cpp
%              Windows (MSVC, OpenMP + AVX2): mex -O COMPFLAGS="$COMPFLAGS /O2 /GL /arch:AVX2 /openmp /fp:fast" LINKFLAGS="$LINKFLAGS /LTCG" hist1d_regular.cpp
% Example: tic; for I=1:3e4, N1= matlab.internal.math.histcounts(R,Edges); end,toc
%          tic; for I=1:3e4, N1=tools.hist.mex.histcounts1regular(R,0,0.01,100); end,toc