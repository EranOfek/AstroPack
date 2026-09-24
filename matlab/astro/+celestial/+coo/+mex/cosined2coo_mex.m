% Fast mex for cosine direction to coordinates
% Description: Convert cosine directions to coordinates in the same
%              reference frame. See also: cosined.m, coo2cosined.m
% Input  : - Matrix of first cosine directions.
%          - Matrix of second cosine directions.
%          - Matrix of third cosine directions.
% Output : - Matrix of longitudes [radians].
%          - Matrix of latitudes [radians].
% Author : ChatGPT + Eran Ofek (Oct 2025)
% Compilation: mex -R2018a -O CXXFLAGS="\$CXXFLAGS -march=native -Ofast -ffast-math -fno-math-errno -funroll-loops -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" cosined2coo_mex.cpp
% Example: [RA,Dec]=celestial.coo.mex.cosined2coo_mex(0.1,0,1)