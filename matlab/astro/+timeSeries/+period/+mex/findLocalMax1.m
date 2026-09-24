% Find all local maxima above some threshold value (fast MEX)
% Input  : - A 1D vector of values.
%          - A threshold value.
% Output : - A list of all indices in the input vector for which the value
%            is larger than the threshold and the point is a local maxima.
% Author : Eran Ofek (2025 Oct) 
% Compilation: mex -O -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" findLocalMax1.cpp
% Example: p2=timeSeries.period.mex.findLocalMax1(P(:,2),100);