% (Mex) Performs: Result = ones(size(MatR2), ClassImage); W_Max(MatR2>Radius2) = Val
%       This is a new (faster) mex verions.
%       Old version in the +obsolete dir.
% Input  : - Matrix (i.e., MatR2).
%          - Scalar (i.e., Radius2).
%          - class(Image).
%          - Value. Default is 0.
% Output : - Matrix of results.
% Author : Eran Ofek (2025 Sep) 
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -march=native -mavx2 -mfma -fopenmp -fno-math-errno -fno-trapping-math" LDFLAGS="$LDFLAGS -fopenmp" onesCondition.cpp
% Example: W_Max1 = tools.array.onesCondition(MatR2,MomRadius2,class(Image));