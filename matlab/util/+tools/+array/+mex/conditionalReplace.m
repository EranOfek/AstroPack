% (MEX) Conditional replace: Given M,A,B,V perform: M(A>B)=V;
% Input  : - M: Array of any dimension and any class.
%          - A: Array of the size of the first input.
%          - B: Scalar.
%          - V: Scalar.
% Output : - The array with replacments.
% Author : Eran Ofek (2025 Sep) 
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -march=native -fopenmp -fno-math-errno -fno-trapping-math" LDFLAGS="$LDFLAGS -fopenmp" conditionalReplace.cpp
% Example: tools.array.mex.conditionalReplace(M, A, B, V);