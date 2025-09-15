% (MEX) Perform a bit-wise and operation along all elements in an array.
%   For less effient function use: tools.array.bitand_array
% Input  : - Array (uint8, 16, 32, 64).
%          - Dimension along to perform the operation.
% Output : - Vector of results.
% Author : Eran Ofek (2025 Sep) 
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -march=native -mavx2 -mfma -fopenmp -fno-math-errno -fno-trapping-math" LDFLAGS="$LDFLAGS -fopenmp" bitandArray.cpp
% Example: Val2=tools.array.mex.bitandArray(A,1);