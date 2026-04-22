% Set multiple bit val of selected elements in an array (Fast MEX function)
% Input  : - Array - Array of integers
%          * Arbitrary number of triplets:
%               Flags - Array of logical flags
%               Bit - Bit number
%               Value - 0 or 1
%                   
% Output : - Am array in which the bits were updated.
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -mavx2 -mfma -fopenmp -march=native -DNDEBUG' LDFLAGS='$LDFLAGS -fopenmp' bitsetFlagMulti.cpp
% Author : ChatGPT, Eran Ofek (Sep 2025)
% Example: Array = zeros(1726, 1726, 'uint32');
%          Flag  = rand(1726, 1726) > 0.9;
%          Bit   = 2;
%          Val   = true;
%          Result3 = tools.array.mex.bitsetFlagMulti(Array, Flag1, Bit1, Val1, Flag2, Bit2, Val2);    