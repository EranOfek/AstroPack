% Set bit val of selected elements in an array (Fast MEX function)
%   Performs: Result = bitset(Array(Flag), Bit, Value);
%   This function is faster than tools.array.bitsetFlag
% Input  : - Array - Array of integers
%          - Flags - Array of logical flags
%          - Bit - Bit number
%          - Value - 0 or 1
%          - Logical: use prescan (true), or not (false).
%            Default is false.
%
% Output : - Am array in which the bits were updated.
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -mavx2 -mfma -fopenmp -march=x86-64-v3 -DNDEBUG' LDFLAGS='$LDFLAGS -fopenmp' bitsetFlag.cpp
%   x86-64-v3 (AVX2+FMA+BMI2) is the baseline these kernels need, and it is
%   what the committed .mexa64 is built with. -march=native would tune the
%   binary for the building machine only, which may not run elsewhere.
% Author : ChatGPT, Eran Ofek (Sep 2025)
% Example: Array = zeros(1726, 1726, 'uint32');
%          Flag  = rand(1726, 1726) > 0.9;
%          Bit   = 2;
%          Val   = true;
%          Result3 = tools.array.mex.bitsetFlag(Array, Flag, Bit, Val);    