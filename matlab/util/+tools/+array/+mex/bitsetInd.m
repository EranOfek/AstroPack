% Set bit val of selected elements, given by indices, in an array (Fast MEX function)
%   Only slightly faster than matlab.
%   Performs: Result = bitset(Array(Flag), Bit, Value);
%   This function is faster than tools.array.bitsetFlag
% Input  : - Array - Array of integers
%          - A vector of indices for which to set the bit value.
%          - Bit - Bit number
%          - Value - 0 or 1
%
% Output : - Am array in which the bits were updated.
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -mtune=native -DNDEBUG -funroll-loops' bitsetInd.cpp
% Author : ChatGPT, Eran Ofek (Sep 2025)
% Example: Array = zeros(1726, 1726, 'uint32');
%          Flag  = rand(1726, 1726) > 0.9;
%          Bit   = 2;
%          Val   = true;
%          Result3 = tools.array.mex.bitsetFlag(Array, Ind, Bit, Val);    