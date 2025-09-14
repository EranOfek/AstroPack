% Set bit val of selected elements in an array (Fast MEX function)
%   Performs: Result = bitset(Array(Flag), Bit, Value);
%   This function is faster than tools.array.bitsetFlag
% Input  : - Array - Array of integers
%          - Flags - Array of logical flags
%          - Bit - Bit number
%          - Value - 0 or 1
%
% Output : - Am array in which the bits were updated.
%
% Author : ChatGPT, Eran Ofek (Sep 2025)
% Example: Array = zeros(1726, 1726, 'uint32');
%          Flag  = rand(1726, 1726) > 0.9;
%          Bit   = 2;
%          Val   = true;
%          Result3 = tools.array.mex.bitsetFlag(Array, Flag, Bit, Val);    