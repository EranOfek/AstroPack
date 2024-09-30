% Encode values as bits in integer (mex)
% Input  : - A vector with N elements. Each element specify the number of
%            bits in which each value will be encoded.
%          - A N column array. Each Row will be encoded.
% Output : - A row vector of N integer (32 or 64 bits) in which the numbers
%            are encoded. Each element correspond to one row in the second
%            input argument.
% Author : Eran Ofek (2024 Sep) 
% Example: a=tools.bit.bitEncode([32 32],[100 100; 100 200])