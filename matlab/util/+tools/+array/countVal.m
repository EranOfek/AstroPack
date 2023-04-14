function Val = countVal(Array,Dim)
% Given an N-D Array, count the number of elements
% that are exactly equal to the value in Val.
% UseNot is an argument indicating if to count the number of elements that are not equal Val.
% 
% For example:
% tools.array.countVal([1 2;3 NaN], NaN, true) will return 3
% tools.array.countVal([1 2;3 NaN], NaN, false) will return 1
% 
% Why? sum(Array(~isnan(Array)) is slow.
% 
% Need a single, double, int16, int32, unit16, uint32 versions.
%
% Package: Util.array
% Description: Perform a bitand operation along all elements in an array
%              along a specific dimension.
% Input  : - An array of integers.
%          - Dimension along to perform the bitand operation. Default is 1.
%          - Flag, true to use MEX optimization if possible. Default is 1.
% Output : - The result of the bitand operation.
% See also: sum_bitor.m (the same)
% License: GNU general public license version 3
% Tested : Matlab R2020b
%     By : Chen Tishler, April 2023
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Val=tools.array.countVal(Array);
%--------------------------------------------------------------------------

UseMex = false;
if ~UseMex
    Val = sum(Array(~isnan(Array))
    return
end


if (nargin==1)
    Dim = 1;
end



if (nargin < 3)
    UseMex = false;
end

C = lower(class(Array));
switch C
    case {'uint16','int16'}
        Nbit = 16;
        Fun  = @uint16;
    case {'uint32','int32'}
        Nbit = 32;
        Fun  = @uint32;
    case {'single'}
        Nbit = 16;
        Fun  = @single;
    case {'double'}
        Nbit = 16;
        Fun  = @double;
    otherwise
        error('Unknown class - only integers are allowed');
end

% Check if we can use MEX implementation, convert input to uint64
if UseMex && (ndims(Array) <= 3) && (Dim <= ndims(Array))
     switch Nbit
        case 8
            Val = tools.array.mex_bitand_array8(Array, Dim);       
        case 16
            Val = tools.array.mex_bitand_array16(Array, Dim);       
        case 32
            Val = tools.array.mex_bitand_array32(Array, Dim);       
        case 64
            Val = tools.array.mex_bitand_array64(Array, Dim);       
    end            
else
    Val = 0;
    for Ibit=1:1:Nbit
        Val = Val + (2.^(Ibit-1)).*all(bitget(Array,Ibit),Dim);
    end

    % transform back to uint
    Val = Fun(Val);    
end


