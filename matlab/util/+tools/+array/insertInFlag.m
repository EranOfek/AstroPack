function Val=countVal(Array,Dim,UseMex)
% Array=tools.array.insertInFlag(Array, Val, Flag, UseNot)
% Here Flag is a vector of logicals with the same length as Array.
% For UseNot=false, this will perform:
% Array(Flag) = Val;
% For UseNot=true, this will perform:
% Array(~Flag) = Val;
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
% Example: Val=tools.array.bitand_array(Array);
%--------------------------------------------------------------------------

if (nargin==1)
    Dim = 1;
end

% MEX version of AND still has bugs and is currently disabled
% @Todo, Chen, 22/12/2021
UseMex = false;

if (nargin < 3)
    UseMex = false;
end

C = lower(class(Array));
switch C
    case {'uint8','int8'}
        Nbit = 8;
        Fun  = @uint8;
    case {'uint16','int16'}
        Nbit = 16;
        Fun  = @uint16;
    case {'uint32','int32'}
        Nbit = 32;
        Fun  = @uint32;
    case {'uint64','int64'}
        Nbit = 64;
        Fun  = @uint64;
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


