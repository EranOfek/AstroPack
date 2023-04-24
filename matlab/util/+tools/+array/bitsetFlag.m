function Result = bitsetFlag(Array, Flag, Bit, Value, UseMex)
    % Given an N-D Array, count the number of elements
    % that are exactly equal to the value in Val.
    % UseNot is an argument indicating if to count the number of 
    % elements that are not equal Val.
    % 
    % For example:
    % tools.array.countVal([1 2;3 NaN], NaN, true) will return 3
    % tools.array.countVal([1 2;3 NaN], NaN, false) will return 1
    % 
    % Why? sum(Array(~isnan(Array)) is slow.
    % 
    % Need a single, double, int16, int32, unit16, uint32 versions.
    %
    % Input  : - An array of integers.
    %          - Value to compare
    %          - Flag, true to count number of not equal elements
    %          - Flag, true to use MEX optimization.
    % Output : - The result of the operation.
    %
    % Author : Chen Tishler, April 2023
    % Example: Val=tools.array.countVal(Array);
    %----------------------------------------------------------------------
    arguments
        Array                   % Input array
        Flag
        Bit
        Value = true;           % Value to look for
        UseMex = true;          % False: Use MATLAB implementaion, True: Use MEX implementation
    end

    % MATLAB implementation
    if ~UseMex
        Result = bitset(Array(Flag), Bit, Value);
        return;
    end
    
    % MEX implementation
    % Call function according to input data type
    C = lower(class(Array));    
    switch C
        case {'uint8','int8'}
            Result = tools.array.mex_bitsetFlag8(Array,  Flag, int32(Bit), int32(Value));               
        case {'uint16','int16'}
            Result = tools.array.mex_bitsetFlag16(Array, Flag, int32(Bit), int32(Value));       
        case {'uint32','int32'}
            Result = tools.array.mex_bitsetFlag32(Array, Flag, int32(Bit), int32(Value));       
        case {'uint64','int64'}
            Result = tools.array.mex_bitsetFlag64(Array, Flag, int32(Bit), int32(Value));                   
        otherwise
            error('tools.array.bitsetFlag - Unsupported data type');
    end
end
