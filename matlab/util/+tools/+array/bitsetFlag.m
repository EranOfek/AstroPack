function Result = bitsetFlag(Array, Flag, Bit, Value, UseMex, UseMP)
    %
    % Input  : - Array - Array of integers
    %          - Flags - Array of logical flags
    %          - Bit - Bit number
    %          - Value - 0 or 1
    %          - UseMex - true to use MEX optimization
    %          - UseMP - true to use threads
    %
    % Output : - The result of the operation.
    %
    % Author : Chen Tishler (Apr 2023)
    % Example: 
    %    Array = zeros(3, 3, 'int32');
    %    Flag = rand(3, 3) > 0.9;
    %    Result = tools.array.bitsetFlag(Array, Flag, 1, 1);            
    %----------------------------------------------------------------------
    arguments
        Array                   % Input array
        Flag
        Bit
        Value = true;           % Value to look for
        UseMex = true;          % False: Use MATLAB implementaion, True: Use MEX implementation
        UseMP = true;           % True: Use threading with OpenMP
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
            Result = tools.array.mex.mex_bitsetFlag_int8(Array,  Flag, int32(Bit), int32(Value), int32(UseMP));               
        case {'uint16','int16'}
            Result = tools.array.mex.mex_bitsetFlag_int16(Array, Flag, int32(Bit), int32(Value), int32(UseMP));       
        case {'uint32','int32'}
            Result = tools.array.mex.mex_bitsetFlag_int32(Array, Flag, int32(Bit), int32(Value), int32(UseMP));       
        case {'uint64','int64'}
            Result = tools.array.mex.mex_bitsetFlag_int64(Array, Flag, int32(Bit), int32(Value), int32(UseMP));                   
        otherwise
            error('tools.array.bitsetFlag - Unsupported data type');
    end
end
