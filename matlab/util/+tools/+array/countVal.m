function Result = countVal(Array, Val, UseNot, UseMex)
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
        Array               % Input array
        Val                 % Value to look for
        UseNot = false;     % False: Count items equal to Val, True: Count items non-equal to Val
        UseMex = true;     	% True: Use MEX implementation, False: Use MATLAB implementaion
        UseMP = true;      	% True: Use threading with OpenMP multi-threading library
    end

    % MATLAB implementation
    if ~UseMex
        if UseNot
            Result = sum(Array(:) ~= Val);
        else
            Result = sum(Array(:) == Val);
        end
        return;
    end
    
    % MEX implementation
    % Call function according to input data type
    C = lower(class(Array));
    switch C
        case {'uint16','int16'}
            Result = tools.array.mex.mex_countVal_int16(Array, int16(Val), UseNot);       
        case {'uint32','int32'}
            Result = tools.array.mex.mex_countVal_int32(Array, int32(Val), UseNot);       
        case {'single'}
            Result = tools.array.mex.mex_countVal_intSingle(Array, single(Val), UseNot);       
        case {'double'}
            Result = tools.array.mex.mex_countVal_intDouble(Array, double(Val), UseNot);       
        otherwise
            error('tools.array.countVal - Unsupported data type');
    end
end
