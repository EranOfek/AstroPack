function Result = insertInFlag(Array, Val, Flag, UseNot, UseMex)
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
        Array
        Val
        Flag
        UseNot = false;
        UseMex = true;
    end

    if ~UseMex
        Result = Array;
        if UseNot
            Result(~Flag) = Val;
        else
            Result(Flag) = Val;
        end
        return;
    end
    
    C = lower(class(Array));
    switch C
        case {'uint16','int16'}
            Result = tools.array.mex_insertInFlag16(Array, int16(Val), Flag, UseNot);       
        case {'uint32','int32'}
            Result = tools.array.mex_insertInFlag32(Array, int32(Val), Flag, UseNot);       
        case {'single'}
            Result = tools.array.mex_insertInFlagSingle(Array, single(Val), Flag, UseNot);       
        case {'double'}
            Result = tools.array.mex_insertInFlagDouble(Array, double(Val), Flag, UseNot);       
        otherwise
            error('tools.array.insertInFlag - Unsupported data type');
    end
end
