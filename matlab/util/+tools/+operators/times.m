function Result = times(A, B)
    %
    % Input  : - A - Array
    %          - B - 
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
        A                  % Input & Output array
        B				   % Input array
        UseMex = true;     % True: Use MEX implementation, False: Use MATLAB implementaion
		UseBLAS = true;    % True: Use threading with BLAS library		
        UseMP = true;      % True: Use threading with OpenMP multi-threading library
    end

    % MATLAB implementation
    if ~UseMex
        Result = A .* B
        return;
    end
    
    % MEX implementation
    % Call function according to input data type
    C = lower(class(Array));    
    switch C
        case {'uint8','int8'}
             Result = tools.operators.times.mex.mex_times8(A, B, int32(UseBLAS), int32(UseMP));                               
        case {'uint16','int16'}
             Result = tools.operators.times.mex.mex_times16(A, B, int32(UseBLAS), int32(UseMP));                   
        case {'uint32','int32'}
             Result = tools.operators.times.mex.mex_times32(A, B, int32(UseBLAS), int32(UseMP));                               
        case {'uint64','int64'}
             Result = tools.operators.times.mex.mex_times64(A, B, int32(UseBLAS), int32(UseMP));                               
        case {'single'}
             Result = tools.operators.times.mex.mex_timesSingle(A, B, int32(UseBLAS), int32(UseMP));                   
        case {'double'}
            Result = tools.operators.times.mex.mex_timesDouble(A, B, int32(UseBLAS), int32(UseMP));                   			
        otherwise
            error('tools.operators.times - Unsupported data type');
    end
end
