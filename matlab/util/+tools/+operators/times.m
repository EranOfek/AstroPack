function Result = times(A, B, UseMex, UseMP, UseAVX)
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
        UseMP = true;      % True: Use threading with OpenMP multi-threading library
		UseAVX = true;     % True: Use threading with BLAS library		        
    end

    % MATLAB implementation
    if ~UseMex
        Result = A .* B;
        return;
    end
    
    % Must be of same type
    assert(isequal(class(A), class(B)));
    
    % MEX implementation
    % Call function according to input data type
    C = lower(class(A));    
    switch C
        case {'uint8','int8'}
             tools.operators.mex.mex_times8(A, B, int32(UseMP));                               
        case {'uint16','int16'}
             tools.operators.mex.mex_times16(A, B, int32(UseMP));                   
        case {'uint32','int32'}
             tools.operators.mex.mex_times32(A, B, int32(UseMP));                               
        case {'uint64','int64'}
             tools.operators.mex.mex_times64(A, B, int32(UseMP));                               
        case {'single'}
             tools.operators.mex.mex_timesSingle(A, B, int32(UseMP));                   
        case {'double'}
            tools.operators.mex.mex_timesDouble(A, B, int32(UseMP));                   			
        otherwise
            error('tools.operators.times - Unsupported data type');
    end
end
