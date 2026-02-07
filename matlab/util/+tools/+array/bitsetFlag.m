function Result = bitsetFlag(Array, Flag, Bit, Value, UseMex, UseMP)
    % Set bit val of selected elements in an array
    %   For faster MEX version see: tools.array.mex.bitsetFlag
    % Input  : - Array - Array of integers
    %          - Flags - Array of logical flags
    %          - Bit - Bit number
    %          - Value - 0 or 1
    %          - UseMex - If true, use new mex tools.array.mex.bitsetFlag
    %            If false, use old mex.
    %            If [false false] use matlab.
    %          - UseMP - true to use threads. Default is true.
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
        Value  = true;           % Value to look for
        UseMex = true;           % False: Use MATLAB implementaion, True: Use MEX implementation
        UseMP  = true;           % True: Use threading with OpenMP
    end


    if UseMex
        Result = tools.array.mex.bitsetFlag(Array, Flag, Bit, Value);
    else
        %UseMex is [false]
        if ~isscalar(UseMex)
            % MATLAB implementation
            Result = zeros(size(Array),'like',Array);
            Result(Flag) = bitset(Array(Flag), Bit, Value);
          
        else
            % UseMex is [false false]
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
    end
end
