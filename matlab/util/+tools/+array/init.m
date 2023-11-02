function Result = init(Size, Val, Type, UseMex, UseMP)
    % Initializes an array (1D, 2D or 3D) with a given value
    % Input  : - size of array [Size].
    %          - initialization value [Val]
    %          - Type: {'single','double','logical','int8','int16','int32','int64'}.
    %          - (UseMex) A logical indicating if to use mex version.
    %            Default is true.
    %          - (UseMP) A logical indicating if to use open MP.
    %            Default is true.
    % Output : - 1D/2D/3D array with input value
    % Author : Dan Elhanati (Oct 2023)
    % Example:
    %           UseMex = 0;
    %           UseMP = 0;
    %           Size = [120 120];
    %           Val = 5;
    %           Type = "int64";
    %           res = tools.array.init(Size, Val, Type, UseMex, UseMP);

    arguments
        Size                	            % Input array
        Val
        Type             = [];               
        UseMex logical   = true;      	% True: Use MEX implementation, False: Use MATLAB implementaion
        UseMP logical    = true;      	% True: Use threading with OpenMP multi-threading library
    end

    if UseMex
        % MEX implementation
        % Call function according to input data type   

        if isempty(Type)
            Type = class(Val);
        end

        switch Type
            case {'uint8','int8'}
                Result = tools.array.mex.mex_init_int8(Size, Val, UseMP);
            case {'uint16','int16'}
                Result = tools.array.mex.mex_init_int16(Size, Val, UseMP);
            case {'uint32','int32'}
                Result = tools.array.mex.mex_init_int32(Size, Val, UseMP);
            case {'uint64','int64'}
                Result = tools.array.mex.mex_init_int64(Size, Val, UseMP);
            case {'single'}
                Result = tools.array.mex.mex_init_single(Size, Val, UseMP);
            case {'double'}
                Result = tools.array.mex.mex_init_double(Size, Val, UseMP);
            case {'logical'}
                Result = tools.array.mex.mex_init_logical(Size, Val, UseMP);                
            otherwise
                error('tools.array.onesCondition - Unsupported data type');
        end
    else
        % MATLAB implementation
        if isscalar(Size)
            Result = ones(1, Size, Type).*Val;
        else
            Result = ones(Size, Type).*Val;
        end
    end
end
