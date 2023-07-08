function [sin_res, cos_res] = sincos(degs, UseMex, UseMP)
    %
    % Input  : - degs - input array
    %          - UseMex - true to use MEX optimization
    %          - UseMP - true to use threading
    %
    % Output : - sin and cosine arrays
    %
    % Author : Dan Elhanati (July 2023)
    % Example:
    %
    %
    %
    %
    %

    %----------------------------------------------------------------------
    arguments
        degs              	% Input array
        UseMex = true;     	% True: Use MEX implementation, False: Use MATLAB implementaion
        UseMP = true;      	% True: Use threading with OpenMP multi-threading library
    end

    % MATLAB implementation
    if ~UseMex
        sin_res = sin(degs);
        cos_res = cos(degs);
        return;
    end
    
    % MEX implementation
    % Call function according to input data type
    C = lower(class(degs));    
    switch C
        case {'uint8','int8'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int8(degs, UseMP);               
        case {'uint16','int16'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int16(degs, UseMP);
        case {'uint32','int32'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int32(degs, UseMP);
        case {'uint64','int64'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int64(degs, UseMP);
        case {'single'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_single(degs, UseMP);
        case {'double'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_double(degs, UseMP);
        otherwise
            error('tools.math.fun.sincos - Unsupported data type');
    end
end
