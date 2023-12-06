function [sin_res, cos_res] = sincos(rads, UseMex, UseMP)
% Simultaneously calculate the sine and cosine for an array
% Input  : - Vector (only) of angles [radians].
%          - (UseMex) A logical indicating if to use mex version.
%            Default is true.
%          - (UseMP) A logical indicating if to use open MP.
%            Default is true.
% Output : - Vector of sine of angle.
%          - Vector of cosine of angle.
% Author : Dan Elhanati (July 2023)
% Example: randomAngles = rand(1, 10) * 2 * pi;
%          [matlab_sin, matlab_cos] = tools.math.fun.sincos(randomAngles, UseMex, UseMP);
%----------------------------------------------------------------------
    arguments
        rads              	% Input array of radians
        UseMex logical = true;     	% True: Use MEX implementation, False: Use MATLAB implementaion
        UseMP logical  = true;      	% True: Use threading with OpenMP multi-threading library
    end

    % MATLAB implementation
    if ~UseMex
        sin_res = sin(rads);
        cos_res = cos(rads);
        return;
    end
    
    % MEX implementation
    % Call function according to input data type
    C = lower(class(rads));    
    switch C
        case {'uint8','int8'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int8(rads, UseMP);               
        case {'uint16','int16'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int16(rads, UseMP);
        case {'uint32','int32'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int32(rads, UseMP);
        case {'uint64','int64'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_int64(rads, UseMP);
        case {'single'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_single(rads, UseMP);
        case {'double'}
            [sin_res,cos_res] = tools.math.fun.mex.mex_sincos_double(rads, UseMP);
        otherwise
            error('tools.math.fun.sincos - Unsupported data type');
    end
end
