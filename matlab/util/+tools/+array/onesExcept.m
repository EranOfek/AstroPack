function Result = onesExcept(Mat, Scalar, Image, UseMex, UseMP)
    %
    % Input  : - Mat - input matrix
    %          - Scalar - scalar value to compare
    %          - Image - Datatype input
    %          - UseMex - true to use MEX optimization
    %          - UseMP - true to use threading
    %
    % Output : - The result of the operation.
    %
    % Author : Chen Tishler, Dan Elhanati (Apr 2023)
    % Example: 
    % UseMex = 0;
    % UseMP = 0;
    % mat = [3 6 9; 4 7 11];
    % scalar = 5;
    % image = 1;
    % matlab_res = tools.array.onesExcept(mat, scalar, image, UseMex, UseMP);

    %----------------------------------------------------------------------
    arguments
        Mat              	% Input array
        Scalar
        Image			   	%        
        UseMex = true;     	% True: Use MEX implementation, False: Use MATLAB implementaion
        UseMP = true;      	% True: Use threading with OpenMP multi-threading library
    end

    % MATLAB implementation
    if ~UseMex
        W = ones(size(Mat), 'like',Image);
        Flag = Mat>Scalar;
        W(Flag) = 0;
        Result = W;
        return;
    end
    
    % MEX implementation
    % Call function according to input data type
    C = lower(class(Mat));    
    switch C
        case {'uint8','int8'}
            Result = tools.array.mex.mex_onesExcept_int8(Mat, Scalar, Image, UseMP);               
        case {'uint16','int16'}
            Result = tools.array.mex.mex_onesExcept_int16(Mat, Scalar, Image, UseMP);
        case {'uint32','int32'}
            Result = tools.array.mex.mex_onesExcept_int32(Mat, Scalar, Image, UseMP);
        case {'uint64','int64'}
            Result = tools.array.mex.mex_onesExcept_int64(Mat, Scalar, Image, UseMP);
        case {'single'}
            Result = tools.array.mex.mex_onesExcept_single(Mat, Scalar, Image, UseMP);
        case {'double'}
            Result = tools.array.mex.mex_onesExcept_double(Mat, Scalar, Image, UseMP);
        otherwise
            error('tools.array.onExcept - Unsupported data type');
    end
end
