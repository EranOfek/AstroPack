function Result = onesCondition(Mat, Radius, Type, UseMex, UseMP)
    % Create a logical matrix in which all values above a threshold are 0, and all values below are 1.
    %   This is equivalent to:
    %       W_Max = ones(size(MatR2), 'like',Image);
    %       W_Max(MatR2>MomRadius2) = 0;
    %       Used by moment2
    %   This code is using a mex version.
    % Input  : - 2D/3D matrix of values [Mat].
    %          - Threshold value.
    %          - Datatype: {[], 'single','double'}.
    %            If [], then use the same type as the first input argument.
    %            Default is [].
    %          - (UseMex) A logical indicating if to use mex version.
    %            Default is true.
    %          - (UseMP) A logical indicating if to use open MP.
    %            Default is true.
    % Output : - 2D/3D logical matrix.
    % Author : Dan Elhanati (Sep 2023)
    % Example: 
    %          MatR2 = rand(25,25,1000,'single');
    %          Type = 'single';
    %          res = tools.array.onesCondition(MatR2,0.5,Type);

    arguments
        Mat                	            % Input array
        Radius
        Type             = [];               
        UseMex logical   = true;      	% True: Use MEX implementation, False: Use MATLAB implementaion
        UseMP logical    = true;      	% True: Use threading with OpenMP multi-threading library
    end

    % MATLAB implementation
    if UseMex
        % MEX implementation
        % Call function according to input data type   

        if isempty(Type)
            Type = class(Mat);
        end

        switch Type
            case {'single'}
                Result = tools.array.mex.mex_onesCondition_single(Mat, Radius, UseMP);
            case {'double'}
                Result = tools.array.mex.mex_onesCondition_double(Mat, Radius, UseMP);
            otherwise
                error('tools.array.onesCondition - Unsupported data type');
        end
    else
        Result = ones(size(Mat), Type);
        Result(Mat>Radius) = 0;       
    end
end
