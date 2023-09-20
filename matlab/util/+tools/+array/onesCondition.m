function Result = onesCondition(Mat, Radius, Type, UseMex, UseMP)
% Create a logical matrix in which all values above a threshold are 0, and all
% values below are 1.
% Input  : - 2D/3D matrix of values [Mat].
%          - Threshold value [Radius].
%          - Datatype - single or double [Type].
%          - (UseMex) A logical indicating if to use mex version.
%            Default is true.
%          - (UseMP) A logical indicating if to use open MP.
%            Default is true.
% Output : - 2D/3D logical matrix.
% Author : Dan Elhanati (September 2023)
% Example: UseMex = 0;
%          UseMP = 0;
%          MatR2 = rand(100,100,'double');
%          MomRadius2 = 0.5;
%          Type = 'double';
%          res = tools.array.onesCondition(MatR2,MomRadius2,Type,UseMex,UseMP);
%----------------------------------------------------------------------
    arguments
        Mat;              	% Input array
        Radius;
        Type;              %        
        UseMex = true;     	% True: Use MEX implementation, False: Use MATLAB implementaion
        UseMP = true;      	% True: Use threading with OpenMP multi-threading library
    end

    % MATLAB implementation
    if ~UseMex
        Result = ones(size(Mat), Type);
        Result(Mat>Radius) = 0;
        return;
    end
    
    % MEX implementation
    % Call function according to input data type   
    switch Type
        case {'single'}
            Result = tools.array.mex.mex_onesCondition_single(Mat, Radius, UseMP);
        case {'double'}
            Result = tools.array.mex.mex_onesCondition_double(Mat, Radius, UseMP);
        otherwise
            error('tools.array.onesCondition - Unsupported data type');
    end
end
