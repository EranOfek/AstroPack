function [Result,ResultVar,FlagBad]=fun_unary_withVariance(Operator, Mat, Var, OpArg)
% Applay an operator on an array and its variance.
% Input  : - Operator handle (e.g., @sin).
%          - An array in which to apply the operator.
%          - A variance array or scalar of the array. Default is [].
%          - A cell array of additional parameters to pass to the function
%            operator. Default is {}.
% Output : - The result of applying the operator to the matrix.
%          - The result of applying the operator to the variance.
%          - A matrix of logicals indicating if a resulted matrix value is
%            NaN or Inf, or the resulted Variance is NaN.
% Author : Eran Ofek (Mar 2021)
% Example: [Result,ResultVar,Flag]=imUtil.image.fun_unary_withVariance(@sin, randn(5,5), rand(5,5).*0.01)

    arguments
        Operator 
        Mat          {mustBeNumeric(Mat)}
        Var          {mustBeNumeric(Var)} = [];
        OpArg cell                        = {};
    end
    
    Result    = Operator(Mat, OpArg{:});
    if isempty(Var)
        % Variance is not provided
        ResultVar = [];
        FlagBad   = [];
    else
        switch func2str(Operator)
            case 'sin'
                ResultVar = Var.*cos(Mat).^2;
            case 'cos'
                ResultVar = Var.*sin(Mat).^2;
            case 'tan'
                ResultVar = Var.*sec(Mat).^2;
            case 'log'
                ResultVar = Var./(Mat.^2);
            case 'log10'
                ResultVar = Var./((log(10).*Mat).^2);
            case 'sum'
                if numel(Var)==1
                    ResultVar = numel(Mat).*Var;
                else
                    ResultVar = Operator(Var, OpArg{:});
                end
            case 'mean'
                if numel(Var)==1
                    ResultVar = Var;
                else
                    ResultVar = Operator(Var, OpArg{:})./numel(Mat);
                end
            
            otherwise
                error('Unknown unary operator option');
                % attempt to use symbolic math
                % FFU
                % syms x
                % eval(sprintf('diff(%s(x))', func2str(Operator)
                % use subs
        end
        FlagBad = isinf(Mat) | isnan(Mat) | isnan(Var);
    end
    
end

