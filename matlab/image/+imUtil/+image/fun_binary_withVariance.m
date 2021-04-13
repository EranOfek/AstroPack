function [Result,ResultVar,FlagBad]=fun_binary_withVariance(Operator, Mat1, Mat2, Var1, Var2, Cov12, OpArgs)
% Applay an operator on an array and its variance.
% Input  : - Operator handle (e.g., @sin).
%          - First array to apply to the left of the operator.
%          - Second array to apply to the right of the operator.
%          - The first variance array. Default is [].
%          - The second variane array. Default is [].
%            If both variances are [], then the errors will not be
%            propagated.
%          - The covariance matrix between the two arrays.
%            Default is 0.
%          - A cell array of additional parameters to pass to the function
%            operator. Default is {}.
% Output : - The result of applying the operator to the matrix.
%          - The result of applying the operator to the variance.
%          - A matrix of logicals indicating if a resulted matrix value is
%            NaN or Inf, or the resulted Variance is NaN.
% Author : Eran Ofek (Mar 2021)
% Example:
% [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@plus, randn(2,2), randn(2,2));
% [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@plus, randn(2,2), randn(2,2),[],[]);
% [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@times, randn(2,2), randn(2,2), 0.01, 0.01)
% [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@power, randn(2,2), randn(2,2), 0.01, 0.01)

    arguments
        Operator 
        Mat1          {mustBeNumeric(Mat1)}
        Mat2          {mustBeNumeric(Mat2)}
        Var1          {mustBeNumeric(Var1)}  = [];
        Var2          {mustBeNumeric(Var2)}  = [];
        Cov12         {mustBeNumeric(Cov12)} = 0;
        OpArgs cell                           = {};
    end
    
    %FunH      = [];
    
    Result    = Operator(Mat1, Mat2, OpArgs{:});
    
    if (isempty(Var1) && isempty(Var2))
        % Variance is not provided
        ResultVar = [];
        FlagBad   = [];
    else
        % we already checked that both Var1 and Var2 are not empty
        if isempty(Var1)
            Var1 = 0;
        end
        if isempty(Var2)
            Var2 = 0;
        end
            
        switch func2str(Operator)
            case 'plus'
                % addition
                ResultVar = Var1 + Var2 + 2.*Cov12;
            case 'minus'
                % subtraction
                ResultVar = Var1 + Var2 - 2.*Cov12;
            case 'times'
                % multiplication
                if numel(Var1)==1 && Var1==0
                    Tmp1 = 0;
                else
                    Tmp1 = Var1./(Mat1.^2);
                end
                if numel(Var2)==1 && Var2==0
                    Tmp2 = 0;
                else
                    Tmp2 = Var2./(Mat2.^2);
                end
                if numel(Cov12)==1 && Cov12==0
                    TmpC = 0;
                else
                    TmpC = 2.*Cov./Result;
                end
                ResultVar = Result.^2 .* (Tmp1 + Tmp2 + TmpC);
            case 'rdivide'
                % division
                if numel(Var1)==1 && Var1==0
                    Tmp1 = 0;
                else
                    Tmp1 = Var1./(Mat1.^2);
                end
                if numel(Var2)==1 && Var2==0
                    Tmp2 = 0;
                else
                    Tmp2 = Var2./(Mat2.^2);
                end
                if numel(Cov12)==1 && Cov12==0
                    TmpC = 0;
                else
                    TmpC = 2.*Cov./Result;
                end
                ResultVar = Result.^2 .* (Tmp1 + Tmp2 - TmpC);
            case 'power'
                % power Mat1.^Mat2
                if numel(Var1)==1 && Var1==0
                    Tmp1 = 0;
                else
                    Tmp1 = (Mat2./Mat1).^2 .* Var1;
                end
                if numel(Var2)==1 && Var2==0
                    Tmp2 = 0;
                else
                    Tmp2 = log(Mat1).^2 .* Var2;
                end
                if numel(Cov12)==1 && Cov12==0
                    TmpC = 0;
                else
                    TmpC = 2.*Mat2.*log(Mat1).*sqrt(Cov12)./Mat1;
                end
                ResultVar = Result.^2 .* (Tmp1 + Tmp2 + TmpC);
            otherwise
                % Unknown binary operator option
                error('Unknown operator - can not propagate errors');
        end
        
        if nargout>2
            FlagBad = isnan(ResultVar) | isinf(Result) | isnan(Result);
        end
    end
    
end

