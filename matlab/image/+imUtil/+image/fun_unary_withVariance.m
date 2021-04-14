function [Result,ResultVar,FlagBad,FunH]=fun_unary_withVariance(Operator, Mat, Var, Args)
% Applay an operator on an array and its variance.
% Input  : - Operator handle (e.g., @sin).
%          - An array in which to apply the operator.
%          - A variance array or scalar of the array. Default is [].
%          * ...,key,val,...
%            'OpArgs' - A cell array of additional parameters to pass to the function
%                   operator. Default is {}.
%            'CCDSEC' - A CCDSEC [Xmin, Xmax, Ymin, Ymax] section on which
%                   to excute the operator. If empty, use entire image.
%                   Default is [].
%            'OutOnlyCCDSEC' - INdicating if to return the full image or
%                   only the CCDSEC region. If false, note that the variance
%                   will be calculated on the entire image.
%                   Default is true.
%            'PropagateErr' - A logical indicating if to propagate the
%                   errors. Default is true.
%            'OperateOnVar' -  If PropagateErr=false, this is a logical
%                   indicating if to operate the operator on the variance matrix.
%                   Default is true.
% Output : - The result of applying the operator to the matrix.
%          - The result of applying the operator to the variance.
%          - A matrix of logicals indicating if a resulted matrix value is
%            NaN or Inf, or the resulted Variance is NaN.
%          - The function handle for the derivative function (only if was
%            found symbolically).
% Author : Eran Ofek (Mar 2021)
% Example: [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@sin, randn(5,5), rand(5,5).*0.01)
%          [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@mean, randn(2,2), rand(2,2).*0.01)
%          [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@mean, randn(2,2), rand(2,2).*0.01,'OpArg',{'all'})
%          [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@tanh, randn(2,2), rand(2,2).*0.01);
%          [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@sin, randn(5,5), rand(5,5).*0.01,'CCDSEC',[1 2 1 2],'OutOnlyCCDSEC',true)
%          [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@sin, randn(5,5), rand(5,5).*0.01,'CCDSEC',[1 2 1 2],'OutOnlyCCDSEC',false)

    arguments
        Operator 
        Mat          
        Var                             = [];
        Args.OpArgs cell                = {};
        Args.CCDSEC                     = [];
        Args.OutOnlyCCDSEC(1,1) logical = true;
        Args.PropagateErr(1,1) logical  = true;
        Args.OperateOnVar(1,1) logical  = true;  % only if PropagateErr=false
    end
    
    FunH      = [];
    if isempty(Args.CCDSEC)
        Result    = Operator(Mat, Args.OpArgs{:});
    else
        if Args.OutOnlyCCDSEC
            Mat = Mat(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
            Var = Var(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
            Result    = Operator(Mat, Args.OpArgs{:});
        else
            % embed the answer in the full image
            % var is operating on the entire image
            Result = Mat;
            Result(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)) = Operator(Mat(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), Args.OpArgs{:});
        end
    end
    
    if ~Args.PropagateErr
        % Variance is not provided
        if Args.OperateOnVar
            if isempty(Args.CCDSEC)
                ResultVar    = Operator(Var, Args.OpArgs{:});
            else
                if Args.OutOnlyCCDSEC
                    Var = Var(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
                    ResultVar    = Operator(Var, Args.OpArgs{:});
                else
                    % embed the answer in the full image
                    % var is operating on the entire image
                    ResultVar = Var;
                    ResultVar(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)) = Operator(Var(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), Args.OpArgs{:});
                end
            end
            
        else
            ResultVar = Var;
        end
        FlagBad   = [];
    else
        if isempty(Var)
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
                        ResultVar = Operator(Var, Args.OpArgs{:});
                    end
                case 'mean'
                    if numel(Var)==1
                        ResultVar = Var;
                    else
                        ResultVar = Operator(Var, Args.OpArgs{:})./numel(Mat);
                    end

                otherwise
                    % Unknown unary operator option
                    % attempt to use symbolic math
                    syms x;
                    SymFun = eval(sprintf('diff(%s(x))', func2str(Operator)));
                    if SymFun==1
                        error('Function derivative could not be found symbolically : Consider 1. adding the function; 2. using the PropagateErr and OperateOnVar arguments');
                    end
                    FunH   = matlabFunction(SymFun);
                    ResultVar = Var.*FunH(Mat);
                    warning('The variance was propagated using symbolic math - For speed consider adding this function to the list of built in functions');
            end

            if nargout>2
                %FlagBad = isinf(Mat) | isnan(Mat) | isnan(Var);
                FlagBad = isnan(ResultVar) | isinf(Result) | isnan(Result);
            end
        end
    end
    
end

