function [Result, BestFitPSF]=fitFunPSF(PSF, Args)
    % Fit a composite function to a PSF stamp.
    %   The fitted function is any combination of imUtil.kernel2 like
    %   functions. The function center is not fitted, and the free
    %   parameters are the normalization of each function, followed by the
    %   function parameters.
    % Input  : - A PSF stamp.
    %          * ...,key,val,...
    %            'Funs' - A cell array of functions to fit.
    %                   Each function in the cell is of the form:
    %                   PSF = Fun(Pars, SizeXY, PosXY), where PosXY=[]
    %                   return the stamp center.
    %                   Default is {@imUtil.kernel2.gauss}
    %            'Par0' - A cell array of initial (guess) parameters for
    %                   each one of the functions in 'Funs'.
    %                   Default is {[2 2 0]}.
    %            'Norm0' - A vector of normalizations, one per each function in
    %                   'Funs'. Default is [1].
    %            'PosXY' - The position of the functions center.
    %                   If empty, use stamp center.
    %                   Default is [].
    %            'LB' - Lower bound for all free parameters in the order:
    %                   [NormFun1, ParsFun1, NormFun2, ParsFun2,...]
    %                   Default is [].
    %            'UB' - Like 'LB', but for the upper bounds.
    %                   Default is [].
    % Output : - A structure with the following fields:
    %            .Par - Best fitted parameters.
    %            .ResNorm - RMS of best fit.
    %            .Resid - Observed - Calculated residuals (note that lsqcurve
    %               returns the calc-obs).
    %            .ExitFlag - Exit flag of lsqcurvefit
    %            .Output - Additional output of lsqcurvefit
    %            .J - Jacobian.
    %          - Best fitted PSF stamp.
    % Author : Eran Ofek (Jun 2023)
    % Example: P1 = imUtil.kernel2.gauss([1.5 1.5 0.1]);
    %          R = imUtil.psf.fitFunPSF(P1);
    %          P2 = imUtil.kernel2.lorentzian(1);
    %          R = imUtil.psf.fitFunPSF(P2);
    %          P = 0.9.*P1 + 0.1.*P2;
    %          R = imUtil.psf.fitFunPSF(P);
    %          [R,BF] = imUtil.psf.fitFunPSF(P, 'Funs',{@imUtil.kernel2.gauss, @imUtil.kernel2.lorentzian}, 'Par0',{[2 2 0],[1]}, 'Norm0',[1 1]);
    
    arguments
        PSF
        Args.Funs      = {@imUtil.kernel2.gauss};
        Args.Par0      = {[2 2 0]};
        Args.Norm0     = [1];
        Args.PosXY     = [];
        Args.LB        = [];
        Args.UB        = [];
    end
    
    SizeXY = fliplr(size(PSF));

    if isempty(Args.PosXY)
        Args.PosXY = ceil(SizeXY.*0.5);
    end
    
    if isa(Args.Funs, 'function_handle')
        Args.Funs = {Args.Funs};
    end
    
    Nfun  = numel(Args.Funs);
    
    X0 = [];
    for IfunP=1:1:Nfun
        X0 = [X0, [Args.Norm0(IfunP), Args.Par0{IfunP}]];
    end
    Options = optimoptions('lsqcurvefit');
    Options.Display = 'off';
    [Result.Par, Result.ResNorm, Result.Resid, Result.ExitFlag, Result.Output, ~, Result.J] = lsqcurvefit(@FittedFun, X0, [], PSF, Args.LB, Args.UB, Options);
    BestFitPSF = FittedFun(Result.Par, []);
    Result.Resid      = -Result.Resid;
    
    function Fun=FittedFun(Pars, Xdata)
        % Internal function to generate the fitted PSF composite
        
        Fun  = zeros(SizeXY(2), SizeXY(1));
        IndP = 0;
        for Ifun=1:1:Nfun
            IndP = IndP + (1:1:(1 + numel(Args.Par0{Ifun})));
            Fun = Fun + Pars(IndP(1)) .* Args.Funs{Ifun}(Pars(IndP(2:end)) ,SizeXY, Args.PosXY);
        
            IndP = IndP(end);
        end
        
    end
    
end

