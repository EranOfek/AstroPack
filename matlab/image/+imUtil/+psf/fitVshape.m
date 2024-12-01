function [FitPar,Result] = fitVshape(X, Y, Args)
    % Fit a V-shape function with two free parameters: MinX, MinY
    %     Assume SlopeDec, SlopeInc are known.
    %     The v-shape function is given by tools.math.fun.vShape
    % Input  : - X
    %          - Y
    %          * ...,key,val,... 
    %            'SlopeDec' - Slope of decreasing branch. Default is -0.035856
    %            'SlopeInc' - Slope of increasing branch. Default is +0.035856
    % Output : - Best fitted parameters: MinX, MinY
    % Author : Eran Ofek (2024 Nov) 
    % Example: [ModelY, X] = tools.math.fun.vShape((-100:20:100), 0, 3, -1, +1)
    %          [FP,Res]=imUtil.psf.fitVshape(X, ModelY+0.2.*randn(size(X)))

    arguments
        X   % foc position
        Y   % FWHM
        Args.SlopeDec          = -0.035856;  % pixel/micron
        Args.SlopeInc          = +0.035856;
    end
    
    % optimize for two free parameters MinX, MinY:
    %[ModelY, X] = tools.math.fun.vShape(X, MinX, MinY, Args.SlopeDec, Args.SlopeInc);

    FitFun = @(Par, X) tools.math.fun.vShape(X, Par(1), Par(2), Args.SlopeDec, Args.SlopeInc);
    
    % find best guess
    [MY,I]   = min(Y);
    GuessPar = [X(I), 0.5.*MY];
    
    Options = optimset('Display', 'off');

    FitPar = lsqcurvefit(FitFun, GuessPar, X, Y, [], [], Options);

    Ymodel = FitFun(FitPar,X);
    Result.Resid  = Y - Ymodel;
    Result.Std    = std(Result.Resid);
    Result.RStd   = tools.math.stat.rstd(Result.Resid(:));
    Result.MaxAbsResid = max(abs(Result.Resid));

end


