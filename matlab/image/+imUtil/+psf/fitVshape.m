function [FitPar] = fitVshape(X, Y, Args)
    % Fit a V-shape function with two free parameters: MinX, MinY
    %     Assume SlopeDec, SlopeInc are known.
    %     The v-shape function is given by tools.math.fun.vShape
    % Input  : - X
    %          - Y
    %          * ...,key,val,... 
    %            'SlopeDec' - Slope of decreasing branch. Default is 1.
    %            'SlopeInc' - Slope of increasing branch. Default is -1.
    % Output : - Best fitted parameters: MinX, MinY
    % Author : Eran Ofek (2024 Nov) 
    % Example: [ModelY, X] = tools.math.fun.vShape((-100:20:100), 0, 3, -1, +1)
    %          FP=imUtil.psf.fitVshape(X, ModelY+0.2.*randn(size(X)))

    arguments
        X   % foc position
        Y   % FWHM
        Args.SlopeDec          = -1
        Args.SlopeInc          = +1;
    end
    
    % optimize for two free parameters MinX, MinY:
    %[ModelY, X] = tools.math.fun.vShape(X, MinX, MinY, Args.SlopeDec, Args.SlopeInc);

    FitFun = @(Par, X) tools.math.fun.vShape(X, Par(1), Par(2), Args.SlopeDec, Args.SlopeInc);
    
    % find best guess
    [MY,I]   = min(Y);
    GuessPar = [X(I), 0.5.*MY];
    
    FitPar = lsqcurvefit(FitFun, GuessPar, X, Y);

end


