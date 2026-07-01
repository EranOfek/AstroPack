function [Result, BestFitPSF] = fitFunPSF(PSF, Args)
    % Fit a composite function to a PSF stamp.
    %
    % The fitted model is:
    %
    %   Model = sum_i Amp_i .* Fun_i(Par_i, SizeXY, PosXY)
    %
    % Input:
    %   PSF
    %   ...,key,val,...
    %     'Funs'         - Function handle or cell array of function handles.
    %                      Each function has the form:
    %                      Kernel = Fun(Pars, SizeXY, PosXY)
    %                      Default is {@imUtil.kernel2.gauss}.
    %     'Par0'         - Cell array of initial parameters, one vector per
    %                      function. Default is {[2 2 0]}.
    %     'Norm0'        - Initial amplitudes, one per function.
    %                      Default is [1].
    %     'PosXY'        - Center position [X Y]. If empty, use stamp center.
    %                      Default is [].
    %     'LB'           - Lower bounds in full parameter order:
    %                      [Norm1, Par1, Norm2, Par2, ...]
    %                      Default is [].
    %     'UB'           - Upper bounds in full parameter order:
    %                      [Norm1, Par1, Norm2, Par2, ...]
    %                      Default is [].
    %     'LsqOptions'   - Options for lsqnonlin.
    %     'FitAmpLinear' - If false, fit amplitudes and shape parameters
    %                      together using lsqnonlin.
    %                      If true, fit only shape parameters nonlinearly and
    %                      solve amplitudes linearly at each iteration.
    %                      Default is false.
    %
    % Output:
    %   Result
    %     .Par        - Best-fit full parameter vector:
    %                   [Norm1, Par1, Norm2, Par2, ...]
    %     .Amp        - Best-fit amplitudes.
    %     .BestFit    - Structure array, one element per fitted function:
    %                   .Fun
    %                   .Amp
    %                   .Par
    %                   .Kernel
    %                   .Model
    %     .ResNorm    - Sum of squared residuals.
    %     .RMS        - sqrt(ResNorm / Npix)
    %     .Sigma      - sqrt(ResNorm / max(Npix - Nfree, 1))
    %     .Resid      - PSF - BestFitPSF residual image.
    %     .ExitFlag   - Exit flag of lsqnonlin.
    %     .Output     - lsqnonlin output structure.
    %     .J          - Jacobian returned by lsqnonlin.
    %
    %   BestFitPSF    - Best-fit PSF stamp.
    % Author : Eran Ofek + ChatGPT (Jun 2023)
    % Example: P1 = imUtil.kernel2.gauss([1.5 1.5 0.1]);
    %          R = imUtil.psf.fitFunPSF(P1);
    %          P2 = imUtil.kernel2.lorentzian(1);
    %          R = imUtil.psf.fitFunPSF(P2);
    %          P = 0.9.*P1 + 0.1.*P2;
    %          R = imUtil.psf.fitFunPSF(P);
    %          [R,BF] = imUtil.psf.fitFunPSF(P, 'Funs',{@imUtil.kernel2.gauss, @imUtil.kernel2.lorentzian}, 'Par0',{[2 2 0],[1]}, 'Norm0',[1 1]);

    arguments
        PSF {mustBeNumeric, mustBeReal}
        Args.Funs = {@imUtil.kernel2.gauss}
        Args.Par0 = {[2 2 0]}
        Args.Norm0 = [1]
        Args.PosXY = []
        Args.LB = []
        Args.UB = []
        Args.LsqOptions = optimoptions('lsqnonlin')
        Args.FitAmpLinear (1,1) logical = false
    end

    %--------------------------------------------------------------
    % Basic setup
    %--------------------------------------------------------------
    if ~ismatrix(PSF)
        error('PSF must be a 2D matrix.');
    end

    PSF = double(PSF);

    SizeXY = fliplr(size(PSF));   % [Xsize, Ysize]

    if isempty(Args.PosXY)
        % Better than ceil(SizeXY.*0.5), especially for even-sized stamps.
        Args.PosXY = (SizeXY + 1).*0.5;
    end

    if isa(Args.Funs, 'function_handle')
        Args.Funs = {Args.Funs};
    end

    Nfun = numel(Args.Funs);

    if numel(Args.Par0) ~= Nfun
        error('Args.Par0 must contain one parameter vector per function.');
    end

    if numel(Args.Norm0) ~= Nfun
        error('Args.Norm0 must contain one normalization per function.');
    end

    % Good pixels only. This makes the function robust to NaNs/Infs in PSF.
    FlagGood = isfinite(PSF);
    if ~any(FlagGood(:))
        error('PSF contains no finite pixels.');
    end

    PSFvec = PSF(FlagGood);

    %--------------------------------------------------------------
    % Build full initial vector and parameter bookkeeping
    % Full order is:
    %   [Norm1, Par1, Norm2, Par2, ...]
    %--------------------------------------------------------------
    X0Full = [];
    IndNorm = zeros(1, Nfun);
    IndParCell = cell(1, Nfun);

    IndStart = 1;

    for Ifun = 1:Nfun
        Par0i = Args.Par0{Ifun};

        if ~isnumeric(Par0i) || ~isvector(Par0i)
            error('Each Args.Par0 entry must be a numeric vector.');
        end

        NparFun = numel(Par0i);

        IndNorm(Ifun) = IndStart;
        IndParCell{Ifun} = (IndStart + 1):(IndStart + NparFun);

        X0Full = [X0Full, Args.Norm0(Ifun), Par0i(:).']; %#ok<AGROW>

        IndStart = IndStart + 1 + NparFun;
    end

    NparFull = numel(X0Full);

    %--------------------------------------------------------------
    % Bounds
    %--------------------------------------------------------------
    if isempty(Args.LB)
        LBFull = -inf(size(X0Full));
    else
        LBFull = Args.LB(:).';
        if numel(LBFull) ~= NparFull
            error('Args.LB must have the same length as the full parameter vector.');
        end
    end

    if isempty(Args.UB)
        UBFull = inf(size(X0Full));
    else
        UBFull = Args.UB(:).';
        if numel(UBFull) ~= NparFull
            error('Args.UB must have the same length as the full parameter vector.');
        end
    end

    if any(LBFull > UBFull)
        error('Some lower bounds are larger than upper bounds.');
    end

    if any(X0Full < LBFull) || any(X0Full > UBFull)
        error('Initial parameter vector X0 is outside the supplied bounds.');
    end

    Args.LsqOptions.Display = 'off';

    %--------------------------------------------------------------
    % Fit
    %--------------------------------------------------------------
    if Args.FitAmpLinear
        % Fit only the nonlinear shape parameters.
        IndShape = setdiff(1:NparFull, IndNorm);

        X0 = X0Full(IndShape);
        LB = LBFull(IndShape);
        UB = UBFull(IndShape);

        AmpLB = LBFull(IndNorm);
        AmpUB = UBFull(IndNorm);

        [Xbest, ResNorm, ResidVecModelMinusData, ExitFlag, Output, ~, J] = ...
            lsqnonlin(@residualFunLinearAmp, X0, LB, UB, Args.LsqOptions);

        [BestFitPSF, AmpBest, ParFullBest] = modelFromShapePars(Xbest);

        Nfree = numel(Xbest);

    else
        % Fit amplitudes and shape parameters together.
        X0 = X0Full;
        LB = LBFull;
        UB = UBFull;

        [ParFullBest, ResNorm, ResidVecModelMinusData, ExitFlag, Output, ~, J] = ...
            lsqnonlin(@residualFunFull, X0, LB, UB, Args.LsqOptions);

        BestFitPSF = modelFromFullPars(ParFullBest);
        AmpBest = ParFullBest(IndNorm);

        Nfree = numel(ParFullBest);
    end

    %--------------------------------------------------------------
    % Output
    %--------------------------------------------------------------
    Result.Par      = ParFullBest;
    Result.Amp      = AmpBest;
    Result.BestFit  = fullParsToBestFitStruct(ParFullBest);
    Result.ResNorm  = ResNorm;
    Result.ExitFlag = ExitFlag;
    Result.Output   = Output;
    Result.J        = J;

    % lsqnonlin residual is Model - Data.
    % User-facing image residual is Data - Model.
    Result.Resid = PSF - BestFitPSF;

    Npix = numel(PSFvec);

    Result.RMS = sqrt(Result.ResNorm ./ Npix);
    Result.Sigma = sqrt(Result.ResNorm ./ max(Npix - Nfree, 1));

    % Raw vector residual returned by lsqnonlin convention.
    Result.ResidVecModelMinusData = ResidVecModelMinusData;

    %==============================================================
    % Nested functions
    %==============================================================

    function Resid = residualFunFull(ParsFull)
        Model = modelFromFullPars(ParsFull);
        Resid = Model(FlagGood) - PSFvec;
    end

    function Resid = residualFunLinearAmp(ShapePars)
        Model = modelFromShapePars(ShapePars);
        Resid = Model(FlagGood) - PSFvec;
    end

    function Model = modelFromFullPars(ParsFull)
        Model = zeros(size(PSF));

        for Jfun = 1:Nfun
            Amp = ParsFull(IndNorm(Jfun));
            Par = ParsFull(IndParCell{Jfun});

            Kernel = Args.Funs{Jfun}(Par, SizeXY, Args.PosXY);

            if ~isequal(size(Kernel), size(PSF))
                error('Function %d returned a kernel with incorrect size.', Jfun);
            end

            Model = Model + Amp .* Kernel;
        end
    end

    function [Model, Amp, ParsFull] = modelFromShapePars(ShapePars)
        % Insert nonlinear shape parameters into full parameter vector.
        ParsFull = X0Full;

        IndShapeLocal = setdiff(1:NparFull, IndNorm);
        ParsFull(IndShapeLocal) = ShapePars;

        % Build design matrix of kernels for good pixels.
        A = zeros(numel(PSFvec), Nfun);

        KernelCell = cell(1, Nfun);

        for Jfun = 1:Nfun
            Par = ParsFull(IndParCell{Jfun});
            Kernel = Args.Funs{Jfun}(Par, SizeXY, Args.PosXY);

            if ~isequal(size(Kernel), size(PSF))
                error('Function %d returned a kernel with incorrect size.', Jfun);
            end

            KernelCell{Jfun} = Kernel;
            A(:, Jfun) = Kernel(FlagGood);
        end

        % Solve amplitudes linearly.
        Amp = solveAmplitudes(A, PSFvec, AmpLB, AmpUB);

        % Insert amplitudes back into full vector.
        ParsFull(IndNorm) = Amp;

        % Build model image.
        Model = zeros(size(PSF));

        for Jfun = 1:Nfun
            Model = Model + Amp(Jfun) .* KernelCell{Jfun};
        end
    end

    function Amp = solveAmplitudes(A, B, AmpLB, AmpUB)
        % Solve min ||A*Amp - B||^2 subject to optional amplitude bounds.

        HasFiniteLB = any(isfinite(AmpLB));
        HasFiniteUB = any(isfinite(AmpUB));

        if ~HasFiniteLB && ~HasFiniteUB
            % Fully unconstrained linear least squares.
            Amp = A \ B;

        elseif all(AmpLB == 0) && ~HasFiniteUB
            % Common PSF case: non-negative amplitudes.
            Amp = lsqnonneg(A, B);

        else
            % General bounded linear least squares.
            % Requires lsqlin from Optimization Toolbox.
            if exist('lsqlin', 'file') ~= 2
                error(['FitAmpLinear=true with general finite amplitude bounds ', ...
                       'requires lsqlin. Use unconstrained amplitudes, ', ...
                       'non-negative amplitudes, or set FitAmpLinear=false.']);
            end

            LsqlinOptions = optimoptions('lsqlin', 'Display', 'off');

            Amp = lsqlin(A, B, [], [], [], [], AmpLB(:), AmpUB(:), [], LsqlinOptions);
        end

        Amp = Amp(:).';
    end

    function BestFit = fullParsToBestFitStruct(ParsFull)
        % Return a structure array, one element per fitted function.

        BestFit = struct( ...
            'Fun',    cell(1, Nfun), ...
            'Amp',    cell(1, Nfun), ...
            'Par',    cell(1, Nfun), ...
            'Kernel', cell(1, Nfun), ...
            'Model',  cell(1, Nfun));

        for Jfun = 1:Nfun
            Amp = ParsFull(IndNorm(Jfun));
            Par = ParsFull(IndParCell{Jfun});

            Kernel = Args.Funs{Jfun}(Par, SizeXY, Args.PosXY);

            if ~isequal(size(Kernel), size(PSF))
                error('Function %d returned a kernel with incorrect size.', Jfun);
            end

            BestFit(Jfun).Fun    = Args.Funs{Jfun};
            BestFit(Jfun).Amp    = Amp;
            BestFit(Jfun).Par    = Par;
            BestFit(Jfun).Kernel = Kernel;
            BestFit(Jfun).Model  = Amp .* Kernel;
        end
    end

end



%% OLD

% function [Result, BestFitPSF]=fitFunPSF(PSF, Args)
%     % Fit a composite function to a PSF stamp.
%     %   The fitted function is any combination of imUtil.kernel2 like
%     %   functions. The function center is not fitted, and the free
%     %   parameters are the normalization of each function, followed by the
%     %   function parameters.
%     % Input  : - A PSF stamp.
%     %          * ...,key,val,...
%     %            'Funs' - A cell array of functions to fit.
%     %                   Each function in the cell is of the form:
%     %                   PSF = Fun(Pars, SizeXY, PosXY), where PosXY=[]
%     %                   return the stamp center.
%     %                   Default is {@imUtil.kernel2.gauss}
%     %            'Par0' - A cell array of initial (guess) parameters for
%     %                   each one of the functions in 'Funs'.
%     %                   Default is {[2 2 0]}.
%     %            'Norm0' - A vector of normalizations, one per each function in
%     %                   'Funs'. Default is [1].
%     %            'PosXY' - The position of the functions center.
%     %                   If empty, use stamp center.
%     %                   Default is [].
%     %            'LB' - Lower bound for all free parameters in the order:
%     %                   [NormFun1, ParsFun1, NormFun2, ParsFun2,...]
%     %                   Default is [].
%     %            'UB' - Like 'LB', but for the upper bounds.
%     %                   Default is [].
%     % Output : - A structure with the following fields:
%     %            .Par - Best fitted parameters.
%     %            .ResNorm - Sum of squares residuals of best fit.
%     %            .Resid - Observed - Calculated residuals (note that lsqcurve
%     %               returns the calc-obs).
%     %            .ExitFlag - Exit flag of lsqcurvefit
%     %            .Output - Additional output of lsqcurvefit
%     %            .J - Jacobian.
%     %          - Best fitted PSF stamp.
%     % Author : Eran Ofek (Jun 2023)
%     % Example: P1 = imUtil.kernel2.gauss([1.5 1.5 0.1]);
%     %          R = imUtil.psf.fitFunPSF(P1);
%     %          P2 = imUtil.kernel2.lorentzian(1);
%     %          R = imUtil.psf.fitFunPSF(P2);
%     %          P = 0.9.*P1 + 0.1.*P2;
%     %          R = imUtil.psf.fitFunPSF(P);
%     %          [R,BF] = imUtil.psf.fitFunPSF(P, 'Funs',{@imUtil.kernel2.gauss, @imUtil.kernel2.lorentzian}, 'Par0',{[2 2 0],[1]}, 'Norm0',[1 1]);
% 
%     arguments
%         PSF
%         Args.Funs      = {@imUtil.kernel2.gauss};
%         Args.Par0      = {[2 2 0]};
%         Args.Norm0     = [1];
%         Args.PosXY     = [];
%         Args.LB        = [];
%         Args.UB        = [];
%         Args.LsqOptions = optimoptions('lsqcurvefit');
%     end
% 
%     SizeXY = fliplr(size(PSF));
% 
%     if isempty(Args.PosXY)
%         Args.PosXY = ceil(SizeXY.*0.5);
%     end
% 
%     if isa(Args.Funs, 'function_handle')
%         Args.Funs = {Args.Funs};
%     end
% 
%     Nfun  = numel(Args.Funs);
% 
%     X0 = [];
%     for IfunP=1:1:Nfun
%         X0 = [X0, [Args.Norm0(IfunP), Args.Par0{IfunP}]];
%     end    
%     Args.LsqOptions.Display = 'off';
%     [Result.Par, Result.ResNorm, Result.Resid, Result.ExitFlag, Result.Output, ~, Result.J] = lsqcurvefit(@FittedFun, X0, [], PSF, Args.LB, Args.UB, Args.LsqOptions);
%     BestFitPSF = FittedFun(Result.Par, []);
%     Result.Resid      = -Result.Resid;
% 
%     function Fun=FittedFun(Pars, Xdata)
%         % Internal function to generate the fitted PSF composite
% 
%         Fun  = zeros(SizeXY(2), SizeXY(1));
%         IndP = 0;
%         for Ifun=1:1:Nfun
%             IndP = IndP + (1:1:(1 + numel(Args.Par0{Ifun})));
%             Fun = Fun + Pars(IndP(1)) .* Args.Funs{Ifun}(Pars(IndP(2:end)) ,SizeXY, Args.PosXY);
% 
%             IndP = IndP(end);
%         end
% 
%     end
% 
% end
% 
