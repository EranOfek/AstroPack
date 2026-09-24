function [Result] = fitPositionalDiff(Diff, X, Y, Args)
    % Fit a positional difference model as a function of X,Y.
    %     Fit a linear model to positional differences using basis functions
    %     of normalized coordinates. Optionally perform iterative sigma
    %     clipping on the normalized residuals. If MaxIter=1, no sigma
    %     clipping is performed.
    % Input  : - Vector of positional differences to fit.
    %          - Vector of X coordinates.
    %          - Vector of Y coordinates.
    %          * ...,key,val,...
    %            'Model' - Cell array of function handles. Each function
    %                   accepts normalized coordinates (Xn,Yn) and returns
    %                   one column of the design matrix.
    %                   Default is {@(x,y) 1, @(x,y) x, @(x,y) y,
    %                   @(x,y) x.*y}.
    %            'Tran2D' - A Tran2D object. Currently not used directly.
    %                   Default is Tran2D('poly1').
    %            'Err' - Errors of the positional differences. Either scalar
    %                   or vector with the same size as Diff. Used as
    %                   1./Err weights in the fit and for sigma clipping of
    %                   Resid./Err. Default is 1.
    %            'CCDSEC' - CCD section [Xmin Xmax Ymin Ymax]. If given,
    %                   coordinates are normalized relative to its center
    %                   and half-range. If empty, no normalization is applied.
    %                   Default is [].
    %            'FitMethod' - Fitting method:
    %                   '\'    - Weighted least squares using 1./Err.
    %                   'lscov' - Weighted least squares using Err.^2 as
    %                            variances.
    %                   Default is '\'.
    %            'SigmaClip' - Two element vector [Lower Upper] giving the
    %                   sigma clipping thresholds. Lower clipping is applied
    %                   to negative normalized residuals and upper clipping
    %                   to positive normalized residuals. Default is [3 3].
    %            'MaxIter' - Maximum number of fit iterations. If MaxIter=1,
    %                   no sigma clipping is performed. Default is 2.
    % Output : - Structure containing the fit results. Fields include:
    %            .Par       - Best-fit parameters.
    %            .ParErr    - Formal parameter errors.
    %            .Cov       - Parameter covariance matrix.
    %            .Pred      - Model prediction for all input points.
    %            .Resid     - Residuals for all input points.
    %            .NormResid - Normalized residuals, Resid./Err.
    %            .Fun       - Anonymous function Fun(X,Y,Par) that evaluates
    %                         the fitted model using original X,Y coordinates.
    %            .RMS       - Standard deviation of residuals for final
    %                         used points.
    %            .StdResid  - Standard deviation of normalized residuals
    %                         for final used points.
    %            .Chi2      - Chi-square for final used points.
    %            .RedChi2   - Reduced chi-square.
    %            .Dof       - Number of degrees of freedom.
    %            .FlagUse   - Logical vector of points used in final fit.
    %            .FlagClip  - Logical vector of rejected points.
    %            .Nobs      - Total number of input observations.
    %            .Nuse      - Number of observations used in final fit.
    %            .Nclip     - Number of clipped observations.
    %            .Iter      - Number of performed iterations.
    %            .Sigma     - Robust sigma used in the final clipping step.
    %            .Median    - Median normalized residual used for clipping.
    % Author : Eran Ofek + ChatGPT (2026 Jul)
    % Example:
    %          Result = imUtil.calib.fitPositionalDiff(Diff, X, Y);
    %          Result = imUtil.calib.fitPositionalDiff(Diff, X, Y, ...
    %                     'CCDSEC',[1 1716 1 1716], ...
    %                     'FitMethod','\', ...
    %                     'SigmaClip',[3 3], ...
    %                     'MaxIter',3);
    %          DiffPred = Result.Fun(Xnew, Ynew, Result.Par);

    arguments
        Diff
        X
        Y
        Args.Model             = {@(x,y) 1, @(x,y) x, @(x,y) y, @(x,y) x.*y};
        Args.Tran2D            = Tran2D('poly1');
        Args.Err               = 1;
        Args.CCDSEC            = []; %[1 1716 1 1716];
        Args.FitMethod         = '\';  % '\'|'lscov'

        Args.SigmaClip         = [3 3];
        Args.MaxIter           = 2;
    end

    % Force column vectors
    Diff = Diff(:);
    X    = X(:);
    Y    = Y(:);

    Nobs = numel(Diff);

    if isscalar(Args.Err)
        Err = Args.Err.*ones(Nobs,1);
    else
        Err = Args.Err(:);
    end

    if numel(X)~=Nobs || numel(Y)~=Nobs || numel(Err)~=Nobs
        error('Diff, X, Y and Err must have the same number of elements');
    end

    if numel(Args.SigmaClip)~=2
        error('SigmaClip must be a two-element vector [Lower Upper]');
    end

    if Args.MaxIter<1
        error('MaxIter must be >= 1');
    end

    if isempty(Args.CCDSEC)
        MidX   = 0;
        RangeX = 1;
        MidY   = 0;
        RangeY = 1;
    else
        MidX   = (Args.CCDSEC(2) + Args.CCDSEC(1)).*0.5;
        RangeX = (Args.CCDSEC(2) - Args.CCDSEC(1)).*0.5;
        MidY   = (Args.CCDSEC(4) + Args.CCDSEC(3)).*0.5;
        RangeY = (Args.CCDSEC(4) - Args.CCDSEC(3)).*0.5;
    end

    % Normalize coordinates
    Xn = (X - MidX)./RangeX;
    Yn = (Y - MidY)./RangeY;

    % Build design matrix
    Npar = numel(Args.Model);

    H = zeros(Nobs, Npar);
    for Ipar=1:1:Npar
        Col = Args.Model{Ipar}(Xn, Yn);

        if isscalar(Col)
            H(:,Ipar) = Col;
        else
            H(:,Ipar) = Col(:);
        end
    end

    % Initial valid points
    FlagUse = isfinite(Diff) & isfinite(Xn) & isfinite(Yn) & ...
              all(isfinite(H),2) & isfinite(Err) & Err>0;

    if sum(FlagUse)<Npar
        error('Number of valid observations is smaller than number of model parameters');
    end

    MedianResid = NaN;
    SigmaResid  = NaN;
    Cov0        = NaN(Npar,Npar);

    for Iiter=1:1:Args.MaxIter

        Huse    = H(FlagUse,:);
        DiffUse = Diff(FlagUse);
        ErrUse  = Err(FlagUse);

        if size(Huse,1)<Npar
            error('Number of used observations is smaller than number of model parameters');
        end

        switch Args.FitMethod
            case '\'
                % Weighted least squares:
                % Minimize sum(((Diff - H*Par)./Err).^2).
                W     = 1./ErrUse;
                Hw    = Huse.*W;
                Diffw = DiffUse.*W;

                Par   = Hw\Diffw;

                % If Err are reliable absolute 1-sigma errors, then
                % inv(Hw.'*Hw) is the formal covariance matrix.
                Cov0  = inv(Hw.'*Hw);

            case 'lscov'
                % ErrUse.^2 are observational variances.
                [Par, ~, ~, Cov0] = lscov(Huse, DiffUse, ErrUse.^2);

            otherwise
                error('Unknown FitMethod option');
        end

        Pred      = H*Par;
        Resid     = Diff - Pred;
        NormResid = Resid./Err;

        % MaxIter=1 means no sigma clipping
        if Args.MaxIter==1
            break;
        end

        % Do not clip after the final requested fit
        if Iiter==Args.MaxIter
            break;
        end

        % Sigma clipping on normalized residuals
        ClipVec = NormResid(FlagUse);
        ClipVec = ClipVec(isfinite(ClipVec));

        MedianResid = median(ClipVec);
        SigmaResid  = 1.4826.*median(abs(ClipVec - MedianResid));

        % Fallback in pathological cases
        if ~isfinite(SigmaResid) || SigmaResid<=0
            SigmaResid = std(ClipVec);
        end

        if ~isfinite(SigmaResid) || SigmaResid<=0
            % Cannot define clipping scale; stop iterations
            break;
        end

        LowerLimit = MedianResid - Args.SigmaClip(1).*SigmaResid;
        UpperLimit = MedianResid + Args.SigmaClip(2).*SigmaResid;

        NewFlagUse = FlagUse & ...
                     isfinite(NormResid) & ...
                     NormResid>=LowerLimit & ...
                     NormResid<=UpperLimit;

        if sum(NewFlagUse)<Npar
            % Do not accept a clipping step that leaves too few points
            break;
        end

        if all(NewFlagUse==FlagUse)
            % Converged
            FlagUse = NewFlagUse;
            break;
        end

        FlagUse = NewFlagUse;
    end

    % Final statistics
    Result.Par       = Par;

    % Anonymous function evaluated using original, non-normalized X,Y.
    % Usage:
    %   DiffPred = Result.Fun(Xnew, Ynew, Result.Par);
    Result.Fun       = @(X,Y,Par) local_eval_model(X, Y, Par, Args.Model, ...
                                                   MidX, RangeX, MidY, RangeY);

    Result.Pred      = Pred;
    Result.Resid     = Resid;
    Result.NormResid = NormResid;

    Result.FlagUse   = FlagUse;
    Result.FlagClip  = ~FlagUse;

    Result.Nobs      = Nobs;
    Result.Nuse      = sum(FlagUse);
    Result.Nclip     = Nobs - Result.Nuse;
    Result.Npar      = Npar;
    Result.Dof       = Result.Nuse - Npar;
    Result.Iter      = Iiter;

    Result.RMS       = std(Result.Resid(Result.FlagUse));
    Result.StdResid  = std(Result.NormResid(Result.FlagUse));

    Result.Chi2      = sum(Result.NormResid(Result.FlagUse).^2);

    if Result.Dof>0
        Result.RedChi2 = Result.Chi2./Result.Dof;
    else
        Result.RedChi2 = NaN;
    end

    % Parameter covariance and errors
    Result.Cov          = Cov0;
    Result.ParErr       = sqrt(diag(Result.Cov));

    % Empirically scaled covariance. Useful when Err are relative weights
    % rather than reliable absolute 1-sigma uncertainties.
    Result.CovScaled    = Cov0.*Result.RedChi2;
    Result.ParErrScaled = sqrt(diag(Result.CovScaled));

    Result.Median    = MedianResid;
    Result.Sigma     = SigmaResid;

    Result.Xn        = Xn;
    Result.Yn        = Yn;
    Result.H         = H;
    Result.Err       = Err;
end


function Pred = local_eval_model(X, Y, Par, Model, MidX, RangeX, MidY, RangeY)
    % Evaluate fitted model using original, non-normalized X,Y coordinates.

    SizeX = size(X);

    X = X(:);
    Y = Y(:);

    if numel(X)~=numel(Y)
        error('X and Y must have the same number of elements');
    end

    Xn = (X - MidX)./RangeX;
    Yn = (Y - MidY)./RangeY;

    Nobs = numel(X);
    Npar = numel(Model);

    if numel(Par)~=Npar
        error('Number of parameters must be equal to number of model terms');
    end

    H = zeros(Nobs, Npar);
    for Ipar=1:1:Npar
        Col = Model{Ipar}(Xn, Yn);

        if isscalar(Col)
            H(:,Ipar) = Col;
        else
            H(:,Ipar) = Col(:);
        end
    end

    Pred = H*Par(:);

    % Return in the same shape as input X
    Pred = reshape(Pred, SizeX);
end