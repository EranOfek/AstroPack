function [A, B, Result] = fitLineErrXY(X, Y, ErrY, ErrX, Args)
    % Fit a straight line with Gaussian errors in X and Y using full LogLikelihood.
    % Description: Fit Y=A+B.*X using the full Gaussian likelihood,
    %              including uncertainties in both X and Y and the
    %              Gaussian normalization term.
    % Input  : - X - Independent variable.
    %          - Y - Dependent variable.
    %          - ErrY - 1-sigma Gaussian errors in Y. May be a scalar or
    %                   an array with the same number of elements as Y.
    %          - ErrX - 1-sigma Gaussian errors in X. May be a scalar or
    %                   an array with the same number of elements as X.
    %                   Default is 0.
    %          * Arbitrary key,value pairs. Available properties:
    %            'Start' - Initial guess [A, B].
    %                      If empty, use an ordinary least-squares fit.
    %                      Default is [].
    %            'Display' - fminsearch display mode.
    %                        Options: 'off' | 'iter' | 'final' | 'notify'.
    %                        Default is 'off'.
    %            'MaxIter' - Maximum number of minimization iterations.
    %                        Default is 1e4.
    %            'MaxFunEvals' - Maximum number of function evaluations.
    %                            Default is 2e4.
    %            'TolX' - Parameter tolerance for fminsearch.
    %                     Default is 1e-10.
    %            'TolFun' - Function tolerance for fminsearch.
    %                       Default is 1e-10.
    %            'CalcCov' - Calculate the parameter covariance matrix
    %                        from the numerical Hessian of -log(L).
    %                        Default is true.
    % Output : - A - Best-fit intercept.
    %          - B - Best-fit slope.
    %          - Result - Structure containing additional fit information:
    %                     .A
    %                     .B
    %                     .ErrA
    %                     .ErrB
    %                     .Cov
    %                     .LogLike
    %                     .NLL
    %                     .Chi2
    %                     .Chi2Dof
    %                     .Dof
    %                     .Resid
    %                     .ModelY
    %                     .SigmaEff
    %                     .ExitFlag
    %                     .Output
    %                     .Ndata
    % Tested : Matlab R2024a
    % Author : Eran Ofek + ChatGPT (Sep 2026)
    % Example:
    %{
          for I=1:1000,
              X    = (1:100).';
              ErrX = 0; %0.2.*ones(size(X));
              Y    = 3 + 2.*X;
              ErrY = sqrt(Y); % 0.5.*ones(size(X));
              Y    = 3 + 2.*X + ...
                     randn(size(X)).*sqrt(ErrY.^2 + 4.*ErrX.^2);
    
              [A(I),B(I),R(I)] = tools.math.fit.fitLineErrXY(X,Y,ErrY,ErrX);
              Par(I,1:2) = polyfit(X,Y,1);
          end
    %}
    
    arguments
        X
        Y
        ErrY
        ErrX = 0
    
        Args.Start        = []
        Args.Display      = 'off'
        Args.MaxIter      = 1e4
        Args.MaxFunEvals  = 2e4
        Args.TolX         = 1e-10
        Args.TolFun       = 1e-10
        Args.CalcCov      = true
    end
    
    
    %% Input preparation
    X = X(:);
    Y = Y(:);
    
    N = numel(X);
    
    if numel(Y) ~= N
        error('fitLineErrXY:SizeMismatch', ...
            'X and Y must contain the same number of elements');
    end
    
    % Expand scalar errors
    if isscalar(ErrY)
        ErrY = repmat(ErrY, N, 1);
    else
        ErrY = ErrY(:);
    end
    
    if isscalar(ErrX)
        ErrX = repmat(ErrX, N, 1);
    else
        ErrX = ErrX(:);
    end
    
    if numel(ErrY) ~= N || numel(ErrX) ~= N
        error('fitLineErrXY:SizeMismatch', ...
            'ErrX and ErrY must be scalars or have the same size as X');
    end
    
    
    %% Remove NaNs / non-finite values
    Flag = isfinite(X)    & ...
           isfinite(Y)    & ...
           isfinite(ErrX) & ...
           isfinite(ErrY);
    
    X    = X(Flag);
    Y    = Y(Flag);
    ErrX = ErrX(Flag);
    ErrY = ErrY(Flag);
    
    N = numel(X);
    
    if N < 2
        error('fitLineErrXY:TooFewPoints', ...
            'At least two valid data points are required');
    end
    
    if any(ErrX < 0) || any(ErrY <= 0)
        error('fitLineErrXY:BadErrors', ...
            'ErrX must be >=0 and ErrY must be >0');
    end
    
    if all(X == X(1))
        error('fitLineErrXY:NoXRange', ...
            'All X values are identical');
    end
    
    
    %% Scale variables for numerical stability
    X0 = mean(X);
    Y0 = mean(Y);
    
    ScaleX = std(X);
    ScaleY = std(Y);
    
    if ~isfinite(ScaleX) || ScaleX <= 0
        ScaleX = 1;
    end
    
    if ~isfinite(ScaleY) || ScaleY <= 0
        ScaleY = max(median(ErrY), 1);
    end
    
    Xs = (X - X0)./ScaleX;
    Ys = (Y - Y0)./ScaleY;
    
    ErrXs = ErrX./ScaleX;
    ErrYs = ErrY./ScaleY;
    
    
    %% Initial guess
    % Parameterization in the scaled coordinates:
    %
    %   Ys = C + M.*Xs
    %
    % with
    %
    %   B = M.*ScaleY./ScaleX
    %   A = Y0 + ScaleY.*C - B.*X0
    
    if isempty(Args.Start)
    
        P0 = polyfit(Xs, Ys, 1);
    
        M0 = P0(1);
        C0 = P0(2);
    
    else
    
        if numel(Args.Start) ~= 2
            error('fitLineErrXY:BadStart', ...
                'Args.Start must contain [A B]');
        end
    
        A0 = Args.Start(1);
        B0 = Args.Start(2);
    
        M0 = B0.*ScaleX./ScaleY;
        C0 = (A0 + B0.*X0 - Y0)./ScaleY;
    
    end
    
    Par0 = [C0, M0];
    
    
    %% Full negative log likelihood
    %
    % In scaled coordinates:
    %
    %   Var_i = ErrYs_i^2 + M^2 ErrXs_i^2
    %
    % and
    %
    %   -ln(L) = 1/2 sum[ Resid_i^2/Var_i + log(2*pi*Var_i) ]
    %
    
    NLLfun = @(Par) localNLL(Par, Xs, Ys, ErrXs, ErrYs);
    
    
    %% Minimization
    Opt = optimset('Display',     Args.Display, ...
                   'MaxIter',     Args.MaxIter, ...
                   'MaxFunEvals', Args.MaxFunEvals, ...
                   'TolX',        Args.TolX, ...
                   'TolFun',      Args.TolFun);
    
    [Par, NLLscaled, ExitFlag, Output] = fminsearch(NLLfun, Par0, Opt);
    
    
    %% Convert back to original coordinates
    C = Par(1);
    M = Par(2);
    
    B = M.*ScaleY./ScaleX;
    A = Y0 + ScaleY.*C - B.*X0;
    
    
    %% Fit statistics in original units
    ModelY = A + B.*X;
    Resid  = Y - ModelY;
    
    VarEff   = ErrY.^2 + B.^2.*ErrX.^2;
    SigmaEff = sqrt(VarEff);
    
    Chi2 = sum(Resid.^2./VarEff);
    
    Dof = N - 2;
    
    if Dof > 0
        Chi2Dof = Chi2./Dof;
    else
        Chi2Dof = NaN;
    end
    
    NLL = 0.5 .* sum(Resid.^2./VarEff + log(2.*pi.*VarEff));
    
    LogLike = -NLL;
    
    
    %% Parameter covariance matrix
    CovAB = NaN(2,2);
    ErrA  = NaN;
    ErrB  = NaN;
    
    if Args.CalcCov
    
        Hess = localHessian(NLLfun, Par);
    
        % Require a positive-definite Hessian
        [~,FlagPD] = chol(Hess);
    
        if FlagPD == 0
    
            CovScaled = inv(Hess);
    
            % Jacobian:
            %
            % [A]   [ScaleY    -ScaleY*X0/ScaleX] [C]
            % [B] = [0          ScaleY/ScaleX    ] [M]
            %
            Jac = [ScaleY, -ScaleY.*X0./ScaleX; ...
                   0,       ScaleY./ScaleX];
    
            CovAB = Jac * CovScaled * Jac.';
    
            ErrA = sqrt(CovAB(1,1));
            ErrB = sqrt(CovAB(2,2));
    
        end
    end
    
    
    %% Output structure
    Result.A          = A;
    Result.B          = B;
    Result.ErrA       = ErrA;
    Result.ErrB       = ErrB;
    Result.Cov        = CovAB;
    
    Result.LogLike    = LogLike;
    Result.NLL        = NLL;
    Result.Chi2       = Chi2;
    Result.Chi2Dof    = Chi2Dof;
    Result.Dof        = Dof;
    
    Result.Resid      = Resid;
    Result.ModelY     = ModelY;
    Result.SigmaEff   = SigmaEff;
    
    Result.ExitFlag   = ExitFlag;
    Result.Output     = Output;
    Result.Ndata      = N;
    
    Result.ParScaled  = Par;
    Result.NLLscaled  = NLLscaled;
    
    end
    
    
    %==========================================================================
    
    function NLL = localNLL(Par, X, Y, ErrX, ErrY)
    % Full negative Gaussian log-likelihood.
    
    C = Par(1);
    M = Par(2);
    
    Resid = Y - C - M.*X;
    
    Var = ErrY.^2 + M.^2.*ErrX.^2;
    
    if any(~isfinite(Var)) || any(Var <= 0)
        NLL = Inf;
        return
    end
    
    NLL = 0.5 .* sum(Resid.^2./Var + log(2.*pi.*Var));
    
    if ~isfinite(NLL)
        NLL = Inf;
    end

end


%==========================================================================

function H = localHessian(Fun, Par)
% Calculate numerical Hessian for a two-parameter function.

Par = Par(:);

Np = numel(Par);

Step = eps.^(1./4) .* max(abs(Par), 1);

F0 = Fun(Par.');

H = zeros(Np, Np);

% Diagonal terms
for Ipar = 1:Np

    DP = zeros(Np,1);
    DP(Ipar) = Step(Ipar);

    Fp = Fun((Par + DP).');
    Fm = Fun((Par - DP).');

    H(Ipar,Ipar) = (Fp - 2.*F0 + Fm)./(Step(Ipar).^2);

end


% Off-diagonal terms
for Ipar = 1:Np
    for Jpar = (Ipar + 1):Np

        DPI = zeros(Np,1);
        DPJ = zeros(Np,1);

        DPI(Ipar) = Step(Ipar);
        DPJ(Jpar) = Step(Jpar);

        Fpp = Fun((Par + DPI + DPJ).');
        Fpm = Fun((Par + DPI - DPJ).');
        Fmp = Fun((Par - DPI + DPJ).');
        Fmm = Fun((Par - DPI - DPJ).');

        H(Ipar,Jpar) = (Fpp - Fpm - Fmp + Fmm) ./ ...
                       (4.*Step(Ipar).*Step(Jpar));

        H(Jpar,Ipar) = H(Ipar,Jpar);

    end
end

end