function [AllBestPar, Cov, Info] = lsqNonLinWithFixed(X, Y, Sigma, Model, Args)
    % lsqNonLinWithFixed - Fit a non-linear model while holding a subset of params fixed.
    %   Use lsqnonlin to fit only the parameters flagged in Args.FitPar,
    %   keeping the rest at their initial values. Residuals are weighted
    %   by Sigma (1./Sigma). Returns fitted-subset params, the full
    %   parameter vector, covariance, chi^2, dof, and residuals.
    % Input  : - X        : Predictor(s), passed to Model(X, P).
    %            If not provided then will run in simulation mode.
    %          - Y        : Observations vector.
    %          - Sigma    : Weights (scalar or vector), default 1 (no weighting).
    %          - Model    : Function handle @(X,P)-> yhat. Default: @(X,P) P(1).*sin(P(2)+P(3).*X)
    %          * ...,key,val,...
    %            'FitPar' - Logical mask or indices of params to fit. Default [true,false,true].
    %            'InitPar' - Initial parameter vector. Default [1 0.1 0.5].
    %            'Lb' - Lower bounds (only fitted, full-length or []), default [].
    %            'Ub' - Upper bounds (only fitted, full-length or []), default [].
    %            'Opts' - optimoptions for lsqnonlin, default [].
    % Output : - BestPar     : Best-fit values for the *fitted subset only*
    %                          (same order as Args.InitPar(Args.FitPar)).
    %          - AllBestPar  : Full parameter vector (fitted + fixed).
    %          - Cov         : Full covariance matrix (size = numel(InitPar)), with
    %                          fitted-subset covariance embedded; zeros elsewhere.
    %          - Chi2        : Sum of squared weighted residuals at the solution.
    %          - Dof         : Degrees of freedom (= numel(residuals) - #fitted parameters).
    %          - Resid       : Weighted residuals at the solution ((Model - Y)./Sigma).
    % See also: lsqnonlin, nlparci
    % Example: [Par, Cov, Info] = lsqNonLinWithFixed(); % simulation mode
    %          or i=1:100, [Par, Cov, Info] = lsqNonLinWithFixed(); C2(i)=Info.Chi2; end; hist(C2,30);
    %          [Par, Cov, Info] = lsqNonLinWithFixed(X,Y,1,@(X,P) P(1).*sin(P(2)+P(3).*X),'FitPar',[true,false,true],'InitPar',[1 0.1 0.5]));
    
    
    arguments
        X             = [];
        Y             = [];
        Sigma         = 1;
        Model         = @(X, P) P(1).*sin(P(2)+P(3).*X);
        Args.FitPar   = [true, false, true];
        Args.InitPar  = [1 0.1 0.5]
        Args.Lb       = []
        Args.Ub       = []
        Args.Opts     = []
    end
    

    if nargin==0
        N      = 200;
        X      = linspace(0, 2*pi, N).';
        Ptrue  = [2.0, 0.30, 1.50];
        Model  = @(X,P) P(1).*sin(P(2) + P(3).*X);
        
        % Generate noisy data with known sigma
        Sigma  = 0.08*ones(size(X));
        Y      = Model(X, Ptrue) + Sigma.*randn(size(X));
        
        % Fix P2, fit P1 & P3
        Args.FitPar  = [true, false, true];
        Args.InitPar = [1.0, 0.30, 1.00];    % P2 fixed at 0.30
        Args.Lb      = [0, -Inf, 0];         % bounds for full vector (sliced internally)
        Args.Ub      = [Inf, Inf, Inf];
        Args.Opts    = optimoptions('lsqnonlin','Display','off');
        
        % [AllBestPar, Cov, Info] = lsqNonLinWithFixed(X, Y, Sigma, Model, Args);
        % fprintf('True      : [%.4f  %.4f  %.4f]\n', Ptrue);
        % fprintf('AllBestPar: [%.4f  %.4f  %.4f]\n', AllBestPar);
        % fprintf('Chi2/DoF  : %.3f (%d dof)\n', Info.Chi2/max(Info.Dof,1), Info.Dof);
        % 
        % % Quick plot
        % xf = linspace(min(X), max(X), 1000).';
        % yf = Model(xf, AllBestPar);
        % figure('Color','w'); hold on;
        % plot(X, Y, '.', 'MarkerSize', 8);
        % plot(xf, yf, 'LineWidth', 2);
        % legend('Data','Fit'); grid on;
        % xlabel('X'); ylabel('Y');
        % title(sprintf('Fit with P2 fixed (P2 = %.2f)', AllBestPar(2)));
    end



    % Normalize mask/indices
    P0  = Args.InitPar(:);
    Np  = numel(P0);
    if islogical(Args.FitPar)
        FitMask = Args.FitPar(:);
    else
        FitMask = false(Np,1);
        FitMask(Args.FitPar) = true;
    end
    
    % Initial subset and sliced bounds (expects full-length Lb/Ub or [])
    Par0 = P0(FitMask);
    if isempty(Args.Lb), Lb = [];
    else,               Lb = Args.Lb(:); Lb = Lb(FitMask);
    end
    if isempty(Args.Ub), Ub = [];
    else,               Ub = Args.Ub(:); Ub = Ub(FitMask);
    end
    
    % Weighted residual with fixed params injected
        function r = ResFun(ParFitted)
            AllPar          = P0;
            AllPar(FitMask) = ParFitted;
            r = ( Model(X, AllPar) - Y ) ./ Sigma;
            r = r(:);
        end
    
    % Solve on reduced space; keep Jacobian for covariance
    [FittedBestPar, ~, Res, ~, ~, ~, J] = lsqnonlin(@ResFun, Par0, Lb, Ub, Args.Opts);
    
    % Rebuild full parameter vector
    AllBestPar          = P0;
    AllBestPar(FitMask) = FittedBestPar;
    
    % Residuals, Chi2, DoF
    Resid = Res(:);
    Chi2  = sum(Resid.^2);
    K     = sum(FitMask);
    Dof   = numel(Resid) - K;
    
    % Covariance: embed fitted-subset covariance into full matrix (Gauss–Newton)
    Cov = zeros(Np, Np);
    if K > 0
        S2      = Chi2 / max(Dof, 1);
        CovFit  = S2 * inv(J.'*J);    % minimal change (explicit inverse)
        Idx     = find(FitMask);
        Cov(Idx, Idx) = CovFit;
    end
    
    % Info bundle
    Info.Chi2          = Chi2;
    Info.Dof           = Dof;
    Info.FittedBestPar = FittedBestPar;
    Info.Nobs          = numel(Y);
    Info.Resid         = Resid;

end
