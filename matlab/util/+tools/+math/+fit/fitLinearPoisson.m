function [Par, ParErr, Fval, Cov_matrix] = fitLinearPoisson(X, N, Args)
    % Fit a linear model to data with Poisson noise (e.g., counts)
    %   Given a linear model for the expectation value of the Poisson
    %   distribution Lambda=a+b*f(x)+c*g(x)+... we would like to fit for
    %   [a,b,c,...] given x and N(x) where N is a Poisson random variable
    %   with expectency value Lambda(x).
    % Input  : - X variable.
    %          - N(X) - number of counts measured in each X position.
    %          * ...,key,val,... 
    %            'Fun' - A cell array of function handels from which the
    %                   linear model is composed:
    %                   The sum of the functionals gives Lambda(X) - i.e.,
    %                   the expectency value of the Poisson distribution at
    %                   point X.
    %                   Default is {@(x) ones(size(x)), @(x) x}
    %                   (corresponds to an Lambda=a+bX model).
    %            'Pars0' - First guess arguments. Default is [0.001 0.001].
    %            'Display' - fminunc Display parameter. Default is 'off'.
    % Output : - Vector of best fit parameters.
    %          - Vector of best fit parameter errors.
    %          - Minus Log likelihood at best fit position.
    %          - Covariance matrix.
    % Author : Eran Ofek (2024 Dec) 
    % Example: [Par,ParErr,LogL,Cov]=tools.math.fit.fitLinearPoisson;
    %          N=5000; Par=zeros(N,2); for I=1:N, Par(I,:)=tools.math.fit.fitLinearPoisson; end
    
    arguments
        X            = [];
        N            = [];
        Args.Fun     = {@(x) ones(size(x)), @(x) x};
        Args.Pars0   = [0 0]+0.001;
        Args.Display = 'off';
    end

    
    if isempty(X)
        % run in simulation mode
        X = (0.0:0.001:1).';
        Y = 0.1 + 0.7.*X;
        N = poissrnd(Y);
    end
    
    
    
    Npar = numel(Args.Fun);
    
    
%     VecA=[0.0:0.01:0.2].';
%     VecB=[0.5:0.01:1.0].';
%     Na= numel(VecA);
%     Nb= numel(VecB);
%     
%     for Ia=1:Na
%         for Ib=1:Nb
%             LogL(Ia,Ib) = objectiveFun([VecA(Ia), VecB(Ib)]);
%         end
%     end
%     surface(VecA,VecB,LogL.')
%     colorbar

    
    Options = optimoptions('fminunc', 'Algorithm', 'quasi-newton', 'Display', Args.Display);
    % Perform minimization
    [Par, Fval, Exitflag, Output, Grad, Hessian] = fminunc(@objectiveFun, Args.Pars0, Options);

    % Estimate errors from Hessian (covariance matrix)
    Cov_matrix = inv(Hessian);
    ParErr = sqrt(diag(Cov_matrix));

        
    function Lambda=calcFun(X, Fun, Par, Npar)
        %

        Lambda = 0;
        for Ipar=1:1:Npar
            Lambda = Lambda + Par(Ipar).*Fun{Ipar}(X);
        end

    end

    function LogL=poissonLogLikelihood(Lambda, N)
        % Return the -Log(likelihood)

        LogNfact = log(gamma(N+1));
        if isinf(LogNfact)
            % use the Stirling approximation:
            LogNfact = N.*log(N) - N + 1;
        end

        LogL = sum(Lambda - N.*log(Lambda) + LogNfact);
        if isnan(LogL)
            LogL = Inf;
        end
    end

    function LogL=objectiveFun(Pars)
        %

        Lambda=calcFun(X, Args.Fun, Pars, Npar);
        LogL = poissonLogLikelihood(Lambda, N);

    end


end
