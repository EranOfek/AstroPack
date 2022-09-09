function Ynew = filterVariableFun(X, Y, Args)
    % NOT DEBUGED
    % Example: t = (9:0.01:1000).';
    %          [Res]=astro.supernova.nickel56_decay(t, 0.1);
    %          Y = astro.supernova.filterVariableFun(t, Res.Edep, 'FunArgs',t);
    

    
    arguments
        X
        Y
        Args.Fun function_handle   = @(x, alpha) alpha.*exp(-alpha.*max(0,x));   % one side normalized exponential (integral is 1)
        Args.FunArgs               = 1;   % alpha  % or vector for each x
    end
    
    
    N = numel(X);
    Ynew = zeros(N,1);
    for I=1:1:N
        X0 = X - X(I);
        
        Ynew = Ynew + Args.Fun(X0, Args.FunArgs);
    end
        
end