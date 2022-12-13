function [Par, ParErr] = fitPolyFiniteExpTime(T,Y,ErrY, ExpTime, Args)
    %
    
    arguments
        T
        Y
        ErrY        = 1;
        ExpTime     = 1;
        Args.Orders = [0 1 2];
        
    end

    
    
    Norder = numel(Args.Orders);
    T = T(:);
    N = numel(T);
    ErrY    = ErrY(:).*ones(N,1);
    ExpTime = ExpTime(:).*ones(N,1);
    T1      = T - ExpTime.*0.5;
    T2      = T + ExpTime.*0.5;
    
    H = zeros(N,Norder);
    for Iorder=1:1:Norder
        K = Args.Orders(Iorder);
        H(:,Iorder) = ((T2.^(K+1))./(K+1) - (T1.^(K+1))./(K+1))./ExpTime;
    end
    
    [Par,ParErr] = lscov(H,Y, 1./ErrY.^2);

end
