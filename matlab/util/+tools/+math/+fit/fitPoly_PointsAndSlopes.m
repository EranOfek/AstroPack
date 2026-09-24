function [Result] = fitPoly_PointsAndSlopes(T, X, Xdot, Args)
    % Fit a polynomial using points and first-derivative constraints
    %   of ther form: X = A + B.*T + C.*T.^2 + ...
    % Input  : - Column vector of independet variable (e.g., time).
    %          - An array of measured X positions. Column per object.
    %          - An array of measured Xdot positions. Column per object.
    %          * ...,key,val,... 
    %            'Orders' - Polynomial orders to fit.
    %                   Default is [0 1 2].
    %            'SubT' - Subtract mid Time from T.
    %                   Default is true.
    %            'ErrX', 'ErrXdot' - Errors for X and Xdot. If empty, use 
    %                   backslash without errors. Default is [].
    % Output : - A structure with the following fields:
    %            .Par - Array of fitted parameters. Column per object.
    %            .T - Vector of time (mid subtracted).
    %            .MidT - Reference time.
    %            .X
    %            .Xdot
    %            .Resid
    %            .RMS
    %            .Chi2
    %            .Nobs
    %            .Npar
    %            .Dof
    % Author : Eran Ofek (2025 Jun) 
    % Example: A=1; B=0.01; C=1e-4; T=(1:2).'; X=A+B.*T+C.*T.^2; Xdot=B+2.*C.*T; R=tools.math.fit.fitPoly_PointsAndSlopes(T, X, Xdot);

    arguments
        T
        X
        Xdot
        Args.Orders      = [0 1 2];
        Args.SubT        = true;
        Args.ErrX        = [];
        Args.ErrXdot     = [];
    end


    Args.Orders = Args.Orders(:).';
    T = T(:);
    N = numel(T);

    if Args.SubT
        MidT = (T(1)+T(end)).*0.5;
        T    = T - MidT;
    else
        MidT = 0;
    end

    Result.T    = T;
    Result.X    = X;
    Result.Xdot = Xdot;
    Result.RefT = MidT;

    H1 = T.^Args.Orders;
    H2 = [zeros(N,1), Args.Orders(2:end).*T.^(Args.Orders(2:end)-1)];
    H  = [H1; H2];

    Ncol = size(X,2);
    [Nobs, Npar] = size(H);

    Y = [X; Xdot];
    if ~isempty(Args.ErrX) && ~isempty(Args.ErrXdot)
        UseErr = true;

        Npar = size(H,2);
        Result.Par    = zeros(Npar, Ncol);
        Result.ParErr = zeros(Npar, Ncol);
        for Icol=1:1:Ncol
            InvV = 1./([Args.ErrX(:,Icol); Args.ErrXdot(:,Icol)].^2);
            [Par, ParErr] = lscov(H, Y(:,Icol), InvV);
            Result.Par(:,Icol)    = Par(:);
            Result.ParErr(:,Icol) = ParErr(:);
        end
       
    else
        UseErr = false;
        
        Result.Par = H\Y;
        Result.ParErr = NaN;
        
    end

    Ymodel = H*Result.Par;
    Result.Resid  = Y - Ymodel;
    Result.RMS  = std(Result.Resid,[],1);

    if UseErr
        Result.Chi2 = sum((Result.Resid./[Args.ErrX; Args.ErrXdot]).^2, 1, 'omitnan');
    else
        Result.Chi2 = [];
    end
    Result.Nobs = Nobs;
    Result.Npar = Npar;
    Result.Dof  = Nobs - Npar;

end
