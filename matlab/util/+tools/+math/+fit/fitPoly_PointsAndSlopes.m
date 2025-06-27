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
    % Output : - A structure with the following fields:
    %            .Par - Array of fitted parameters. Column per object.
    %            .T - Vector of time (mid subtracted).
    %            .MidT - Reference time.
    %            .X
    %            .Xdot
    % Author : Eran Ofek (2025 Jun) 
    % Example: A=1; B=0.01; C=1e-4; T=(1:2).'; X=A+B.*T+C.*T.^2; Xdot=B+2.*C.*T; R=tools.math.fit.fitPoly_PointsAndSlopes(T, X, Xdot);

    arguments
        T
        X
        Xdot
        Args.Orders      = [0 1 2];
        Args.SubT        = true;
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

    H1 = T.^Args.Orders;
    H2 = [zeros(N,1), Args.Orders(2:end).*T.^(Args.Orders(2:end)-1)];
    H  = [H1; H2];

    Par = H\[X; Xdot];
    Result.Par  = Par;
    Result.T    = T;
    Result.X    = X;
    Result.Xdot = Xdot;
    Result.RefT = MidT;

end
