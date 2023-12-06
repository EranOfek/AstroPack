function [Int, IntErr]=trapzErr(X,Y,ErrY,Dim)
    % Trapz interation with error propagation
    %   Given a vector or matrix of values and errors, performs trapz
    %   integration with error propagation.
    % Input  : - Vector or matrix of X values.
    %            If input is vector, then it is treated as a column vector
    %            if Dim=1 and as a row vector if Dim=2
    %          - Vector or matrix of Y values.
    %          - Vector or matrix of errors on Y values.
    %            If empty, call regular trapz.
    %            Default is [].
    %          - Dimension on which to perform the integration.
    %            Default is 1.
    % Output : - Trapz integration values along the requested dimension.
    %          - Trapz integration error propagated from the errors on the
    %            Y values.
    % Author : Eran Ofek (Sep 2023)
    % Example: x=(1:1:3).'; y=[1 2 3; 1 3 4; 1 4 5]; Err=ones(3,3).*0.1;
    %          [Int,IntErr]=tools.math.integral.trapzErr(x,y,Err)
    
    arguments
        X
        Y
        ErrY  = [];
        Dim   = 1;
    end
    
    if min(size(X))==1
        if Dim==1
            X = X(:);
        elseif Dim==2
            X = X(:).';
        else
            error('Dim must be 1 or 2');
        end
    end
    
    Int    = trapz(X, Y, Dim);
    
    if isempty(ErrY)
        IntErr = [];
    else
        DX     = diff(X, 1, Dim);
        if Dim==1
            Ymean    = 0.5.*(Y(2:end,:) + Y(1:end-1,:));
            YmeanErr = 0.5.*(ErrY(2:end,:) + ErrY(1:end-1,:));
        elseif Dim==2
            Ymean    = 0.5.*(Y(:,2:end) + Y(:,1:end-1));
            YmeanErr = 0.5.*(ErrY(:,2:end) + ErrY(:,1:end-1));
        else
            error('Dim must be 1 or 2');
        end

        IntErrVec = YmeanErr.*DX;
        IntErr    = sqrt(sum(IntErrVec.^2, Dim));
    end
end