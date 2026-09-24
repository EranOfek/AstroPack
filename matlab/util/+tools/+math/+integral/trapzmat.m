function Trapz=trapzmat(X,Y,Dim, UseMex)
    % Trapezoidal numerical integration on columns or rows of matrices.
    % Description: Trapezoidal numerical integration on columns or rows of
    %              matrices.
    %              Contrary to trapz.m, the X input for this function can
    %              be a matrix.
    % Input  : - X matrix.
    %          - Y matrix.
    %          - Dimension along to preform the integration, default is 1.
    %          - UseMex. If true call: tools.math.integral.mex.trapzmat_mex
    %            default is true.
    % Output : - Vector of integrals.
    % Author : Eran Ofek (Jun 2009)
    
    
    arguments
        X
        Y
        Dim    = 1;
        UseMex = true;
    end
    
    if UseMex
        Trapz = tools.math.integral.mex.trapzmat_mex(X, Y, Dim);
    else
        switch Dim
         case 1
            % do nothing
         case 2
            X = X.';
            Y = Y.';
         otherwise
            error('Unknwon Dim option');
        end
        
        Trapz = sum(abs(diff(X,1,1)).*0.5.*( Y(1:end-1,:) + Y(2:end,:) ) );
    end
end