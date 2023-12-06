function [Deriv,X] = numerical_deriv(X,Y, Args)
    % Numerical derivative of vectors
    % Input  : - X vector. If empty, use 1..N.
    %          - Y vector.
    %          * ...,key,val,...
    %            'Method'  - Options are:
    %                   'diffTwoSided' (default) - average of two sided
    %                           derivative.
    % Output : - Derivative.
    %          - X vector
    % Author : Eran Ofek (Jun 2022)
    % Example: [Deriv,X] = tools.deriv.numerical_deriv((1:1:3).',[1;2;4]);
   
    arguments
        X 
        Y
        Args.Method   = 'diffTwoSided';
    end
        
    
    if isempty(X)
        N = numel(Y);
        X = (1:1:N).';
        Y = Y.';
    end
    
    
    switch lower(Args.Method)
        case 'difftwosided'
            DX = diff(X);
            DY = diff(Y);
            DYDX = DY./DX;
            
            Deriv = nan(size(X));
            Deriv(end) = DYDX(end);
            Deriv(1)   = DYDX(1);
            Deriv(2:end-1) = 0.5.*(DYDX(1:end-1) + DYDX(2:end));
            
        otherwise
            error('Unknown Method option');
    end
    
    
end