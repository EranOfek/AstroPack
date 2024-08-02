function [D] = pitagoras(X, Y, UseMex)
    % Pitagorian distance sqrt(X.^2+Y.^2) using mex.
    % Input  : - Array of X.
    %          - Array of Y.
    %          - A logical indicating if to use mex. Default is true.
    % Output : - Pitaogrian distance : sqrt(X.^2+Y.^2)
    % Author : Eran Ofek (2024 Aug) 
    % Example: X=rand(1700,1700); Y=rand(1700,1700); X=single(X); Y=single(Y);
    %          tools.math.geometry.pitagoras(X,Y)

    arguments
        X
        Y
        UseMex logical   = true;
    end
    
    if UseMex
        switch class(X)
            case 'double'
                D = tools.math.geometry.mex.mex_pitagoras_double(X, Y);
            case 'single'
                D = tools.math.geometry.mex.mex_pitagoras_single(X, Y);
            otherwise
                error('All inputs should be either double or single class');
        end
    else
        D = sqrt(X.^2 + Y.^2);
    end
            

end
