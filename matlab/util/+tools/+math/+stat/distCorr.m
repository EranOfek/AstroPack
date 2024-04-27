function [DCov2n, Tn, DCorr2] = distCorr(X, Y)
    % Calculate the distance correaltaion and covariance (independeny test)
    % Input  : - Vector X.
    %          - Vector Y.
    % Output : - Distance covariance dCov2n
    %          - A consistent multivariate test of independence of random
    %            vectors in arbitrary dimensions.
    %            =n*DCov2n
    %          - Distance correlation
    % Author : Eran Ofek (2024 Apr) 
    % Example: tools.math.stat.distCorr((1:1:100),(1:1:100))

    arguments
        X
        Y
    end
    
    X = X(:);
    Y = Y(:);
    
    N = numel(X);
    
    a_jk = abs(X - X.');
    b_jk = abs(Y - Y.');
    
    A_jk = a_jk - mean(a_jk, 2, 'omitnan') - mean(a_jk, 1, 'omitnan') + mean(a_jk, 'all','omitnan');
    B_jk = b_jk - mean(b_jk, 2, 'omitnan') - mean(b_jk, 1, 'omitnan') + mean(b_jk, 'all','omitnan');
    
    DCov2n = sum(A_jk.*B_jk, 'all', 'omitnan')./(N.^2);
    
    if nargout>1
        Tn     = N.*DCov2n;
    
        DVarXn = sum(A_jk.*A_jk, 'all', 'omitnan')./(N.^2);
        DVarYn = sum(B_jk.*B_jk, 'all', 'omitnan')./(N.^2);
        
        DCorr2 = DCov2n./sqrt(DVarXn.*DVarYn);
    end
end
