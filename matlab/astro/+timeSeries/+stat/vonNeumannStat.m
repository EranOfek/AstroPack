function [VN, Mu, Sigma] = vonNeumannStat(X, Dim, Norm)
    % Calculate the unbiased von Neumann statistics for a time series.
    % Input  : - The values of a time series (sorted by time).
    %          - Dimension along to calculate the statistics.
    %            Default is 1 (for each colum).
    %          - Normalize the statistics such that it will have mean=0
    %            and std=1.
    %            Default is true.
    % Output : - The von Neumann test statistics.
    %          - Mu by which the statistics is normalized.
    %          - Sigma by which the statistics is normalized.
    % Reference: https://search.r-project.org/CRAN/refmans/DescTools/html/VonNeumannTest.html
    % Author : Eran Ofek (Jan 2023)
    % Example: X = rand(20,1000);
    %          [VN, Mu, Sigma] = timeSeries.stat.vonNeumannStat(X);
    
    arguments
        X
        Dim          = 1;
        Norm logical = true;
    end
   
    if Dim==1
        % do nothing
    elseif Dim==2
        X = X.';
    else
        error('Dim 1 or 2 only are supported');
    end
        
    % unbiased case
    N  = size(X,1);
    VN = sum(N.*(X(1:end-1,:) - X(2:end,:)).^2)./sum( (N-1).*(X - mean(X)).^2 );
    
    Mu    = 2.*N./(N-1);
    Sigma = sqrt( 4.*N.^2.*(N-2)./( (N+1).*(N-1).^3 ) ); 
    
    if Norm
        VN = (VN - Mu)./Sigma;
    end
    
    
end
