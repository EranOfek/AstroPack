function varargout = stairsCumulative(X, Style, Args)
    % Cumulative stairs plot of a vector.
    %   plot the sorted values of a vector vs. the running index
    %   I.e., a KS plot.
    % Input  : - A vector.
    %          - Style. Default is 'k-'.
    %          * ...,key,val,...
    %            'Norm' - Normalize by the number of data points.
    %                   Default is true.
    % Output : - A axis handle.
    % Author : Eran Ofek (Jul 2022)
    % Example: plot.stairsCumulative(rand(100,1));
    
    arguments
        X
        Style = 'k-';
        Args.Norm logical   = true; 
    end
   
    % remove NaN
    X = X(~isnan(X));
    N = numel(X);
    % sort X
    X = sort(X);
    
    if Args.Norm
        Norm = N;
    else
        Norm = 1;
    end
    [varargout{1:1:nargout}] = stairs(X,(1:1:N)./Norm, Style);
    
end