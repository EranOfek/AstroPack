function Result = nanmean(X, Dim)
    % faster version of nanmean using the 'omitnan' option.
    % see median for options:
    % Example: tools.math.stat.nanmean(rand(100,100),2)
    
    arguments
        X
        Dim = 1;
    end
    
    Result = mean(X, Dim, 'omitnan');
end
