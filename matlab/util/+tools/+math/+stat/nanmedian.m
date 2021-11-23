 function Result = nanmedian(X, Dim)
    % faster version of nanmedian using the 'omitnan' option.
    % see median for options:
    % Example: tools.math.stat.nanmedian(rand(100,100),2)
    
    arguments
        X
        Dim = 1;
    end
    
    Result = median(X, Dim, 'omitnan');
end
