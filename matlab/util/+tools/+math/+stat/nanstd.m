function Result = nanstd(X, Type, Dim)
    % faster version of nanstd using the 'omitnan' option.
    % see median for options:
    % Example: tools.math.stat.nanstd(rand(100,100),[],2)
    
    arguments
        X
        Type   = [];
        Dim    = 1;
    end
    
    Result = std(X, Type, Dim, 'omitnan');
end
