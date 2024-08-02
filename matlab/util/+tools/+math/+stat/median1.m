function [Result] = median1(X)
    % Fast median of a vector without ignoring NaNs, using mex.
    % Input  : - A vector (single or double). NaNs are not ignored.
    % Output : - The median.
    % Compilation : mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mex_median1_double.cpp
    % Author : Eran Ofek (2024 Aug) 
    % Example: tools.math.stat.median1(V)

    arguments
        X
    end

    switch class(X)
        case 'double'
            Result = tools.math.stat.mex.mex_median1_double(X);
        case 'single'
            Result = tools.math.stat.mex.mex_median1_single(X);
        otherwise
            error('median1 works on single or double precision inputs');
    end
    
end
