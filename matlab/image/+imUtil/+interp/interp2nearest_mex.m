function [Result] = interp2nearest_mex(X, Y, V, XI, YI)
    % Fast (mex) 2D nearest interpolation for output grid defined by row and column vectors.
    % Input  : - Matrix of X coordinates.
    %          - Matrix of Y coordinates.
    %          - Matrix of values to interpolate.
    %          - Row vector of output X coordinates.
    %          - Column vector of output Y coordinates.
    % Output : - Output Interpolated matrix in grid defined by
    %            the row and column (XI, YI) vectors.
    % Author : Eran Ofek (2024 Aug) 
    % Example: R=imUtil.interp.interp2nearest_mex(X,Y,V,XI,YI)

    arguments
        X
        Y
        V
        XI
        YI
    end
    
    switch class(V)
        case 'uint32'
            Result = imUtil.interp.mex.interp2nearest_uint32(X, Y, V, XI, YI);
        case 'uint16'
            Result = imUtil.interp.mex.interp2nearest_uint16(X, Y, V, XI, YI);
        case 'single'
            Result = imUtil.interp.mex.interp2nearest_single(X, Y, V, XI, YI);
        case 'double'
            Result = imUtil.interp.mex.interp2nearest_double(X, Y, V, XI, YI);
        otherwise
            error('Unsupported class for matrix of values');
    end

end
