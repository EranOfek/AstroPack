function NewY = interp1evenlySpaced(X,Y,NewX)
    % A faster versio of interp1q for evenly spaced data (linear interpolation).
    % Input  : - X (equally spaced and sorted)
    %          - Y
    %          - Vector of new X values in which to interpolate.
    % Output : - Vector of interpolated Y values.
    % Author : Eran Ofek (Oct 2021)
    % Example: X = (1:2:100); Y = (1:2:100); NewX = [3.1, 4.1, 5.9];
    %          tic, for I=1:1:10000, NewY = tools.interp.interp1evenlySpaced(X,Y,NewX); end, toc
    %          tic; for I=1:1:10000, NewY=interp1(X,Y,NewX); end, toc
    %          tic; for I=1:1:10000, NewY=interp1q(X,Y,NewX); end, toc
    
    X = X(:);
    Y = Y(:);
    NewX = NewX(:);
    
    StepX = X(2) - X(1);
    NX = NewX(:).' - X(:);
    NX(NX<0) = Inf;
    [~,IndMin] = min(NX);
    DY = Y(IndMin+1) - Y(IndMin);
    DX = NewX - X(IndMin);
    NewY = Y(IndMin) + DY .* DX./StepX;
    
end

