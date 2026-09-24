function [Xvec,Yvec] = increaseCurveDensityByLocalInterp(X, Y, Args)
    % Increase the sampling density of some 2D curve.
    % Input  : - Vector of X.
    %          - Optional Vector of Y. Default is [].
    %          * ...,key,val,... 
    %            'Nadd' - Number of points to add + 1, between each pair of points.
    %                   Default is 2.
    %            'InterpMethod' - Interpolation method. Default is 'cubic'.
    %            'AddFirstLast' - If true, then add first point at the end
    %                   of the vector. Useful for closed curves.
    %                   Default is false.
    % Output : - Column vector of oversampled X.
    %          - Column vector of oversampled Y.
    % Author : Eran Ofek (2024 Mar) 
    % Example: [Xv,Yv]=tools.interp.increaseCurveDensityByLocalInterp(xE,yE)

    arguments
        X
        Y                      = [];
        Args.Nadd              = 2;
        Args.InterpMethod      = 'cubic';
        Args.AddFirstLast logical = false;
    end
    
    X = X(:);
    Y = Y(:);
    
    if Args.AddFirstLast
        X = [X; X(1)];
        if ~isempty(Y)
            Y = [Y; Y(1)];
        end
    end
    VecNadd = (0:1:Args.Nadd-1).'./Args.Nadd;
    
    N = numel(X);
    DX = diff(X);
    DY = diff(Y);
    Xvec = [];
    Yvec = [];
    for I=1:1:N-1
        
        %Xvec = [Xvec; X(I) + VecNadd.*DX(I)];
        %Yvec = [Yvec; Y(I) + VecNadd.*DY(I)];
        if I<N-1
            Xv = interp1([0 1 2], [X(I) X(I+1) X(I+2)], VecNadd, Args.InterpMethod);
            Xvec = [Xvec; Xv];
            Yv = interp1([0 1 2], [Y(I) Y(I+1) Y(I+2)], VecNadd, Args.InterpMethod);
            Yvec = [Yvec; Yv];
        else
            Xv = interp1([-1 0 1], [X(I-1) X(I) X(I+1)], VecNadd, Args.InterpMethod);
            Xvec = [Xvec; Xv];
            Yv = interp1([-1 0 1], [Y(I-1) Y(I) Y(I+1)], VecNadd, Args.InterpMethod);
            Yvec = [Yvec; Yv];
        end
    end

end
