function Result = iqrFast(X, Dim)
    % A fast iqr (inter quantile range) function (without interpolation) 
    % Input  : - An array.
    %          - Dimension along to calculate the quantile (1 | 2 | [1 2]).
    %            Default is 1.
    % Output : - IQR.
    % Author : Eran Ofek (Jul 2021)
    % Spped  : ~3 times faster than iqr.
    % Example: R=rand(10000,1);
    %          Quant = tools.math.stat.iqrFast(R)
    %          tic; for I=1:1:1e3, Quant = tools.math.stat.iqrFast(R); end, toc
    %          tic; for I=1:1:1e3, Quant = iqr(R); end, toc
    %          R = rand(1000,3);
    %          Quant = tools.math.stat.iqrFast(R)
    %          Quant = tools.math.stat.iqrFast(R,[1 2])
    %          R = rand(10,3,4);
    %          Quant = tools.math.stat.iqrFast(R,3)
    
    arguments
        X
        Dim = 1;
    end
    P = [0.25, 0.75];
    
    if all(Dim==[1 2])
        X = X(:);
        Dim = 1;
    end
    
    N   = size(X, Dim);
    X   = sort(X, Dim);
    Ind = ceil(P.*N);
    Ind = (Ind(1):Ind(2));
    Ndims = ndims(X);
    if Ndims>3
        error('iqrFast can treat only: 1,2,3 dimensions');
    end
    
    switch Dim
        case 1
            if Ndims==3
                Result = range(X(Ind,:,:));
            else
                Result = range(X(Ind,:));
            end
        case 2
            if Ndims==3
                Result = range(X(:,Ind,:),2);
            else
                Result = range(X(:,Ind),2);
            end
        case 3
            Result = range(X(:,:,Ind),3);
        otherwise
            error('Dim must be 1, 2, or 3');
    end
    
end