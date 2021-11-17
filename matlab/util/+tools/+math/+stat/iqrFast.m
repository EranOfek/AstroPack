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
    
    switch Dim
        case 1
            Result = range(X(Ind,:));
        case 2
            Result = range(X(:,Ind),2);
        otherwise
            error('Dim must be 1 or 2');
    end
    
end