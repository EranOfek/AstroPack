function Quant = quantileFast(X, P, Dim)
    % A fast quantile function (without interpolation) 
    % Input  : - An array.
    %          - Fraction, or a vector of fractions.
    %          - Dimension along to calculate the quantile (1 | 2 | [1 2]).
    %            Default is 1.
    % Output : - Quantile.
    % Author : Eran Ofek (Jul 2021)
    % Spped  : ~3 times faster than quantile.
    % Example: R=rand(10000,1);
    %          Quant = tools.math.stat.quantileFast(R, 0.1)
    %          Quant = tools.math.stat.quantileFast(R, [0.1,0.2])
    %          tic; for I=1:1:1e3, Quant = tools.math.stat.quantileFast(R, 0.1); end, toc
    %          tic; for I=1:1:1e3, Quant = quantile(R, 0.1); end, toc
    
    arguments
        X
        P
        Dim = 1;
    end
    
    if all(Dim==[1 2])
        X = X(:);
        Dim = 1;
    end
    
    N   = size(X, Dim);
    X   = sort(X, Dim);
    Ind = ceil(P.*N);
    
    switch Dim
        case 1
            Quant = X(Ind,:);
        case 2
            Quant = X(:,Ind);
        otherwise
            error('Dim must be 1 or 2');
    end
    
end