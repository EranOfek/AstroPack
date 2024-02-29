function [Result] = std_outlier1(X, Type, Dim)
    % Std with a single outlier removal if found outside the expected range.
    %   Here the expected range is calculated assuming Gaussian
    %   distribution and the number of data points.
    % Input  : - Data.
    %          - Std type: 0, 1 (see std).
    %          - Dim. Default is 1.
    % Output : - Std.
    % Author : Eran Ofek (2024 Feb) 
    % Example: X=randn(1000,3); X(1)=1000; tools.math.stat.std_outlier1(X)

    arguments
        X
        Type  = [];
        Dim   = 1;
    end

    Med = median(X, Dim, 'omitnan');
    N   = numel(X);
    Nsigma = norminv(1-1./N,0,1);
    RStd   = tools.math.stat.rstd(X, Dim);

    Z = (X - Med)./RStd;
    [~,Imax] = max(abs(Z),[],Dim);
    Ni = numel(Imax);
    for Ii=1:1:Ni
        if Dim==1
            X(Imax(Ii),Ii) = NaN;
        else
            X(Ii, Imax(Ii)) = NaN;
        end
    end

    Result = std(X, Type, Dim, 'omitnan');

end
