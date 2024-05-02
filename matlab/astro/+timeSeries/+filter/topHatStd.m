function [Z,V,Prob] = topHatStd(X, Args)
    % A moving mean/median filter normalized to the global mean and by the local std.
    %   Calculate a top hat mean filter of the form:
    %   Z=(local_mean - global_mean)/(local_std/sqrt(local_size))
    %   The Z parameter should follow a student-t distribution.
    % Input  : - A matrix of measurments. By default the time axis is in
    %            the first dimension.
    %          * ...,key,val,... 
    %            'Dim' - Dimension of time axis. Default is 1.
    %            'FilterFun' - Running top-hat filter function.
    %                   Default is @medfilt1
    %            'FilterSize' - Filter block size. Default is 3.
    %            'MeanFun' - Global mean function.
    %                   Default is @median.
    %            'MeanFunArgs' - A cell array of additional arguments to
    %                   pass to the MeanFun.
    %                   Default is {1,'omitnan'}.
    %                   If 'MeanFun' is @quantile, then use here e.g., {0.75,1}
    %            'StdFilt' - Local std filter function.
    %                   Default is @stdfilt.
    % Output : - A matrix with size identical to the input matrix.
    %            Each value corresponds to the significance of the point.
    %            Negative for negative deviations.
    %          - Number of d.o.f.
    %          - Student-t probability corresponding to the significance.
    % Author : Eran Ofek (2024 May) 
    % Example: X = rand(100,1e5);
    %          [Z,V,Prob] = timeSeries.filter.topHatStd(X);
    %          [Z,V,Prob] = timeSeries.filter.topHatStd(X,'FilterFun',@movmean, 'MeanFun',@mean);

    arguments
        X
        Args.Dim               = 1;
        Args.FilterFun         = @medfilt1;  % @movmean;
        Args.FilterSize        = 3;
        Args.MeanFun           = @median;         % @quantile
        Args.MeanFunArgs       = {1, 'omitnan'};  % {0.75,1};
        
        Args.StdFilt           = @stdfilt;
    end

    if Args.Dim==2
        X = X.';
        Args.Dim = 1;
    end
    
    MovingFilt = Args.FilterFun(X, Args.FilterSize);
    Mean       = Args.MeanFun(X, Args.MeanFunArgs{:});
    Std        = Args.StdFilt(X, true(Args.FilterSize,1));
    
    Z          = (MovingFilt - Mean)./(Std./sqrt(Args.FilterSize));
    V          = Args.FilterSize;
    
    if nargout>2
        Prob       = tcdf(Z, V-1);
    end
    
end
