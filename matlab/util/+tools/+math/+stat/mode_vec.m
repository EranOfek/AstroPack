function [Mode, Variance] = mode_vec(Vector, Log, IgnoreNaN, Accuracy, MinN, OnlyLower)
    % Mode and variance of a distribution
    % Description: Calculate the mode and robust variance of an array.
    %              The mode is calculated by making an histogram and choosing
    %              the bin with the highest number of counts. The robust 
    %              variance is calculated from the histogram (via interpolation).
    % Input  : - An array for which to calculate the global mode, and robust
    %            variance.
    %          - (Log) A logical indicating if to calculate the histogram of
    %            the log of the values, this is recomended when the values
    %            distribution has an higher tail (e.g., like in astronomical
    %            images).
    %            Default is true.
    %          - Ignore NaNs. Default is true.
    %          - (Accuracy). The (roughly) required accuracy. This parameter
    %            controls the histogram bin size (requiring that on average
    %            each bin contains (1/Accuracy)^2 points).
    %            Default is 0.1.
    %          - (MiN) Minimum number of points (not NaN) in the array.
    %            If the number of points is smaller than this bound then the
    %            function will return NaNs.
    %            Default is 10.
    %          - A logical indicating if to calculate the variance on the lower
    %            quantile. Default is true.
    % Output : - The robust median calculated using the scaled iqr
    %      By: Eran Ofek (Feb 2022)
    % Example : R=randn(300,300);
    %           [Mode, Variance] = tools.math.stat.mode_vec(R);
    
    arguments
        Vector
        Log logical           = true;
        IgnoreNaN logical     = false;
        Accuracy              = 0.1;
        MinN                  = 10;
        OnlyLower logical     = false;
    end
    
    Array = Vector(:);
    
    if IgnoreNaN
        Array = Array(~isnan(Array) & ~isinf(Array));
    end

    Min = min(Array);
    if Log
        Min = max(1, Min);  % ignore negative values
    end
    Max = max(Array);

    Nbin = (1./Accuracy).^2;
        
    if Log
        Edges   = logspace(log10(Min), log10(Max), Nbin).';
        BinSize = Edges(2:end) - Edges(1:end-1);
    else
        BinSize = (Max - Min)./Nbin;
        Edges = (Min-BinSize:BinSize:Max+BinSize).';
    end
    
    if isempty(Edges)
        % this happens when there is only a single value
        if Max==Min
            Mode = Max;
            Variance = 0;
        else
            error('Edges is empty and Max not equal Min');
        end
    else
        Nhist = matlab.internal.math.histcounts(Array, Edges);

        [~,MaxI]  = max(Nhist);

        if MaxI==numel(Edges)
            Mode = Edges(end);
        else
            Mode = (Edges(MaxI) + Edges(MaxI+1)).*0.5;
        end
        
        if nargout>1
            CumN = cumsum(Nhist(:));
            CumN = CumN + (1:1:numel(CumN)).'.*10000.*eps;

            % interp1q is faster, but doesnt check validity
            
            IqrVal = interp1q(CumN, Edges(1:end-1)+0.5.*BinSize, [0.25 0.75]'.*CumN(end));

            Factor = 0.7413;  %  = 1./norminv(0.75,0,1)

            if OnlyLower
                IqrVal(2) = log(Mode);
                Factor    = Factor.*2;
            end
            Variance = (range(IqrVal).*Factor).^2;
        end
    end
end