function [Mode,Variance]=mode(Array,Log,IgnoreNaN,Accuracy,MinN) 
% Mode and variance of a distribution
% Package: @imUtil.background
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
% Output : - The robust median calculated using the scaled iqr
%      By: Eran O. Ofek                       Apr 2020             
% Example: imUtil.background.mode(randn(1000,1000))

arguments
    Array
    Log             = true;
    IgnoreNaN       = true;
    Accuracy        = 0.1;
    MinN            = 10;
end


if IgnoreNaN
    Array = Array(~isnan(Array));
end

N = numel(Array);
if N<MinN
    Mode      = NaN;
    Variance  = NaN;
else
    if Log
        % remove negative numbers
        Fnneg = Array>0;
        Array = log10(Array(Fnneg));
    else
        Array = Array(:);
    end
    
    % remove infinities
    Array = Array(~isinf(Array));

    Min = min(Array);
    Max = max(Array);
    
    Nbin = (1./Accuracy).^2;
    BinSize = (Max - Min)./Nbin;
    Edges = (Min-BinSize:BinSize:Max+BinSize).';
    if isempty(Edges)
        % this happens when there is only a single value
        if Max==Min
            if Log
                Mode = 10.^Max;
            else
                Mode = Max;
            end
            Variance = 0;
        else
            error('Edges is empty and Max not equal Min');
        end
    else
        
        Nhist = histcounts(Array,Edges);

        [~,MaxI]  = max(Nhist);
        Mode = Edges(MaxI) + 0.5.*BinSize;

        if Log
            Mode = 10.^Mode;
        end

        if nargout>1
            CumN = cumsum(Nhist(:));
            CumN = CumN + (1:1:numel(CumN)).'.*1000.*eps;
            % interp1q is faster, but doesnt check validity
            IqrVal = interp1q(CumN,Edges(1:end-1)+0.5.*BinSize,[0.25 0.75]'.*CumN(end));
            %IqrVal = interp1(CumN,Edges(1:end-1)+0.5.*BinSize,[0.25 0.75]'.*CumN(end),'linear');

            Factor = 0.7413;  %  = 1./norminv(0.75,0,1)

            if Log
                Variance = (range(10.^IqrVal).*Factor).^2;
            else
                Variance = (range(IqrVal).*Factor).^2;
            end
        end
    end
end

        
    
    
    
end


