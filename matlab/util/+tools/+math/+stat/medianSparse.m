function Result = medianSparse(Vec, Sparse, UseFast)
    % Calculate the median of an array using only a sub-sample of the points.
    % Input  : - An array for which to calculate the median over all
    %            dimensions.
    %          - Sparsness factor (step size for point to use).
    %            Default is 10.
    %          - A logical indicating if to use fast_median.
    %            Default is false.
    % Output : - The median of the array over all dimensions.
    % Author : Eran Ofek (Nov 2023)
    % Example: RR=ones(1700,1700); tic: for I=1:1:100, median(RR,'all','omitnan'); end
    %          tic; for I=1:1:100,tools.math.stat.medianSparse(RR,1,true);end,toc
    %          tic; for I=1:1:100,tools.math.stat.medianSparse(RR,5,false);end,toc

    arguments
        Vec
        Sparse          = 10;
        UseFast logical = false;
    end

    Vec = Vec(:);
    if UseFast
        Result = fast_median(Vec(1:Sparse:end));
    else
        Result = median(Vec(1:Sparse:end), 1, 'omitnan');
    end

end