function [StdX] = filterStd(X, HalfSize, Args)
    % Calculate the 1-D std filter on equally spaced time series
    %     The std filter can be calculated in a top-hat function region, or
    %     a top-hat with central gap.
    % Input  : - A matrix in which the columns (for 'Dim'=1) are the
    %            equally spaced time series on which to apply the filter.
    %          - Half size of the top-hat filter, or [gap, halfsize] of the
    %            filter. Default is [10 20].
    %          * ...,key,val,... 
    %            'Dim' - Dimension on which to apply the filter.
    %                   Default is 1.
    % Output : - The filtered time series.
    % Author : Eran Ofek (2023 Dec) 
    % Example: SX=timeSeries.filter.filterStd(randn(100,1),[2 20]);
    %          
    arguments
        X
        HalfSize               = [10 20];
        Args.Dim               = 1;
    end
    
    if Args.Dim == 2
        X = X.';
    end

    if numel(HalfSize)==1
        % top hat filter
        Filter = [0; ones(2.*HalfSize+1,1);0];
        
    else
        % assume a gap-filter
        Gap      = HalfSize(1);
        HalfSize = HalfSize(2);
        Filter = [ones(2.*HalfSize+1,1)];
        Filter = fftshift(Filter);
        XF      = (-HalfSize:1:HalfSize)';
        Flag   = abs(XF)<Gap;
        Filter(Gap) = 0;
    end
    Filter = Filter./sum(Filter);
    Filter = fftshift(Filter);
    Nx     = size(X,1);
    MeanFiltX  = ifft(fft(X, [], 1) .* conj(fft(Filter, Nx, 1)));
    
    XX = (X - MeanFiltX).^2;
    VarFiltX  = ifft(fft(XX, [], 1) .* conj(fft(Filter, Nx, 1)));
    StdX      = real(sqrt(VarFiltX));
    
    
end
