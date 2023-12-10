function [Result] = findPeaks(Data, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2023 Dec) 
    % Example: 
    %          
    %          AS=AstroSpec.getSkyArcsSpecLines;
    %          Data = AS(1).Flux;
    %          timeSeries.peaks.findPeaks(Data)

    arguments
        Data
        Args.DimWave           = 1
        Args.Filter            = @(X, Sigma) normpdf(X, 0, Sigma);
        Args.FilterArgs        = {1.5};  % sigma of filter
        Args.X                 = (-10:1:10)';
        Args.StdFilterGap      = [10 20];
        Args.Threshold         = 10;
    end
        
    % make sure that the wavelengh is in the first dim
    if Args.DimWave==2
        Data = Data.';
    end
    
    % filter the data
    if isa(Args.Filter, 'function_handle')
        Filter = Args.Filter(Args.X(:), Args.FilterArgs{:});
    else
        Filter = Args.Filter(:);
    end
    Filter = Filter./sum(Filter);
    Norm   = sqrt(sum(Filter.^2));
    
    Nwave = size(Data,1);
    Filter = fftshift(Filter);  % move to begining to avoid shift
    FiltData = (ifft(fft(Data,[],1).*conj(fft(Filter,Nwave,1))));

    StdF = timeSeries.filter.filterStd(Data);
    
    
    Stat      = FiltData./StdF;
    error('not working')
    
    Result.Max  = find(Stat>Args.Threshold);
    Result.MaxS = Stat(Result.Max);
    Result.MaxV = Data(Result.Max);
    
    Result.Min  = find(Stat<-Args.Threshold);
    Result.MinS = Stat(Result.Min);
    Result.MinV = Data(Result.Min);
    
    Result.Stat = Stat;    
    
end
