function [Result] = findLines(Data, Args)
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
    %          imUtil.spec.lines.findLines(Data)

    arguments
        Data
        Args.DimWave           = 1
        Args.Filter            = @(X, Sigma) normpdf(X, 0, Sigma);
        Args.FilterArgs        = {1.5};  % sigma of filter
        Args.X                 = (-10:1:10)';
        Args.StdFilterGap      = [10 20];
        Args.Threshold         = 10;
    end
    
    error('not working')
    
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
    
    Nwave = size(Data,1);
    Filter = fftshift(Filter);  % move to begining to avoid shift
    FiltData = (ifft(fft(Data,[],1).*conj(fft(Filter,Nwave,1))));

    % Std filter around each poistion
    HalfSizeFilterStd = max(Args.StdFilterGap);
    Xstd              = (-HalfSizeFilterStd:1:HalfSizeFilterStd).';
    StdFilt           = zeros(size(Xstd));
    Flag1             = abs(Xstd)>min(Args.StdFilterGap) & abs(Xstd)<=max(Args.StdFilterGap);
    StdFilt(Flag1)    = 1;
    StdFilt           = StdFilt./sum(StdFilt);
    StdFilt           = fftshift(StdFilt);
    
    FiltStdX2 = (ifft(fft(Data.^2,[],1).*conj(fft(StdFilt,Nwave,1))));
    FiltStdX1 = (ifft(fft(Data,[],1).*conj(fft(StdFilt,Nwave,1))));
    FiltStd   = FiltStdX2 - FiltStdX1.^2;
    Stat      = FiltData./sqrt(FiltStd);
    
    Result.Max  = find(Stat>Args.Threshold);
    Result.MaxS = Stat(Result.Max);
    Result.MaxV = Data(Result.Max);
    
    Result.Min  = find(Stat<-Args.Threshold);
    Result.MinS = Stat(Result.Min);
    Result.MinV = Data(Result.Min);
    
    Result.Stat = Stat;    
    
end
