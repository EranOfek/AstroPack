function [FreqVec, Result] = getFreq(T, Args)
    % Given times, get a frequency vector, over which it is recommended to search for periodicity
    % Input  : - A vector of times.
    %          * ...,key,val,... 
    %            'OverSampling' - Frequency over sampling. Default is 2.
    %            'OverNyquist' - Multiply the highest frequency by this
    %                   number. Defaut is 0.5.
    %            'StartWith0' - Logical indicaring if lowest frequency
    %                   is 0. Default is true.
    %            'DiffFun' - Function handle to use for calculating the
    %                   mean diff. Default is @median.
    % Output : - A vector of frequencies.
    %          - A structure with the following fields:
    %            .MinFreq
    %            .DeltaFreq
    %            .MaxFreq
    % Author : Eran Ofek (2024 Feb) 
    % Example: [FreqVec, Result] = timeSeries.period.getFreq(rand(100,1));

    arguments
        T
        Args.OverSampling       = 2;
        Args.OverNyquist        = 0.5;
        Args.StartWith0 logical = true;
        Args.DiffFun            = @median;
    end


    TimeSpan = range(T);

    Result.DeltaFreq = 1./(Args.OverSampling.*TimeSpan);

    if Args.StartWith0
        Result.MinFreq = 0;
    else
        Result.MinFreq = DeltaFreq;
    end

    TypicalDiff = Args.DiffFun(diff(sort(T)));

    Result.MaxFreq = Args.OverNyquist./TypicalDiff;
    
    FreqVec = (Result.MinFreq:Result.DeltaFreq:Result.MaxFreq).';
    
end
