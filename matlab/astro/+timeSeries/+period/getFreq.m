function [FreqVec, Result] = getFreq(T, Freq, Args)
    % Given times, get a frequency vector, over which it is recommended to search for periodicity
    % Input  : - A vector of times.
    %          - Optional Frequency vector. If provided, then this will be
    %            returned as an output. Default is [].
    %          * ...,key,val,... 
    %            'OverSampling' - Frequency over sampling. Default is 2.
    %            'OverNyquist' - Multiply the highest frequency by this
    %                   number. Defaut is 0.5.
    %            'StartWith0' - Logical indicaring if lowest frequency
    %                   is 0. Default is true.
    %            'DiffFun' - Function handle to use for calculating the
    %                   mean diff. Default is @median.
    %            'MaxFreq' - Maximum Freuency. Default is [].
    % Output : - A vector of frequencies.
    %          - A structure with the following fields:
    %            .MinFreq
    %            .DeltaFreq
    %            .MaxFreq
    % Author : Eran Ofek (2024 Feb) 
    % Example: [FreqVec, Result] = timeSeries.period.getFreq(rand(100,1));

    arguments
        T
        Freq                    = [];
        Args.OverSampling       = 2;
        Args.OverNyquist        = 0.5;
        Args.StartWith0 logical = true;
        Args.DiffFun            = @median;
        Args.MaxFreq            = [];
    end


    if isempty(Freq)
        TimeSpan = range(T);
    
        Result.DeltaFreq = 1./(Args.OverSampling.*TimeSpan);
    
        if Args.StartWith0
            Result.MinFreq = 0;
        else
            Result.MinFreq = DeltaFreq;
        end
    
        TypicalDiff = Args.DiffFun(diff(sort(T)));
    
        if ~isempty(Args.MaxFreq)
            Result.MaxFreq = Args.MaxFreq;
        else
            Result.MaxFreq = Args.OverNyquist./TypicalDiff;
        end
        
        FreqVec = (Result.MinFreq:Result.DeltaFreq:Result.MaxFreq).';
    else
        % construct from FreqVec
        FreqVec = Freq;
        Result  = [];

    end
    
end
