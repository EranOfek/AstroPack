function [PS,PSextra]=periodFoldedEvents(T, FreqVec, Args)
    % Search periodicity in time tagged events by folding and sin fitting
    % Input  : - A vector of time tagged events
    %          - A vector of trial frequncies to test.
    %          * ...,key,val,...
    %            'BinSize' - Bin size in phase folding. Default is 0.1.
    % Output : - A column matrix of [Freq, Amplitude, Mean, A, B]
    % Author : Eran Ofek (Oct 2023)
    % Example: PS=timeSeries.period.periodFoldedEvents % simulation mode
    
    arguments
        T                    = [];
        FreqVec              = [];
        Args.ExtraVal        = [];
        Args.BinSize         = 0.1;
        Args.Verbose logical = false;
    end
    
    if isempty(T)
        % simulation mode

        FreqVec = (0:0.00005:0.1);
        %PS = timeSeries.period.periodPoisson(TT,FreqVec);
    %
        Time     = (1:1:1e5)';
        Lambda0  = 0.1;
        A        = 0.1;
        FreqPeak = 0.01;
        LambdaT  = Lambda0.*(1 + A.*sin(2.*pi.*FreqPeak.*Time));
        Cnt      = poissrnd(LambdaT);
        FlagT    = Cnt>0;
        T       = Time(FlagT);
        %numel(T)    

    end
    
    T = T(:) - min(T);
    FreqVec = FreqVec(:).';
    Nfreq   = numel(FreqVec);
    Edges   = (0:Args.BinSize:1);
    MidBin  = (Edges(1:end-1) + Edges(2:end)).*0.5;
    
    Nbin = numel(MidBin);
    H = [ones(Nbin,1), sin(2.*pi.*MidBin(:)), cos(2.*pi.*MidBin(:))];
    
    PS      = [FreqVec(:), zeros(Nfreq,4)];
    PSextra = [FreqVec(:), zeros(Nfreq,4)];
    for Ifreq=1:1:Nfreq
        if Args.Verbose
            if Ifreq./1000==floor(Ifreq./1000)
                [Ifreq, Nfreq]
            end
        end
        TF      = T.*FreqVec(Ifreq);
        % Vector of phases for trial frequency
        PhaseTF = (TF - floor(TF));
       
        % bin
        %N = histcounts(PhaseTF, Edges);
        N = matlab.internal.math.histcounts(PhaseTF, Edges);
        %PS(Ifreq,2) = std(N)./sqrt(mean(N));
        
        if ~isempty(Args.ExtraVal)
            %
            B = timeSeries.bin.binning([PhaseTF, Args.ExtraVal], Args.BinSize,[0 1],{'MeanBin',@median,@numel});
            F = B(:,3)>0;
            Par = H(F,:)\B(F,2);
            PSextra(Ifreq,2:5) = [sqrt(Par(2).^2 + Par(3).^2), Par(:).'];
        end

        Par = H\N(:);
        %Amp = sqrt(Par(2).^2 + Par(3).^2);
        PS(Ifreq,2:5) = [sqrt(Par(2).^2 + Par(3).^2), Par(:).'];
        
    end
    
    
    
end
