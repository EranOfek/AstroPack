function PS=periodPoisson(TT, FreqVec, Args)
    % Power spectrum (periodogram) for time-tagged events with Poisson statistics.
    %   timeSeries.period.periodFoldedEvents is better and faster
    % Input  : - Either a vector of time tagged events, or a two column
    %            matrix with [MidTime, CountsInBin]
    %          - A vector of frequencies to test.
    %          * ...,key,val,...
    %            'MinT' - Minimum Time. If [], take min of time vector.
    %                   Default is [].
    %            'MaxT' - Maximum Time. If [], take max of time vector.
    %                   Default is [].
    %            'BinT' - Tim bin. Defualt is 1.
    %            'MeanLambda' - Expectation number of counts per bin.
    %                   If [], then take the mean.
    %                   Default is [].
    % Output : - Power spectrunm [Freq, Power]
    % Author : Eran Ofek (Oct 2023)
    % Example: 
    %          PS = timeSeries.period.periodPoisson; % simulation mode
    %
    
    arguments
        TT          = [];
        FreqVec     = [];
        Args.MinT   = [];
        Args.MaxT   = [];
        Args.BinT   = 1;
        
        Args.MeanLambda = [];
    end
    
    if isempty(TT)
        % simulation mode
        
        FreqVec = (0:0.00005:0.1);
        %PS = timeSeries.period.periodPoisson(TT,FreqVec);
    %
        Time     = (1:1:1e5)';
        Lambda0  = 0.1;
        A        = 0.5;
        FreqPeak = 0.01;
        LambdaT  = Lambda0.*(1 + A.*sin(2.*pi.*FreqPeak.*Time));
        Cnt      = poissrnd(LambdaT);
        FlagT    = Cnt>0;
        TT       = Time(FlagT);
        %numel(TT)    
        
    end
    
    FreqVec = FreqVec(:).';
    
    T = TT(:);
    
%     if size(TT,2)==1
%         % T is a vector of time taggs
%         
%         if isempty(Args.MinT) || isempty(Args.MaxT)
%             Args.MinT = min(T);
%             Args.MaxT = max(T);
%         end
%         
%         Edges  = [Args.MinT:Args.BinT:Args.MaxT];
%         MidBin = (Edges(1:end-1) + Edges(2:end)).*0.5; 
%         Ncnt   = histcounts(T, Edges);
%         TT     = [MidBin(:), Ncnt(:)];
%     
%     else
%         % T is [T, Count] two column matrix
%         Args.BinT = TT(2,1) - TT(1,1);
%     end
%     RangeT = TT(end,1) - TT(1,1)  + Args.BinT;
%     Ntot   = size(TT,1);
%     T      = TT(:,1);   % mid time of each bin
%     K      = TT(:,2);   % Observed counts per bin
    
%     if isempty(Args.MeanLambda)
%         Lambda0 = Ntot./RangeT;
%     else
%         Lambda0 = Args.MeanLambda;
%     end
    
    A = 1;
    Lambda0 = 1;
    G     = Lambda0.*A.*exp(-2.*pi.*1i.*FreqVec.*T);
    % Formally replace 0 with G, but because G does not depand on data (K)
    % then doesn't matter, except 0 frequency.
    K = 1;
    Power = abs(sum(0 - K.*log(G), 1)).^2;  
    PS    = [FreqVec(:), Power(:)];
        
    
    
    
end

