function PS=periodFoldedMatchedFilter(LC, FreqVec, Args)
    % Matched filter periodicity search for unevenly spaced light curves.
    %   Given a time series and a template bank, for each trial period,
    %   fold the data and attempt to fit the template bank to the folded
    %   light curve.
    %   Uses timeSeries.filter.fitFilter1D with the Method='filt' option.
    % Input  : - An array containing a light curve [Time, Flux, Err].
    %            Should contain at least 3 columns.
    %            If empty, use simulation mode.
    %            Default is [].
    %          - A vector of frequencies to test (with out zero frequency).
    %          * ...,key,val,...
    %            'FilterTime' - A vector of times at which the Filter is
    %                   given.
    %            'FilterVal' - An array of filter values. Each column
    %                   corresponds to one filter.
    %            'DelayStep' - Delay step used if 'Delay' is empty.
    %                   Default is 1.
    %            'MeanFun' - Function to use in order to calculate the mean
    %                   of the time series. This mean will be subtracted
    %                   prior to fitting.
    %                   If empty, then do not subtract mean.
    %                   Default is @median
    %            'InterpMethod' - Default is 'linear'.
    % Output : - An array with the following columns:
    %            [Freq, max(abs(stat)), IndPhase, IndFilt]
    % Author : Eran Ofek (Oct 2023)
    % Example: PS = timeSeries.period.periodFoldedMatchedFilter; % simulation mode
    
    arguments
        LC                    = [];
        FreqVec               = [];
        
        Args.FilterTime
        Args.FilterVal
        Args.DelayStep        = 1;
        Args.MeanFun          = @median;
        Args.InterpMethod     = 'linear';
        
        Args.ColF     = 2;
        Args.ColE     = 3;
        Args.Verbose logical  = false;
    end
    
    if isempty(LC)
        % simulation mode
        
        Npt    = 1000;
        Trange = 1000;
        Period = 9;
        Err    = 0.1;
        T = (1:1:Trange).';
        F = zeros(Trange,1);
        Peaks = (1:Period:Trange)';
        F(Peaks) = 1;
       
        Template = normpdf((-10:1:10)',0,2);
        F        = conv(F, Template, 'same');
        
        TS = rand(Npt,1).*Trange;
        FS = interp1(T, F, TS, 'cubic');
        FS = FS + randn(Npt,1).*Err;
        
        LC = [TS, FS, Err.*ones(Npt,1)];
        FreqVec = (0.1:0.0005:1)';
        
        Args.FilterTime = (-10:0.1:10).';
        Args.FilterVal  = normpdf(Args.FilterTime,0,[1:0.5:3]);
        Args.DelayStep  = 0.1;
        
    end
    
    T = LC(:,1);
    
    Nfreq = numel(FreqVec);
    PS    = [FreqVec(:), zeros(Nfreq, 3)];
    for Ifreq=1:1:Nfreq
        if Args.Verbose
            if Ifreq./1000==floor(Ifreq./1000)
                [Ifreq, Nfreq]
            end
        end
        TF      = T.*FreqVec(Ifreq);
        % Vector of phases for trial frequency
        PhaseTF = (TF - floor(TF));
       
        PhaseTime = PhaseTF./FreqVec(Ifreq);
        
        Result = timeSeries.filter.fitFilter1D(PhaseTime,LC(:,Args.ColF), LC(:,Args.ColE),...
                                               'FilterTime',Args.FilterTime,...
                                               'FilterVal',Args.FilterVal,...
                                               'Delay',[],...
                                               'DelayStep',Args.DelayStep,...
                                               'Method','filt',...
                                               'MeanFun',Args.MeanFun,...
                                               'InterpMethod',Args.InterpMethod);
        %
        
        [MaxS, MaxInd] = max(abs(Result.Stat(:)));
%         if isnan(MaxS)
%             'a'
%         end
        [IndPhase, IndFilter] = imUtil.image.ind2sub_fast(size(Result.Stat), MaxInd);
        
        PS(Ifreq,2:4) = [MaxS, IndPhase, IndFilter];
       
    end
    
end
