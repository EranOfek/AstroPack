function Result=fitFilter1D(T,F,Err,Args)
    % Matched filter fitting of a 1-D time series with a template bank.
    %   Given an unevenly spaced time series and a set of filters,
    %   interpolate the filters over the time series and calculate the
    %   \chi^2 for each filter against the time series.
    % Input  : - A vector of times for the time series.
    %            If empty, then will run in simulation mode.
    %          - A vector of values for the time series.
    %          - A scalar or a vector of errors for the time series.
    %            If empty, then will use the tools.math.stat.rstd
    %            function to estimate the error.
    %          * ...,key,val,...
    %            'FilterTime' - A vector of times at which the Filter is
    %                   given.
    %            'FilterVal' - An array of filter values. Each column
    %                   corresponds to one filter.
    %            'Delay' - A vector of time delays on which to apply the
    %                   filters. If empty, then will use:
    %                   (0:Args.DelayStep:RangeT), where RangeT is the range of times.
    %                   Default is [].
    %            'DelayStep' - Delay step used if 'Delay' is empty.
    %                   Default is 1.
    %            'Method' - Either \chi^2 fitting 'chi2' or filtering
    %                   'filt'. Default is 'filt'.
    %            'MeanFun' - Function to use in order to calculate the mean
    %                   of the time series. This mean will be subtracted
    %                   prior to fitting.
    %                   If empty, then do not subtract mean.
    %                   Default is @median
    %            'InterpMethod' - Default is 'linear'.
    % Output  : - A structure with the following fields:
    %             .Stat - A matrix of \chi^2 or score statistics values.
    %                   Statistic values are currently not normalized.
    %                   Different columns
    %                   corresponds to different filters, while raw
    %                   corresponds to different delays.
    %             .Dof - Number of degrees of freedom in the fit.
    %                   Not taking into account edges.
    %             .Delay - A column vector of time delay
    % Example : Result=timeSeries.filter.fitFilter1D;  % simulation mode
    
    arguments
        T                     = [];
        F                     = [];
        Err                   = [];
        Args.FilterTime
        Args.FilterVal
        Args.Delay            = [];
        Args.DelayStep        = 1;
        Args.Method           = 'filt';
        Args.MeanFun          = @median;
        Args.InterpMethod     = 'linear';
    end
    
    if isempty(T)
        % simulation mode
        
        Npt = 5000;
        Err = 0.1;
        RangeT = 1000;
        T = rand(Npt,1).*RangeT;
        T = sort(T);
        F = normpdf(T-50, 0, 2);
        F = F + Err.*randn(Npt,1);
        
        Args.FilterTime = (-3:0.2:3).';
        Args.FilterVal  = normpdf(Args.FilterTime,0,[1:0.1:3]);
        Args.Delay      = (0:0.5:RangeT);
    end
        
    Npt    = numel(T);
    if isempty(Err)
        % estimate error from rstd
        Err = tools.math.stat.rstd(F);
        Npar = 1;
        
    else
        Npar = 0;
    end
    T = T(:);
    F = F(:);
    Err = Err(:);
    
    T      = T - min(T);
    
    if isempty(Args.MeanFun)
        Npar = Npar;
    else
        F      = F - Args.MeanFun(F, 1, 'omitnan');
        Npar   = Npar + 1;
    end
    
    if isempty(Args.Delay)
        RangeT = range(T);
        Args.Delay = (0:Args.DelayStep:RangeT);
    end
    
    
    
    Nfilt  = size(Args.FilterVal,2);
    
    Ndelay        = numel(Args.Delay);
    Result.Stat   = zeros(Ndelay, Nfilt);
    Result.NotNaN = zeros(Ndelay, Nfilt);
    %Result.Dof    = zeros(1, Nfilt);
    
    switch lower(Args.Method)
        case 'filt'
            % filtering
            %NormS  = (sum(Args.FilterVal.^2, 1));
            ErrN = Err;   %./NormS;
            %NormS = zeros(Ndelay, Nfilt);
            for Idelay=1:1:Ndelay
                
                InterpFilter            = interp1(Args.FilterTime(:)+Args.Delay(Idelay), Args.FilterVal, T, Args.InterpMethod, NaN);
                InterpFilter            = InterpFilter./sum(InterpFilter, 1, 'omitnan');
                Result.Stat(Idelay,:)   = sum(InterpFilter.*F./(Err.^2), 1, 'omitnan');
                Result.NotNaN(Idelay,:) = sum(~isnan(InterpFilter), 1);
                %NormS(Idelay,:)         = sqrt(sum(InterpFilter.^2, 1, 'omitnan'));
            end
            Result.Stat   = Result.Stat; %./NormS;
            
            %tools.math.stat.rstd(Result.Stat(1:10:end,:),1); % *NormS;
            
            
        case 'chi2'
            % chi^2 fitting
            for Idelay=1:1:Ndelay
                In  terpFilter          = interp1(Args.FilterTime(:)+Args.Delay(Idelay), Args.FilterVal, T, Args.InterpMethod, NaN);
                %Result.Dof(Idelay,:)  = sum(~isnan(InterpFilter),1) - Npar;
                Result.Stat(Idelay,:) = sum(((InterpFilter - F)./Err).^2, 1, 'omitnan');
            end
        otherwise
            error('Unknown Method option');
    end
    
    Result.Dof   = Npt - Npar;
    Result.Delay = Args.Delay(:);
    
end
