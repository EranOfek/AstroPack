function [Result,FreqVec] = perid_FalseAlarmRedNoise(Time, Flux, Args)
    % Return the simulations of the power-spectrum of a lgight curve with a red-power-law spectrum.
    %     The simulations have a user specified power-law index and their
    %     std is equal to that of the std of the flux of the observations.
    %     The power spectrum is calculated given the window function of the
    %     observations.
    % Input  : - Vector of times. If empty, then run in simulation mode.
    %          - Vector of fluxs.
    %          * ...,key,val,... 
    %            'PL' - Minus the power law index. Default is 3.
    %            'Nsim' - Number of simulated power spectra.
    %                   Default is 1000.
    %            'FreqVec' - Vector of frequencies in which to calculate
    %                   the power spectrum. If empty, then use the
    %                   timeSeries.period.getFreq function.
    %                   Default is [].
    % Output : - A matrix of power spectra. Column per simulation. Raw per
    %            frequency.
    %          - A vector of frequencies.
    % Author : Eran Ofek (2025 Feb) 
    % Example: [PS,F]=timeSeries.period.perid_FalseAlarmRedNoise;
    %          loglog(F,max(PS,[],2))
    %          loglog(F,quantile(PS,0.95,2))
    
    arguments
        Time                   = [];
        Flux                   = [];
        Args.PL                = 3;
        Args.Nsim              = 1000;
        Args.FreqVec           = [];
    end

    if isempty(Time)
        % simulation mode
        
        Time  = (1:100)';
        Nfreq = numel(Time);
        F_w   = timeSeries.timeDelay.rand_psd(Nfreq,Args.PL);
        Flux  = ifft(F_w).*1000 + 1000;
    end
    
    [Time, SI] = sort(Time);
    Flux       = Flux(SI);
    Time       = Time - Time(1);
    Flux       = Flux - mean(Flux);
    
    MaxFreq = 0.5./min(diff(Time));
    MinFreq = 0.5./range(Time);
    Nfreq   = 2.*ceil(MaxFreq./MinFreq);
    
    if isempty(Args.FreqVec)
        FreqVec = timeSeries.period.getFreq(Time);
    else
        FreqVec = Args.FreqVec;
    end
    StdFlux = std(Flux);
    
    Result = zeros(numel(FreqVec), Args.Nsim);
    for Isim=1:1:Args.Nsim
        F_w = timeSeries.timeDelay.rand_psd(Nfreq,Args.PL);
        RandFlux = ifft(F_w);
        InterpFlux = interp1((0:1:Nfreq-1).', RandFlux, Time);
        InterpFlux = InterpFlux.*StdFlux./std(InterpFlux);
        
        PS = timeSeries.period.period([Time, InterpFlux], FreqVec);
        Result(:,Isim) = PS(:,2);
        
    end
            
end
