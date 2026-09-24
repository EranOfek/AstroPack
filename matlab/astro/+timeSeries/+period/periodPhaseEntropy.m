function [PhaseEntropy,MPE] = periodPhaseEntropy(Time, Freq, Args)
    % The phase entropy of a time series as a function of frequency
    %   The phase entropy is defined as sum(-p*log(p)), where p is the
    %   probability density of phases for each trial frequency.
    %   This gives indications for trial periods that doesn't have good
    %   phase coverage.
    % Input  : - A vector of times.
    %          - A vector of trial frequencies.
    %          * ...,key,val,... 
    %            'Nbins' - Number of bins when calculating the phase
    %                   histogram for each trial frequency.
    %                   Default is 10.
    %            'Eps' - A small number to add to the histogram counts in
    %                   order to avoid log(0).
    %                   Default is 1e-10
    % Output : - Entropy for each trial frequency.
    %          - Median of the phase entropy over all frequencies.
    % Author : Eran Ofek (2026 Mar) 
    % Example: Time = (1:1:1500)' + randn(1500,1).*0.1;
    %          PM=Time./29.53 - floor(Time./29.53);               
    %          PS=Time./365.24219 - floor(Time./365.24219);       
    %          F=PM<0.9 & PS>0.5;                                      
    %          Time=Time(F);
    %          Freq=(0:1./3000:1)';
    %          PhaseEntropy=timeSeries.period.periodPhaseEntropy(Time,Freq)

    arguments
        Time
        Freq
        Args.Nbins         = 10;
        Args.Eps           = 1e-10;
    end

    Nfreq = numel(Freq);
    PhaseEntropy = zeros(Nfreq,1);
    for Ifreq=1:1:Nfreq
        %Phase = mod(Time, 1./Freq(Ifreq)).*Freq(Ifreq); % slowest
        %Tmp = Time.*Freq(Ifreq);
        %Phase = Tmp - floor(Tmp);
        Phase = Time.*Freq(Ifreq) - floor(Time.*Freq(Ifreq)); % faster
        
        % calculate the phase entropy:
        [Pbin] = tools.hist.mex.hist1reg_mex(Phase, [0 1], Args.Nbins);
        PhaseEntropy(Ifreq) = sum(-Pbin.*log(Pbin + Args.Eps));

    end

    if nargout>1
        MPE = median(PhaseEntropy);
    end
end
