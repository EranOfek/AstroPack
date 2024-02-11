function [PS] = period_nufft(Data,FreqVec,Norm,SubMean)
    % Normalized classical power spectrum calculated using nufft
    %     Return results equivalent to period_normnl, but much faster.
    % Input  : - Two column matrix containing the time series
    %            [Time, measurment] or [Time, measurment, error].
    %          - Frequency range and interval in which to calculate the
    %            power spectrum.
    %            This is a column vector of frequencies at which to
    %            calculate the power spectrum.
    %          - Normalization method:
    %            'Var' - Normalize by variance (Default).
    %            'Amp' - Normalize by amplitude.
    % Output : - Two columns matrix of the un-normalized power spectrum
    %            [frequency, power_spec].
    % Author : Eran Ofek (2024 Feb) 
    % Example: x=rand(1000,1).*1000; y=randn(1000,1)+sin(2.*pi.*x./22);
    %          F=(0:0.00001:1)';
    %          PS1=timeSeries.period.period_nufft([x,y],F);


    arguments
        Data
        FreqVec
        Norm              = 'Var';
        SubMean logical   = true;
    end
    

    Col.T = 1;
    Col.M = 2;
    T       = Data(:,Col.T);
    N       = numel(T);

    if (SubMean)
        M       = Data(:,Col.M) - mean(Data(:,Col.M));
    else
        M       = Data(:,Col.M);
    end

    
    PS = [FreqVec(:), abs(nufft(M,T,FreqVec)).^2./N];

    switch lower(Norm)
     case 'amp'
        % do nothing
     case 'no'
        PS(:,2) = PS(:,2).*N;
     case 'var'
        PS(:,2) = PS(:,2)./var(M);
     otherwise
        error('Unknwon normalization option');
    end

end
