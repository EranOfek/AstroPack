function [Result] = period_EntropyConsistent(Time, Mag, FreqVec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Mar) 
    % Example: timeSeries.period.period_EntropyConsistent

    arguments
        Time
        Mag
        FreqVec                = [];
        Args.MagErr            = 0.1;
        Args.Nsigma            = 3;
        Args.MagStep           = 0.01;
        Args.PhaseStep         = 0.1;
    end
    
    error('Not ready')
    
    if isempty(FreqVec)
        [FreqVec] = timeSeries.period.getFreq(Time);
    end
    
    MinMag = min(Mag);
    MaxMag = max(Mag);
    Nobs   = numel(Mag);
    
    VecMag   = (MinMag:Args.MagStep:MaxMag).';
    VecPhase = (0:Args.PhaseStep:1);
    MagCenter   = (VecMag(1:end-1) + VecMag(2:end)).*0.5;
    PhaseCenter = (VecPhase(1:end-1) + VecPhase(2:end)).*0.5;
    Nphase      = numel(PhaseCenter);
    Nmag        = numel(MagCenter);
    
    Lambda = Nobs./(Nphase.*Nmag)
    
    MagDistMat  = MagCenter - MagCenter.';
    ExpDistMat  = exp(-(MagDistMat./2.*Args.MagErr).^2);
    ExpDistMat  = triu(ExpDistMat,1);
    
    MagFilter   = (-Args.Nsigma.*Args.MagErr:Args.MagStep:Args.Nsigma.*Args.MagErr).';
    Filter      = normpdf(MagFilter, 0, Args.MagErr);
    Filter      = Filter./sum(Filter);
    
    
    Nfreq = numel(FreqVec);
    E     = zeros(Nfreq, 1);
    EP    = zeros(Nfreq, 1);
    for Ifreq=1:1:Nfreq
        Temp   = Time.*FreqVec(Ifreq);
        Phase  = Temp - floor(Temp);
        
        % N = double(tools.hist.histcounts2regular_mex(Phase, Mag, VecPhase, VecMag, true));
        N2 = histcounts2(Mag, Phase, VecMag, VecPhase);
        Mu = N2./Nobs;
        
        %W_Mat = ExpDistMat.*N2;
        %W_Mat(W_Mat==0) = NaN;
        
        MMF = zeros(size(N2));
        for Iphase=1:1:Nphase
            MM = MagCenter.*N2(:,Iphase);
            Back        = mean(N2(:,Iphase));
            PoissFilter = log(1 + Filter./Back);
            MMF(:,Iphase) = conv(N2(:,Iphase), PoissFilter, 'same');            
        end
        MMF = MMF./sum(MMF,'all','omitnan');
        
        %E(Ifreq) = -sum(MMF.*log(MMF + eps), 'all', 'omitnan');
        
        In0 = find(MMF~=0);
        E(Ifreq) = -sum(MMF(In0).*log(MMF(In0)), 'all', 'omitnan');
        
        SumMMF = sum(MMF, 1, 'omitnan');
        EP(Ifreq) = -sum(SumMMF.*log(SumMMF+eps));
    end
    
    E  = E./log(numel(MMF));
    EP = EP./log(numel(SumMMF));
    
    Result = [FreqVec, E, EP];
    
end
