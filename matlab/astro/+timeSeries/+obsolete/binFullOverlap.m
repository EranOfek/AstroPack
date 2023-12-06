function B=binFullOverlap(LC, ExpTime)
    % Bin light curve with finite ExpTime using only points fully covering
    % the bins.
    % Input  : - LC [T, Mag, Err]
    %          - ExpTime.
    % Output : - Binned LC [MidT, MeanT, MeanMag, MedianMag, StdMag, ErrMag, N]
    % Author : Eran Ofek (Dec 2022)
    
    arguments
        LC
        ExpTime
    end
    
    T  = LC(:,1);
    M  = LC(:,2);
    E  = LC(:,3);
    
    Tstart = T - ExpTime.*0.5;
    Tend   = T + ExpTime.*0.5;
    
    EdgeT  = Tstart;
    Nbin   = numel(EdgeT) - 1;
    
    B = nan(Nbin,7);
    for Ibin=1:1:Nbin
        Flag = Tstart<EdgeT(Ibin) & Tend>EdgeT(Ibin+1);
        
        
        N     = sum(Flag);
        MidT  = (EdgeT(Ibin) + EdgeT(Ibin+1)).*0.5;
        MeanT = mean(T(Flag));
        MeanM = mean(M(Flag));
        MedM  = median(M(Flag));
        StdM  = std(M(Flag));
        ErrM  = StdM./sqrt(N);
        
        B(Ibin,:) = [MidT, MeanT, MeanM, MedM, StdM, ErrM, N];
    end
    
    
    
    
end
