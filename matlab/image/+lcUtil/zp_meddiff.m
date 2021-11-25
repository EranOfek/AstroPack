function zp_meddiff(MS, Args)
    %
   
    
    arguments
        MS(1,1) MatchedSources
        Args.MagField char          = 'MAG';
        Args.MagErrField char       = 'MAGERR';
        
        Args.MaxMagErr              = 0.03;
        
        Args.MinNepoch              = Inf;  % Inf - source appear in all epochs
        Args.Niter                  = 2;
                
        Args.Plot(1,1) logical      = false;
    end
    
    
    Mag    = getMatrix(MS, Args.MagField);
    MagErr = getMatrix(MS, Args.MagErrField);
    
    MedMagErr = median(MagErr, 1, 'omitnan');
    FlagMM    = MedMagErr<Args.MaxMagErr;
    Mag       = Mag(:,FlagMM);
    MagErr    = MagErr(:,FlagMM);
    
    [Nep, Nsrc] = size(Mag);
    
    % select sources with minimum number of observations
    NdetPerSrc = sum(~isnan(Mag),1);
    
    Args.MinNepoch = min(Nep, Args.MinNepoch);
    
    FlagMin    = NdetPerSrc>Args.MinNepoch;

    Mag    = Mag(:,FlagMin);
    MagErr = MagErr(:,FlagMin);

    [Nep, Nsrc] = size(Mag);
    
    DiffMagEpoch = diff(Mag, 1, 1);
    DZP = [0; median(DiffMagEpoch, 2)];
        
    
end