function zp_lsq(MS, Args)
    %
    % Example: Fzp   = 1 + rand(100,1);
    %          Fstar = rand(1,200).*3900 + 100; 
    %          Flux = Fzp.*Fstar;
    %          Flux = poissrnd(Flux);
    %          FluxErr = sqrt(Flux);
    %          Mag     = 22-2.5.*log10(Flux);
    %          MagErr  = 1.086.*FluxErr./Flux;
    %          MS = MatchedSources;
    %          MS.addMatrix({Mag, MagErr},{'MAG','MAGERR'});
   
    arguments
        MS MatchedSources
        Args.MagField char          = 'MAG';
        Args.MagErrField char       = 'MAGERR';
        
        
        
    end
    
    
    
end