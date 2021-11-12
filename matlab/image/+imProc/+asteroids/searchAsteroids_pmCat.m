function searchAsteroids_pmCat(CatPM, BitDict, Args)
    %
    % Example : imProc.asteroids.searchAsteroids_pmCat(MergedCat, AllSI(1).MaskData.Dict, 'ExpTime',range(JD), 'PM_Radius',3./3600)
    
    arguments
        CatPM AstroCatalog
        BitDict(1,1) BitDictionary
        Args.Images AstroImage
        
        Args.ColNamePM_RA                 = 'PM_RA';
        Args.ColNamePM_Dec                = 'PM_Dec';
        Args.ColNamePM_TdistProb          = 'PM_TdistProb';
        Args.ColNameNobs                  = 'Nobs';
        Args.ColNameFlags                 = 'FLAGS';
        
        Args.RemoveBitNames               = {'Saturated', 'Spike', 'CR_DeltaHT', 'CR_Laplacian', 'CR_Streak', 'Streak', 'Ghost', 'Persistent', 'NearEdge'};
        Args.ExpTime                      = [];  % same units as PM time
        Args.PM_Radius                    = [];  % same units as the PM
        
    end
    
    %[MergedCat, MatchedS, Result] = pipeline.generic.mergeCatalogs(AllSI)
    
    Ncat = numel(CatPM);
    for Icat=1:1:Ncat
        % select columns from CatPM
        
    
        PM           = CatPM(Icat).getCol({Args.ColNamePM_RA, Args.ColNamePM_Dec});
        PM_TdistProb = CatPM(Icat).getCol(Args.ColNamePM_TdistProb);
        Nobs         = CatPM(Icat).getCol(Args.ColNameNobs);
        DecFlags     = CatPM(Icat).getCol(Args.ColNameFlags);
        
        TotPM        = sqrt(sum(PM.^2, 2));  % total PM [deg/day]
        ExpectedNobs = TotPM.*Args.ExpTime./(0.5.*Args.PM_Radius);
        
        % remove sources with some selected flags
        Flags(Icat).FLAGS  = ~findBit(BitDict, DecFlags, Args.RemoveBitNames, 'Method','any');
        
        Flags(Icat).Tdist  = ((PM_TdistProb > 0.995 & Nobs>5) | (PM_TdistProb>0.9999 & Nobs>3));
                
        Flags(Icat).Nobs   = Nobs>(0.9.*ExpectedNobs);
        
        Flags(Icat).All    = Flags.FLAGS & Flags.Tdist & Flags.Nobs;
        
    end
        
    
    %          I = 1; AA=MergedCat(I).toTable; Flag = (AA.PM_TdistProb>0.999 & AA.Nobs>5) | (AA.PM_TdistProb>0.9999 & AA.Nobs>3); remove near edge..., 
    % check that motion is consistent with Nobs sum(Flag)
    %          ds9(AllSI(1,I), 1); 
    %          ds9(AllSI(end,I), 2); 
    %          ds9.plot(MergedCat(I).Catalog(Flag,1:2),'o','Coo','fk5')
    
    
end

