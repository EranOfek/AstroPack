function [SucessFlag, QualitySummary] = assessAstrometricQuality(ResFit, Args)
    %
    
    arguments
        ResFit struct
        Args.MinNumSrc            = 10;
        Args.MinFracMatches       = ?
        Args.MinRMS               = 0.3./3600;    % deg
        Args.MinAsymRMS           = 0.2./3600;    % deg
        Args.MinErrOnMean         = 0.01./3600;   % deg
    end
    
    Nfit = numel(ResFit);
    for Ifit=1:1:Nfit
        SucessFlag(Ifit) = ResFit(Ifit).Ngood > Args.MinNumSrc && ...
                           ResFit(Ifit).Ngood <
    
                       
         QualitySummary(Ifit).Ngood = ResFit(Ifit).Ngood;
         QualitySummary(Ifit).Nsrc       =   % number of sources
         QualitySummary(Ifit).Nref       =   % number of sources in ref
         QualitySummary(Ifit).RMS        = ResFit(Ifit).RMS;
         QualitySummary(Ifit).RMS_X      = ResFit(Ifit).RMS_X;
         QualitySummary(Ifit).RMS_Y      = ResFit(Ifit).RMS_Y;
         QualitySummary(Ifit).AssymRMS   = ResFit(Ifit).AssymRMS;
         QualitySummary(Ifit).ErrOnMean  = 
         QualitySummary(Ifit).SucessFlag = SucessFlag(Ifit);
         
         
    end
    
    
end