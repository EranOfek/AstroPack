function [SucessFlag, QualitySummary] = assessAstrometricQuality(ResFit, Args)
    % Collect information regarding quality of astrometric solution and
    % return a sucess flag.
    % Input  : - The ResFit structure returned by Tran2D/fitWCS
    %          * ...,key,val,...
    %            'MinNumSrc' - Min numbr of sources used in solution.
    %                   Default is 10.
    %            'MinUsedSrcFrac' - Min fraction of sources used for
    %                   solution. Default is 0.2.
    %            'MinRMS' - Min RMS of solution [deg].
    %                   Default is 0.3./3600.
    %            'MinAssymRMS' - Min AssymRMS of solution [deg].
    %                   Default is 0.2./3600.
    %            'MinErrOnMean' - Min error on the maem for solution [deg].
    %                   Default is 0.05./3600.
    % Output : - A vector of sucess flags (one per ResFit element).
    %          - A structure array of collected astrometric quality
    %            paramseters.
    % Author : Eran Ofek (Aug 2021)
    % Example: [SucessFlag, QualitySummary] = imProc.astrometry.assessAstrometricQuality(Result.ResFit)
    
    arguments
        ResFit struct
        Args.MinNumSrc            = 10;
        Args.MinUsedSrcFrac       = 0.2;
        
        Args.MinRMS               = 0.7./3600;    % deg
        Args.MinAssymRMS          = 0.5./3600;    % deg
        Args.MinErrOnMean         = 0.25./3600;   % deg
    end
    
    Nfit = numel(ResFit);
    for Ifit=1:1:Nfit
         QualitySummary(Ifit).Ngood        = ResFit(Ifit).Ngood;
         QualitySummary(Ifit).Nind         = ResFit(Ifit).NsrcInd;  % number of sources
         QualitySummary(Ifit).Ndep         = ResFit(Ifit).NsrcDep;  % number of sources in ref
         QualitySummary(Ifit).NsrcCat      = min(QualitySummary(Ifit).Nind, QualitySummary(Ifit).Ndep);
         QualitySummary(Ifit).NsuedSrcFrac = ResFit(Ifit).Ngood./QualitySummary(Ifit).NsrcCat;
         QualitySummary(Ifit).RMS          = ResFit(Ifit).RMS;
         QualitySummary(Ifit).RMS_X        = ResFit(Ifit).RMS_X;
         QualitySummary(Ifit).RMS_Y        = ResFit(Ifit).RMS_Y;
         QualitySummary(Ifit).AssymRMS     = ResFit(Ifit).AssymRMS;
         QualitySummary(Ifit).ErrOnMean    = [ResFit(Ifit).AssymRMS_mag]./sqrt([ResFit(Ifit).Ngood]);
         
         
         SucessFlag(Ifit) = ResFit(Ifit).Ngood > Args.MinNumSrc && ...
                            QualitySummary(Ifit).NsuedSrcFrac > Args.MinUsedSrcFrac && ...
                            ResFit(Ifit).RMS < Args.MinRMS && ...
                            ResFit(Ifit).AssymRMS < Args.MinAssymRMS && ...
                            QualitySummary(Ifit).ErrOnMean < Args.MinErrOnMean;
                        
         QualitySummary(Ifit).SucessFlag = SucessFlag(Ifit);
         
    end
    
    
end