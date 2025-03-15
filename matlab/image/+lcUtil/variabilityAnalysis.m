function [Result] = variabilityAnalysis(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Mar) 
    % Example: 

    arguments
        Obj MatchedSources
        Args.RemoveFlags              = {'Saturated', 'NearEdge', 'Overlap', 'NaN', 'Negative'};
        Args.BitDict                  = BitDictionary;
        Args.FieldFlags               = 'FLAGS';
        Args.FieldMag                 = 'MAG_BEST';
        Args.FieldMagErr              = 'MAGERR_PSF';
        Args.Detrend2D logical        = true;
        Args.zp_fit2DArgs             = {};
        Args.DetrendZP logical        = true;
        Args.zp_meddiffArgs           = {};
        
        Args.PeriodMinPower           = 12;   % Inf will not run
        Args.PeriodMinN               = 16;
        
        Args.RMSMinSigma              = 10;   % Inf will not run
        Args.RMSMinN                  = 16;
        
        Args.PolyMinChi2              = 20;   % Inf will not run
        Args.PolyOrders               = (0:1:5);
        Args.PolyMinN                 = 16;
        
        Args.RunMeanMinSN             = 8;    % Inf will not run
        Args.RunMeanWin               = [2 3 4 5 6];
        Args.RunMeanMinN              = 16;
        
        Args.FlareNanMinSN            = 8;
        Args.FlareNanMinN             = 2;
        
        Args.CreateNewObj logical     = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    % populate:
    Result.bestMag;
    Result.addSrcData;
    
    % clean data
    F = Result.searchFlags('UseSrcData',true, 'BitDict',Args.BitDict, 'FieldFlags',Args.FieldFlags, 'FlagsList',Args.RemoveFlags);
    Result.selectBySrcIndex(~F, 'CreateNewObj',false);
    
    % detrend data
    if Args.Detrend2D
        Result = lcUtil.zp_fit2D(Result, 'FieldMag',Args.FieldMag, 'FieldMagErr',Args.FieldMagErr, 'CreateNewObj',false, 'BitDict',Args.BitDict, Args.zp_fit2DArgs{:});
    end
    if Args.DetrendZP
        Rzp = lcUtil.zp_meddiff(Result, 'MagField',Args.FieldMag, 'MagErrField',Args.FieldMagErr, 'BitDict',Args.BitDict, Args.zp_meddiffArgs{:});
        Result.applyZP(Rzp);
    end
    
    % MatchedSources stat:
    % Nsrc, NsrcAll, Nep, MinJD, MaxJD, Duration, MidJD, Node, Mount,
    % Camera, CropID, Visit, VisitDate, FullFileNames
    
    
    
    % power spectrum
    [~,TablePS] = lcUtil.variabilityPS(Result);
    
    
    lcUtil.variabilityRMS
    
    lcUtil.variabilityPoly
    
    lcUtil.variabilityRunMean
    
    lcUtil.variabilityFlareNan
    
    lcUtil.variabilityCorr
    
    
    
    
end
