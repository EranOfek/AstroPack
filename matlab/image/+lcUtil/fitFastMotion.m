function [Result] = fitFastMotion(Obj, Args)
    % Search fast moving asteroids in MatchedSources object.
    %     See details / using: imUtil.asteroids.fitFastMotion
    % Input  : - A MatchedSources object.
    %          * ...,key,val,... 
    %            'FieldRA'  - Field containing RA. Default is 'RA'.
    %            'FieldDec' - Default is 'Dec'.
    %            'FieldMag' - Default is 'MAG_PSF'.
    %            'FieldFlag' - Default is 'FLAGS'.
    %            'BitDict' - BitDictionary object. Default is BitDictionary
    %            'FlagsList' - Bad flags to remove.
    %                   Default is {'NearEdge','Saturated','NaN','Negative'};
    %
    %            'MaxNdet'- Max. number of detections of source "in the smae position"
    %                   over all epochs. Default is 7.
    %            'MaxTimeDiff' - Max. time range for MaxNdet (days).
    %                   Default is 7.*20./86400.
    %            'MaxDist' - Maximum distance between points to fit linear
    %                   motion (units are in CooUnits').
    %                   Default is 0.03.
    %            'CooUnits' - RA, Dec, MaxDist units. Default is 'deg'.
    %            'NptFit' - Number of points to fit in each RANSAC
    %                   simulation. Default is 3.
    %            'MinNpt' - Min. Number of points in the best fit solution
    %                   of a moving source.
    %                   Default is 8.
    %            'ThresholdDist' - Threshold distance of points in
    %                   best fit solution from the linear motion.
    %                   Default is 3 arcsec.
    %
    % Output : - A structure array with element per fast moving source
    %            found. See details in imUtil.asteroids.fitFastMotion
    %            The .Tag field specify the index of the MatchedSources
    %            object element.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.fitFastMotion(MS);

    arguments
        Obj
        Args.FieldMag                 = 'MAG_PSF';
        Args.BitDict                  = BitDictionary;
        Args.FieldFlag                = 'FLAGS';
        Args.FlagsList                = {'NearEdge','Saturated','NaN','Negative'};
        
        Args.MaxNdet           = 7;
        Args.MaxTimeDiff       = 7.*20./86400;  % days
        Args.MaxDist           = 0.03;  % deg
        Args.CooUnits          = 'deg';
        Args.NptFit            = 3;
        Args.MinNpt            = 8;
        Args.ThresholdDist     = 3;  % arcsec
    end

    % remove bad flags
    FlagBad = searchFlags(Obj, 'BitDic',Args.BitDict, 'PropFlags',Args.FieldFlag, 'FlagsList',Args.FlagsList, 'UseSrcData',true);

    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        [Tmp] = imUtil.asteroids.fitFastMotion(Obj(Iobj).JD, Obj(Iobj).Data.RA, Obj(Iobj).Data.Dec,...
                                                    'FlagGood',~FlagBad, 'Tag',Iobj,...
                                                    'DimEpoch',Obj.DimEpoch,...
                                                    'MaxNdet',Args.MaxNdet,...
                                                    'MaxTimeDiff',Args.MaxTimeDiff,...
                                                    'MaxDist',Args.MaxDist,...
                                                    'CooUnits',Args.CooUnits,...
                                                    'NptFit',Args.NptFit,...
                                                    'MinNpt',Args.MinNpt,...
                                                    'ThresholdDist',Args.ThresholdDist);
        if Iobj==1
            Result = Tmp(:);
        else
            Result = [Result; Tmp(:)];
        end
    end

end
