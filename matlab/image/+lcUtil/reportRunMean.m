function [Result, ColNames] = reportRunMean(Obj, Args)
    % Search flares/dips using run-mean filter
    % Input  : - A single object MatchedSources object.
    %          * ...,key,val,... 
    %            'FieldMag' - Default is 'MAG_BEST'.
    %            'WinSize' - Run mean filters to test.
    %                   Default is [2 3 4 5].
    %            'PolyFit' - A vector of polynomial orders to fit and
    %                   subtract from data prior to filtering.
    %                   If empty, then skip this step.
    %                   Default is [0].
    %            'OutType' - Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    % Output : - A matrix or table with four columns per window:
    %            [MaxSN, JDmaxSN, MinSN, JDminSN]
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.reportRunMean(MS);

    arguments
        Obj(1,1)
        Args.FieldMag          = 'MAG_BEST';
        Args.WinSize           = [2 3 4 5];
        Args.PolyFit           = 0;
        %Args.MinSN             = 8;
        Args.OutType           = 'table';
    end

    Ncol = 4;
    
    Nwin = numel(Args.WinSize);
    Result = zeros(Obj.Nsrc, Nwin.*Ncol);
    ColNames = cell(1, Nwin.*Ncol);
    for Iwin=1:1:Nwin
        ColI = (Iwin-1).*Ncol;
        Res   = timeSeries.filter.runMeanFilter(Obj.Data.(Args.FieldMag), 'WinSize',Args.WinSize(Iwin), 'PolyFit',Args.PolyFit);
        
        [MaxSN, MaxInd] = max(Res.Z, [], 1);
        JDmaxSN         = Obj.JD(MaxInd);
        [MinSN, MinInd] = min(Res.Z, [], 1);
        JDminSN         = Obj.JD(MinInd);
        
        ColNames{ColI + 1} = sprintf('RM_MaxSN_Win%d',Args.WinSize(Iwin));
        ColNames{ColI + 2} = sprintf('RM_JDmaxSN_Win%d',Args.WinSize(Iwin));
        ColNames{ColI + 3} = sprintf('RM_MinSN_Win%d',Args.WinSize(Iwin));
        ColNames{ColI + 4} = sprintf('RM_JDminSN_Win%d',Args.WinSize(Iwin));
        
        Result(:,ColI+1) = MaxSN;
        Result(:,ColI+2) = JDmaxSN;
        Result(:,ColI+3) = MinSN;
        Result(:,ColI+4) = JDminSN;
        
    end
    
    switch lower(Args.OutType)
        case 'table'
            Result = array2table(Result);
            Result.Properties.VariableNames = ColNames;
        otherwise
            % do nothing
    end
end
