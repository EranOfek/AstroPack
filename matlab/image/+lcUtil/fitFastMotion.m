function [Result] = fitFastMotion(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Mar) 
    % Example: 

    arguments
        Obj
        Args.FieldMag                 = 'MAG_PSF';
        Args.BitDict                  = BitDictionary;
        Args.PropFlags    = 'FLAGS';
        Args.FlagsList    = {'NearEdge','Saturated','NaN','Negative'};
    end

    % remove bad flags
    FlagId = searchFlags(Obj, 'BitDic',Args.BitDict, 'PropFlags',Args.PropFlags, 'FlagsList',Args.FlagsList, 'UseSrcData',true);
    
    Nnotnan = sum(~isnan(Obj.Data.(Args.FieldMag))) & ~FlagId;
    


end
