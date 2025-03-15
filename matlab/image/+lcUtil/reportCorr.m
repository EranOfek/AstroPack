function [Result] = reportCorr(Obj, Args)
    % Return correlation coef. and p-val between selected fields in MatchedSources object.
    % Input  : - A single element MatchedSources object.
    %          * ...,key,val,... 
    %            'Pairs' - A two column cell array of pairs of fields
    %                   between which to calculate the corr.
    %                   Default is {'MAG_BEST','X1';
    %                                'MAG_BEST','Y1';
    %                                'MAG_BEST','RA';
    %                                'MAG_BEST','Dec'}
    %            'OutType' - Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    % Output : - A matrix or table of corr. coef. and p-val between
    %            selected fields for each source.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.reportCorr(MS)

    arguments
        Obj(1,1)
        Args.Pairs                 = {'MAG_BEST','X1';
                                      'MAG_BEST','Y1';
                                      'MAG_BEST','RA';
                                      'MAG_BEST','Dec'};
        Args.OutType               = 'table';
    end
    Ncol  = 2;
    
    Npair = size(Args.Pairs,1);
    Result = zeros(Obj.Nsrc, Npair.*Ncol);
    ColNames = cell(1, Npair.*Ncol);
    for Ipair=1:1:Npair
        ColI    = (Ipair-1).*Ncol;
        Corr = Obj.corrFields('Field1',Args.Pairs{Ipair,1}, 'Field2',Args.Pairs{Ipair,2}, 'Type','pairs_sim');
        
        ColNames{ColI+1} = sprintf('CorrC_%s_%s',Args.Pairs{Ipair,1}, Args.Pairs{Ipair,2});
        ColNames{ColI+2} = sprintf('CorrP_%s_%s',Args.Pairs{Ipair,1}, Args.Pairs{Ipair,2});
        Result(:,ColI+1) = Corr.Corr;
        Result(:,ColI+2) = Corr.PVal;        
    end

    
    switch lower(Args.OutType)
        case 'table'
            Result = array2table(Result);
            Result.Properties.VariableNames = ColNames;
        otherwise
            % do nothing
    end
end
