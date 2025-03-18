function [Result, ColNames] = reportRMS(Obj, Args)
    % Report the rms of each source in units of sigma above mean.
    %   Return [MeanMag, Std, NsigmaPred, NsigmaStd] for each source.
    %   Nsigma* is the number of sigmas the star rms is above mean rms at
    %   the same mag level.
    % Input  : - A single object MatchedSources object.
    %          * ...,key,val,... 
    %            'FieldMag' - Default is 'MAG_BEST'.
    %            'rmsMagArgs' - A cell array of additional arguments to
    %                   pass to MatchedSources/rmsMag. Default is {}.
    %            'OutType' - Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    %            'ThresholdRMSpred' - Selected only sources with RMS_NsigmaPred
    %                   above this value. Default is -Inf.
    %            'ThresholdNdet' - Select pnly sources with number of
    %                   detections equal or higher than this value.
    %                   Default is 0.
    % Output : - A matrix or table with the columns:
    %            [Ndet, MeanMag, Std, NsigmaPred, NsigmaStd]
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.reportRMS(MS);

    arguments
        Obj(1,1)
        Args.FieldMag          = 'MAG_BEST';
        Args.rmsMagArgs        = {};
        Args.OutType           = 'table';
        Args.ThresholdRMSpred  = -Inf;
        Args.ThresholdNdet     = 0;
    end
    

    ResRms = Obj.rmsMag('MagField',Args.FieldMag, Args.rmsMagArgs{:});
    
    ColNames = {'Ndet', 'MeanMag', 'Std', 'RMS_NsigmaPred', 'RMS_NsigmaStd'};
    Result = [ResRms.Ndet(:) ResRms.MeanMag(:), ResRms.StdPar(:), ResRms.NsigmaPred(:), ResRms.NsigmaStd(:)];
    
    if Args.ThresholdRMSpred>-Inf
        FlagSelected = Result(:,4)>Args.ThresholdRMSpred;
        Result       = Result(FlagSelected,:);
    end
    FlagSelected = Result(:,1)>=Args.ThresholdNdet;
    Result       = Result(FlagSelected,:);
    
    switch lower(Args.OutType)
        case 'table'
            Result = array2table(Result);
            Result.Properties.VariableNames = ColNames;
        otherwise
            % do nothing
    end
end
