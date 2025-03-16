function [Result] = reportPowerSpec(Obj, Args)
    % Return max power spectra and corresponding frequency for each source in MatchedSources object.
    % Input  : - A single element MatchedSources object.
    %          * ...,key,val,... 
    %            'MagField' - Default is 'MAG_APER_3'.
    %            'MaxFreq' - Maximum frequency. Default is 86400./60.
    %            'ThresholdNp' - Return also the number of peaks in the power
    %                   spectra of each star, above this threshold.
    %                   Default is 12.
    %            'OutType' - Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    %            'Threshold' - Select sources with a peak above this value.
    %                   Default is 0 (return all sources).
    % Output : - A three column matrix or table of:
    %            [MaxPower, FrequencyOfMaxPower, NpeaksAboveThreshold]
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.reportPowerSpec(MS)

    arguments
        Obj(1,1)
        Args.FieldMag          = 'MAG_APER_3';
        Args.MaxFreq           = 86400./60;
        Args.ThresholdNp       = 12;
        Args.OutType           = 'table';
        
        Args.Threshold         = 0; % select only peaks above this
    end
    
    VecFreq = timeSeries.period.getFreq(Obj.JD, 'MaxFreq',Args.MaxFreq);
    [VecFreq, MatPS] = Obj.period(VecFreq, 'MagField',Args.FieldMag);
    
    [MaxPS, MaxI] = max(MatPS, [], 1);
    MaxFreq = VecFreq(MaxI);
    
    Nabove  = sum(MatPS>Args.ThresholdNp, 1);
    Result  = [MaxPS(:), MaxFreq(:), Nabove(:)];

    if Args.Threshold>0
        FlagSelected = MaxPS > Args.Threshold;
        Result       = Result(FlagSelected,:);
    end        
    
    switch lower(Args.OutType)
        case 'table'
            Result = array2table(Result);
            Result.Properties.VariableNames = {'MaxPower', 'FreqMaxPower', 'NpowerAboveTh'};
        otherwise
            % do nothing
    end
end
