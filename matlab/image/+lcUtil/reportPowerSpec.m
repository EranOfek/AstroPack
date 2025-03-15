function [Result] = reportPowerSpec(Obj, Args)
    % Return max power spectra and corresponding frequency for each source in MatchedSources object.
    % Input  : - A single element MatchedSources object.
    %          * ...,key,val,... 
    %            'MagField' - Default is 'MAG_APER_3'.
    %            'MaxFreq' - Maximum frequency. Default is 86400./60.
    %            'Threshold' - Return also the number of peaks in the power
    %                   spectra of each star, above this threshold.
    %                   Default is 12.
    %            'OutType' - Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    % Output : - A three column matrix or table of:
    %            [MaxPower, FrequencyOfMaxPower, NpeaksAboveThreshold]
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.reportPowerSpec(MS)

    arguments
        Obj(1,1)
        Args.FieldMag          = 'MAG_APER_3';
        Args.MaxFreq           = 86400./60;
        Args.Threshold         = 12;
        Args.OutType           = 'table';
    end
    
    VecFreq = timeSeries.period.getFreq(Obj.JD, 'MaxFreq',Args.MaxFreq);
    [VecFreq, MatPS] = Obj.period(VecFreq, 'MagField',Args.FieldMag);
    
    [MaxPS, MaxI] = max(MatPS, [], 1);
    MaxFreq = VecFreq(MaxI);
    
    Nabove  = sum(MatPS>Args.Threshold, 1);
    Result  = [MaxPS(:), MaxFreq, Nabove(:)];

    switch lower(Args.OutType)
        case 'table'
            Result = array2table(Result);
            Result.Properties.VariableNames = {'MaxPower', 'FreqMaxPower', 'NpowerAboveTh'};
        otherwise
            % do nothing
    end
end
