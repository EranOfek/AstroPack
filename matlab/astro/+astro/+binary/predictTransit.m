function [Result] = predictTransit(Epoch0, Period, Args)
    % Given binary ephemeris, predict the time of transits
    %       Nota including light time correction.
    % Input  : - Vector of epochs.
    %          - Vector of periods.
    %          * ...,key,val,... 
    %            'StartJD' - start JD or [D M Y H M S] for predictions.
    %            'EndJD' - End JD or [D M Y H M S] for predictions.
    %            'ErrorEpoch0' - Error in epoch. Default is 5e-9.
    %            'ErrorPeriod' - Error in period. Default is 5e-12.
    %            'EpochUnits' - Epoch units: 'JD'|'MJD'|'J'|'B'
    %                       Default is 'mjd'.
    %            'PeriodUnits' - Period units and epoch error units.
    %                       Default is 'day'.
    %            'Pdot' - Period derivative [s/s].
    %
    % Output : - A structure with predicted events.
    %            Containing the following fields:
    %            .PredJD - A matrix of all predicted JD withing time range.
    %                   Columns are for differenr epoch/periods, while raws
    %                   are for different events.
    %            .PredJD0 - Same as PredJD, but without taking into accounf
    %                   Pdot.
    %            .PredErr - A matrix of predicted errors.
    % Author : Eran Ofek (2023 Dec) 
    % Example: R=astro.binary.predictTransit(Epoch0, Period)
    %          Epoch0 = [51919.206615968 51919.206615991]
    %          Period=[0.837113489987 0.837113489970]
    %          R=astro.binary.predictTransit(Epoch0-0.25.*Period, Period)

    arguments
        Epoch0
        Period
        
        Args.StartJD           = celestial.time.julday;      % JD or [D M Y H M S]
        Args.EndJD             = celestial.time.julday + 1;  % JD or [D M Y H M S]
        
        Args.ErrorEpoch0       = 5e-9;  % period units
        Args.ErrorPeriod       = 5e-12  % period units
        Args.EpochUnits        = 'mjd';
        Args.PeriodUnits       = 'day';
        
        Args.Pdot              = -53e-15;  % [s/s]
        
    end

    if numel(Args.StartJD)>1
        Args.StartJD = celestial.time.julday(Args.StartJD);
    end
    if numel(Args.EndJD)>1
        Args.EndJD = celestial.time.julday(Args.EndJD);
    end
    
    % convert to JD
    Epoch0 = convert.time(Epoch0, Args.EpochUnits, 'JD');
    Epoch0 = Epoch0(:).';
    ErrorEpoch0 = convert.timeUnits(Args.PeriodUnits, 'day', Args.ErrorEpoch0);
    ErrorEpoch0 = ErrorEpoch0(:).';
    
    % convert to days
    Period = convert.timeUnits(Args.PeriodUnits, 'day', Period);
    Period = Period(:).';
    ErrorPeriod = convert.timeUnits(Args.PeriodUnits, 'day', Args.ErrorPeriod);
    ErrorPeriod = ErrorPeriod(:).';
    
    Nmin = floor((Args.StartJD - Epoch0)./Period);
    Nmax = ceil((Args.EndJD - Epoch0)./Period);
    N    = (Nmin:1:Nmax).';
    
    
    
    Result.PredJD  = Epoch0 + Period.*N + 0.5.*Args.Pdot.*Period.*86400 .*N.^2;
    Result.PredJD0 = Epoch0 + Period.*N; 
    Result.PredErr = sqrt(ErrorEpoch0.^2 + (N.*ErrorPeriod).^2);
    
    
    
end
