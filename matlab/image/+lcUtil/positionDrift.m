function [GlobalMotion, Result] = positionDrift(MS, Args)
    % Measure the mean X,Y position drift as a function of time in MatchedSources object.
    % Input  : - A MatchedSources object
    %          * ...,key,val,... 
    %            'Method' - one of the following methods:
    %                   'diff' - using median of sucessive differences.
    %                   Default is 'diff'.
    %            'MinSN' - Min S/N of stars to use. I fempty, use all
    %                   stars. Default is 20.
    %            'ColX' - X coordinate field name in MatchedSosurces.
    %                   Default is 'X'.
    %            'ColY' - Y coordinate field name in MatchedSosurces.
    %                   Default is 'Y'.
    %            'ColSN' - S/N field name in MatchedSosurces.
    %                   Default is 'SN'.
    % Output : - A structure array (element per MatchedSources element)
    %            with the field global motion as a function of time.
    %            Fields include:
    %            .ResidX
    %            .StdX
    %            .RateX - [pix/s]
    %            .ResidY
    %            .StdY
    %            .RateY - [pix/s]
    %          - Structure array with additional information:
    %            .DShiftX - median diff between X positions of all strars
    %                   in sucessive epochs.
    %            .DShiftY - median diff between Y positions of all strars
    %                   in sucessive epochs.
    %            .ShiftXY - A two column matrix of cumulative shifts [X,Y].
    % Author : Eran Ofek (2025 Dec) 
    % Example: [GM,Info]=lcUtil.positionDrift(MS);

    arguments
        MS
        
        Args.Method      = 'diff';
        Args.MinSN       = 20;
        Args.ColX        = 'X';
        Args.ColY        = 'Y';
        Args.ColSN       = 'SN';
    end
    SEC_DAY = 86400;

    Nms = numel(MS);
    Result = struct('DShiftX',cell(Nms,1), 'DShiftY',cell(Nms,1), 'ShiftXY',cell(Nms,1));
    GlobalMotion = struct('ResidX',cell(Nms,1), 'StdX',cell(Nms,1), 'RateX',cell(Nms,1), 'ResidY',cell(Nms,1), 'StdY',cell(Nms,1), 'RateY',cell(Nms,1));
    for Ims=1:1:Nms
        switch lower(Args.Method)
            case 'diff'
                if isempty(Args.MinSN)
                    Result(Ims).DShiftX    = median(diff(MS(Ims).Data.(Args.ColX),1,1), 2, 'omitnan');
                    Result(Ims).DShiftY    = median(diff(MS(Ims).Data.(Args.ColY),1,1), 2, 'omitnan');
                   
                else
                    IndSN = find(mean(MS(Ims).Data.(Args.ColSN), 1, 'omitnan')>Args.MinSN);
                    Result(Ims).DShiftX    = median(diff(MS(Ims).Data.(Args.ColX)(:,IndSN),1,1), 2, 'omitnan');
                    Result(Ims).DShiftY    = median(diff(MS(Ims).Data.(Args.ColY)(:,IndSN),1,1), 2, 'omitnan');
                    
                end
                JD = MS(Ims).JD;
                Result(Ims).DeltaTime = median(diff(JD));
                Result(Ims).ShiftXY = cumsum([0 0; -[Result(Ifields).DShiftX, Result(Ifields).DShiftY]]);


                RelTimeDay                 = JD-mean(JD);
                Par                        = polyfit(RelTimeDay, Result(Ims).ShiftXY(:,1),1);
                GlobalMotion(Ims).ResidX   = Result(Ims).ShiftXY(:,1) - polyval(Par, RelTimeDay);
                GlobalMotion(Ims).StdX     = std(GlobalMotion(Ims).ResidX);
                GlobalMotion(Ims).RateX    = Par(1)./SEC_DAY;       % pix/sec
                Par                        = polyfit(RelTimeDay, Result(Ims).ShiftXY(:,2),1);
                GlobalMotion(Ims).ResidY   = Result(Ims).ShiftXY(:,2) - polyval(Par, RelTimeDay);
                GlobalMotion(Ims).StdY     = std(GlobalMotion(Ims).ResidY);
                GlobalMotion(Ims).RateY    = Par(1)./SEC_DAY;  % pix/sec
            otherwise
                error('Unknown Method option');
        end

    end

end
