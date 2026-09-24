function [AI, JD, ExpTime,IsFixed] = fixJD(AI, Args)
    % Update JD to contain mid-exposure and fix JD (i.e., if JD is round, then read from DATE-OBS).
    % Input  : - AstroImage object.
    %          * ...,key,val,... 
    %            'CheckJD' - A logical indicating if to check the JD.
    %                   if JD is round then recalculate it from DATE-OBS
    %                   Default is true.
    %            'ExpTimeKey' - Haeder keyword for Exposure Time.
    %                   Default is 'EXPTIME'.
    %            'DateObsKey' - Header keyword for date.
    %                   Default is 'DATE-OBS'.
    % Output : - An AstroImage with updated header.
    %            JD keyword should contains the JD of middle of exposure.
    %          - JD of middle of exposure.
    %          - Exposure time [s].
    %          - A logical indicating that a problemwas found and fixed.
    % Author : Eran Ofek (2025 Sep) 
    % Example: [AI,JD,ExpTime]=imProc.header.fixJD(AI);

    arguments
        AI
        Args.CheckJD           = true;
        Args.ExpTimeKey        = 'EXPTIME';
        Args.DateObsKey        = 'DATE-OBS';
        
    end

    [JD, ExpTime] = AI.julday('ExpTimeKey',Args.ExpTimeKey);
    IsFixed = false;
    if Args.CheckJD
        FlagBadJD = floor(JD)==JD;
        if any(FlagBadJD)
            % JD is rounded - maybe a problem
            % Check consisteny of JD and DATE-OBS
            Nai = numel(AI);
            for Iai=1:1:Nai
                Date  = AI(Iai).HeaderData.getVal(Args.DateObsKey);
                Date  = sprintf('%s:%s:%s', Date(1:13), Date(14:15), Date(16:end));
                JD_DateObs = celestial.time.julday(Date);
                if abs(JD(Iai)-JD_DateObs)>1e-5
                    % Problem found with JD
                    % fix JD
                    JD(Iai) = JD_DateObs;
                    StrJD = sprintf('%16.8f',JD_DateObs);
                    AI(Iai).setKeyVal('JD',StrJD);
                    IsFixed = true;
                end
            end
        end
    end

end
