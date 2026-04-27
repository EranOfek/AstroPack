function [Result] = saveProductStreak(AI, FN, Args)
    % Save streaks data stored in AstroImage object
    % Input  : - An AstroImage object.
    %          - An AstroFileName object with file name to save.
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Apr) 
    % Example: 

    arguments
        AI
        FN
        Args.JD     = [];
        Args.Save   = true;
    end

    [Nep, Nsub] = size(AI);
    K = 0;
    for Isub=1:1:Nsub
        for Iep=1:1:Nep
            if ~isempty(AI(Iep, Isub).Streaks)
                K = K + 1;
                AllStreaks(K).Streak = AI(Iep, Isub).Streak;
                AllStreaks(K).Epoch  = Iep;
                AllStreaks(K).Crop   = Isub;
                if isempty(Args.JD)
                    AllStreaks(K).JD = AI(Iep, Isub).julday;
                else
                    AllStreaks(K).JD     = Args.JD(Iep, Isub);
                end
            end
        end
    end

    if K>0 && Args.Save
        % save
        save('-v7.3', FN.genFull, "AllStreaks");
    end



end
