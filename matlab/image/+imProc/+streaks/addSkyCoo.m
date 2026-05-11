function [AI] = addSkyCoo(AI, Args)
    % Add sky coordinates to AstroStreak structure/object
    % Input  : - An AstroImage object containing Streaks and WCS.
    %          * ...,key,val,... 
    %            'OutUnits' - 'deg'|'rad'. Default is 'deg'.
    % Output : - An AstroImage object in which the Streaks struct/object
    %            is updated with RA, Dec.
    % Author : Eran Ofek (2026 May) 
    % Example: AI=imProc.streaks.addSkyCoo(AI);

    arguments
        AI
        Args.OutUnits                 = 'deg';
    end

    N = numel(AI);
    for I=1:1:N
        if ~isempty(AI(I).Streaks) && ~isempty(AI(I).Streaks.X)
            if ~isempty(AI(I).WCS) && AI(I).WCS.Success
                [AI(I).Streaks.RA, AI(I).Streaks.Dec] = AI(I).WCS.xy2sky(AI(I).Streaks.X, AI(I).Streaks.Y, 'OutUnits',Args.OutUnits);
            end
        end
    end

end
