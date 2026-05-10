function [Streaks] = addSkyCoo(Streaks, WCS, Args)
    % Add sky coordinates to AstroStreak structure/object
    % Input  : - AstroStreak, or structure containing streaks data.
    %          - An AstroWCS object, or AstroImage containing WCS data.
    %            If AstroStreak is a structure array, then this is an array
    %            with the same number of elements.
    %          * ...,key,val,... 
    %            'OutUnits' - 'deg'|'rad'. Default is 'deg'.
    % Output : - An updated Streaks struct/object inw hich the RA/Dec are
    %            populated.
    % Author : Eran Ofek (2026 May) 
    % Example: St=imProc.streaks.addSkyCoo(St, AI);

    arguments
        Streaks
        WCS
        Args.OutUnits                 = 'deg';
    end

    if isa(WCS, 'AstroWCS')
        IsWCS = true;
    else
        IsWCS = false;
    end

    N = numel(WCS);
    for I=1:1:N
        if ~isempty(Streaks) && ~isempty(Streaks(I).X)
            if IsWCS
                [Streaks(I).RA, Streaks(I).Dec] = WCS(I).xy2sky(Streaks(I).X, Streaks(I).Y, 'OutUnits',Args.OutUnits);
            else
                [Streaks(I).RA, Streaks(I).Dec] = WCS(I).WCS.xy2sky(Streaks(I).X, Streaks(I).Y, 'OutUnits',Args.OutUnits);
            end
        end
    end

end
