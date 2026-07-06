function [AI] = addSkyCoo(AI, Args)
    % Add sky coordinates to AstroStreak structure/object
    % Input  : - An AstroImage object containing Streaks and WCS.
    %          * ...,key,val,... 
    %            'OutUnits' - 'deg'|'rad'. Default is 'deg'.
    %            'PopJD' - Populated JD. Default is false.
    %            'JD' - Optional matrix of MidJD to use.
    %            'ExpTime' - Optional ExpTime.
    % Output : - An AstroImage object in which the Streaks struct/object
    %            is updated with RA, Dec.
    % Author : Eran Ofek (2026 May) 
    % Example: AI=imProc.streaks.addSkyCoo(AI);

    arguments
        AI
        Args.OutUnits                 = 'deg';
        Args.PopJD                    = false;
        Args.JD                       = [];
        Args.ExpTime                  = [];
        Args.PopIsEdge                = true;
        Args.EdgeDist                 = 10;
    end
    SEC_DAY = 86400;

    if Args.PopJD
        if isempty(Args.JD)
            [Args.JD, Args.ExpTime] = AI.julday;
        end

        ExpTime = Args.ExpTime./SEC_DAY; % [day]
    end
    N = numel(AI);
    for I=1:1:N
        if ~isempty(AI(I).Streaks) && ~isempty(AI(I).Streaks.X)
            if ~isempty(AI(I).WCS) && AI(I).WCS.Success
                [AI(I).Streaks.RA, AI(I).Streaks.Dec] = AI(I).WCS.xy2sky(AI(I).Streaks.X, AI(I).Streaks.Y, 'OutUnits',Args.OutUnits);
                Nst = numel(AI(I).Streaks.Curve);
                for Ist=1:1:Nst
                    [AI(I).Streaks.Curve(Ist).RA, AI(I).Streaks.Curve(Ist).Dec] = AI(I).WCS.xy2sky(AI(I).Streaks.Curve(Ist).X, AI(I).Streaks.Curve(Ist).Y, 'OutUnits',Args.OutUnits);
                end
            end
        
            if Args.PopJD
                AI(I).Streaks.JD = [Args.JD(I)-0.5.*ExpTime(I); Args.JD(I)+0.5.*ExpTime(I)];
            end
    
            if Args.PopIsEdge
                SizeImageIJ = size(AI(I).ImageData.Data);
                IEX = AI(I).Streaks.X<Args.EdgeDist | AI(I).Streaks.X>(SizeImageIJ(2)-Args.EdgeDist);
                IEY = AI(I).Streaks.Y<Args.EdgeDist | AI(I).Streaks.Y>(SizeImageIJ(2)-Args.EdgeDist);
                AI(I).Streaks.IsEdge = [IEX(1,:) | IEY(1,:); IEX(2,:) | IEY(2,:)];
            end
        end
    end

end
