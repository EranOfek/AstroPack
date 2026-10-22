function [AI] = addSkyCoo(AI, Args)
    % Add sky coordinates to AstroStreak structure/object
    % Input  : - An AstroImage object containing Streaks and WCS.
    %          * ...,key,val,... 
    %            'OutUnits' - 'deg'|'rad'. Default is 'deg'.
    %            'PopJD' - Populated JD. Default is false.
    %            'JD' - Optional matrix of MidJD to use.
    %            'ExpTime' - Optional ExpTime.
    %            'PopIsEdge' - Populate Streaks.IsEdge. Default is false.
    %                          (should already have been done by the detector)
    %            'EdgeDist' - Distance [pix] from the image edge within which
    %                   a streak end is flagged as being at the edge.
    %                   Default is 20 (two slice widths).
    % Output : - An AstroImage object in which the Streaks struct/object
    %            is updated with RA, Dec.
    %            IsEdge is tested on the first and last photometrically
    %            accepted slices of Streaks.Curve (issue #1416); false if no
    %            slice was accepted, and Streaks.X/Y if there is no Curve.
    % Author : Eran Ofek (2026 May) 
    % Example: AI=imProc.streaks.addSkyCoo(AI);

    arguments
        AI
        Args.OutUnits                 = 'deg';
        Args.PopJD                    = false;
        Args.JD                       = [];
        Args.ExpTime                  = [];
        Args.PopIsEdge                = false;
        Args.EdgeDist                 = 20;
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
                AI(I).Streaks.setIsEdge(size(AI(I).ImageData.Data),Args.EdgeDist)
%                 SizeImageIJ = size(AI(I).ImageData.Data);
%                 % ends of the accepted part of each streak
%                 EndX   = AI(I).Streaks.X;
%                 EndY   = AI(I).Streaks.Y;
%                 Nseg   = size(EndX, 2);
%                 HasRun = true(1, Nseg);
%                 for Ist=1:1:min(Nseg, numel(AI(I).Streaks.Curve))
%                     [EndX(:,Ist), EndY(:,Ist), HasRun(Ist)] = acceptedEnds(AI(I).Streaks.Curve(Ist), EndX(:,Ist), EndY(:,Ist));
%                 end
%                 IEX = EndX<Args.EdgeDist | EndX>(SizeImageIJ(2)-Args.EdgeDist);
%                 IEY = EndY<Args.EdgeDist | EndY>(SizeImageIJ(1)-Args.EdgeDist);
%                 AI(I).Streaks.IsEdge = (IEX | IEY) & HasRun;
            end
        end
    end

end

% function [EndX, EndY, HasRun] = acceptedEnds(Curve, EndX, EndY)
%     % First and last accepted slice of one streak curve, in curve order.
%     % Without curve samples the given ends are kept; HasRun is false if
%     % no slice was accepted. Empty Acceptable means all slices count.
%     HasRun = true;
%     Cx = Curve.X(:);
%     Cy = Curve.Y(:);
%     if isempty(Cx) || isempty(Cy)
%         return;
%     end
%     Q = isfinite(Cx) & isfinite(Cy);
%     if ~isempty(Curve.Acceptable)
%         Q = Q & Curve.Acceptable(:);
%     end
%     Ind = find(Q);
%     if isempty(Ind)
%         HasRun = false;
%     else
%         EndX = Cx(Ind([1 end]));
%         EndY = Cy(Ind([1 end]));
%     end
% end
