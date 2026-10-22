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
        AI AstroImage
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
                Nst = numel(AI(I).Streaks.Curve);
                for Ist=1:1:Nst
                    % Streaks.Curve().RA,Dec - set according to all X,Y,
                    %  acceptable or not
                    [AI(I).Streaks.Curve(Ist).RA, AI(I).Streaks.Curve(Ist).Dec] = ...
                        AI(I).WCS.xy2sky(AI(I).Streaks.Curve(Ist).X, ...
                                         AI(I).Streaks.Curve(Ist).Y, ...
                                         'OutUnits',Args.OutUnits);
                     % set Streaks.RA,Dec: if no extent set as X,Y (detected),
                     %   if there is an extent set it as such
                     EI=AI(I).Streaks.Curve(Ist).ExtentIndices;
                     if isempty(EI)
                         [AI(I).Streaks.RA(:,Ist), AI(I).Streaks.Dec(:,Ist)] = ...
                             AI(I).WCS.xy2sky(AI(I).Streaks.X(:,Ist), ...
                                              AI(I).Streaks.Y(:,Ist), ...
                                              'OutUnits',Args.OutUnits);
                     else
                         [AI(I).Streaks.RA(:,Ist), AI(I).Streaks.Dec(:,Ist)] = ...
                             AI(I).WCS.xy2sky(AI(I).Streaks.Curve(Ist).X(EI)', ...
                                              AI(I).Streaks.Curve(Ist).Y(EI)', ...
                                              'OutUnits',Args.OutUnits);
                     end
                end
            end
        
            if Args.PopJD
                AI(I).Streaks.JD = [Args.JD(I)-0.5.*ExpTime(I); Args.JD(I)+0.5.*ExpTime(I)];
            end
    
            if Args.PopIsEdge
                AI(I).Streaks.setIsEdge(size(AI(I).ImageData.Data),Args.EdgeDist)
            end
        end
    end

end
