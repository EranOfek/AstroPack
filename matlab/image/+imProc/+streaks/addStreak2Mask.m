function [Result] = addStreak2Mask(AI, Args)
    % Set Streak bit in MaskData for every detected streak in an AstroImage.
    % Description:
    %   For each AstroImage element, iterates over the streaks supplied in
    %   the matching Streaks element and uses imUtil.streaks.parfit2mask to
    %   compute the per-streak pixel mask, then calls maskSet on the
    %   AstroImage MaskData to set the requested bit for all flagged pixels.
    %
    %   The Streaks input must contain the following fields per streak:
    %     .X      - 2×N array: [x_start; x_end] for each of N streaks.
    %     .Y      - 2×N array: [y_start; y_end] for each of N streaks.
    %     .FitPar - 3×N array: parabolic offset coefficients [a;b;c] per streak
    %               (as returned by imUtil.streaks.detectStreaksLSD /
    %                imUtil.streaks.streak_photometry).
    %     .IsEdge - 2×N logical: [extendStart; extendEnd] per streak.
    %               Passed to imUtil.streaks.parfit2mask as IsEdges so that
    %               the mask is extended to the image boundary when true.
    %               May be [] to disable edge extension for all streaks.
    %
    % Input  : - AI: AstroImage object (scalar or array).
    %            containing a Streaks property.
    %          * ...,key,val,...
    %            'BitName' - Bit name (or bit index) to set in MaskData.
    %                   Default is 'Streak'.
    %            'SemiWidth' - Half-width in pixels around the streak curve
    %                   passed to parfit2mask. Default is 3.
    %            'CreateNewObj' - If true return a copy; if false update AI
    %                   in place. Default is false.
    % Output : - AstroImage with MaskData updated; Streak bit set for all
    %            pixels within SemiWidth of any detected streak.
    % Author : Eran Ofek + Cursor s.addStreak2Mask(AI, AI.Streaks);
    %   Result = imProc.streaks.addStreak2Mask(AI, St, ...
    %               'BitName','Streak','SemiWidth',5);

    arguments
        AI
        
        Args.BitName       = 'Streak';
        Args.SemiWidth     = 3;
        Args.CreateNewObj  = false;
    end

    if Args.CreateNewObj
        Result = AI.copy;
    else
        Result = AI;
    end

    Nobj = numel(AI);
    for Iobj = 1:1:Nobj

        % --- Get image size for this element ---
        ImSz = size(Result(Iobj).ImageData.Data);
        if numel(ImSz) < 2 || any(ImSz == 0)
            % no image data; skip
            continue;
        end

        % --- Get matching streaks element ---
        %Istreak_obj = min(Iobj, numel(Streaks));
        St = AI(Iobj).Streaks;

        if isempty(St) || isempty(St.X) || isempty(St.FitPar)
            continue;
        end

        Nstreak = size(St.X, 2);

        for Istreak = 1:1:Nstreak

            % --- Endpoint coordinates for this streak ---
            X_seg = St.X(:, Istreak).';   % [x1, x2]  (1x2)
            Y_seg = St.Y(:, Istreak).';   % [y1, y2]  (1x2)

            % --- Parabolic coefficients ---
            Parfit = St.FitPar(:, Istreak);  % 3x1

            % --- Edge extension flags ---
            if isempty(St.IsEdge)
                IsEdges = [];
            else
                IsEdges = St.IsEdge(:, Istreak).';  % [extStart, extEnd] (1x2 logical)
            end

            % --- Build pixel mask via parfit2mask ---
            Flag = imUtil.streaks.parfit2mask(ImSz, Parfit, ...
                'X',         X_seg,   ...
                'Y',         Y_seg,   ...
                'SemiWidth', Args.SemiWidth, ...
                'IsEdges',   IsEdges);

            % --- Set the Streak bit in MaskData ---
            Result(Iobj) = Result(Iobj).maskSet(Flag, Args.BitName);

        end % for Istreak

    end % for Iobj

end
