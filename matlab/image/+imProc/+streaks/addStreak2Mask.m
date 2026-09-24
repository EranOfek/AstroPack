function [Result] = addStreak2Mask(AI, Args)
    % Set Streak bit in MaskData for every detected streak in an AstroImage.
    % Description:
    %   For each AstroImage element, builds a pixel mask for every streak
    %   and calls maskSet to set the requested bit. AddCurvature false
    %   masks the straight line between the streak ends. AddCurvature true
    %   follows the measured curve samples in St.Curve.
    %
    %   The Streaks input must contain the following fields per streak:
    %     .X      - 2×N array: [x_start; x_end] for each of N streaks.
    %     .Y      - 2×N array: [y_start; y_end] for each of N streaks.
    %     .FitPar - 3×N array: parabolic offset coefficients [a;b;c] per streak
    %               (as returned by imUtil.streaks.detectStreaksLSD /
    %                imUtil.streaks.streak_photometry). Used only when
    %               AddCurvature is true and St.Curve has no samples.
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
    %            'AddCurvature' - If true, the mask follows the measured
    %                   streak curve (St.Curve). If false, the mask is the
    %                   straight line between that curve's endpoints.
    %                   Default is false.
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
        Args.AddCurvature  = false;
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

        if isempty(St) || isempty(St.X) || isempty(St.Y)
            continue;
        end

        Nstreak = size(St.X, 2);

        for Istreak = 1:1:Nstreak

            % --- Endpoint coordinates for this streak ---
            X_seg = St.X(:, Istreak).';   % [x1, x2]  (1x2)
            Y_seg = St.Y(:, Istreak).';   % [y1, y2]  (1x2)

            % --- Edge extension flags ---
            if isempty(St.IsEdge)
                IsEdges = [];
            else
                IsEdges = St.IsEdge(:, Istreak).';  % [extStart, extEnd] (1x2 logical)
            end

            % Curve samples span the extended streak. FitPar is only the
            % parabola on the short LSD segment, so zeroing it leaves the
            % mask on that same short segment.
            [Cx, Cy] = localCurveXY(St, Istreak);

            if Args.AddCurvature && numel(Cx) >= 2
                Flag = polyline2mask(ImSz, Cx, Cy, Args.SemiWidth, IsEdges);
            elseif numel(Cx) >= 2
                Flag = polyline2mask(ImSz, [Cx(1), Cx(end)], [Cy(1), Cy(end)], ...
                    Args.SemiWidth, IsEdges);
            elseif Args.AddCurvature && ~isempty(St.FitPar)
                Flag = imUtil.streaks.parfit2mask(ImSz, St.FitPar(:, Istreak), ...
                    'X', X_seg, 'Y', Y_seg, ...
                    'SemiWidth', Args.SemiWidth, 'IsEdges', IsEdges);
            else
                Flag = polyline2mask(ImSz, X_seg, Y_seg, Args.SemiWidth, IsEdges);
            end

            % --- Set the Streak bit in MaskData ---
            Result(Iobj) = Result(Iobj).maskSet(Flag, Args.BitName);

        end % for Istreak

    end % for Iobj

end

function [Cx, Cy] = localCurveXY(St, Istreak)
    % Finite column/row samples of one streak, in along-streak order.
    Cx = [];
    Cy = [];
    if Istreak > numel(St.Curve) || isempty(St.Curve(Istreak).X) || isempty(St.Curve(Istreak).Y)
        return;
    end
    Cx = double(St.Curve(Istreak).X(:).');
    Cy = double(St.Curve(Istreak).Y(:).');
    Q = isfinite(Cx) & isfinite(Cy);
    Cx = Cx(Q);
    Cy = Cy(Q);
end

function Flag = polyline2mask(ImageSize, X, Y, SemiWidth, IsEdges)
    % Mask pixels within SemiWidth of a polyline. X is column, Y is row.
    Nrows = ImageSize(1);
    Ncols = ImageSize(2);
    X = double(X(:).');
    Y = double(Y(:).');
    Q = isfinite(X) & isfinite(Y);
    X = X(Q);
    Y = Y(Q);
    if numel(X) < 2
        Flag = false(Nrows, Ncols);
        return;
    end

    if ~isempty(IsEdges) && numel(IsEdges) == 2
        if IsEdges(1)
            [Xe, Ye] = exitPoint(X(2), Y(2), X(1), Y(1), Nrows, Ncols);
            X = [Xe, X];
            Y = [Ye, Y];
        end
        if IsEdges(2)
            [Xe, Ye] = exitPoint(X(end-1), Y(end-1), X(end), Y(end), Nrows, Ncols);
            X = [X, Xe];
            Y = [Y, Ye];
        end
    end

    CX = zeros(1, 0);
    CY = zeros(1, 0);
    for K = 1:numel(X)-1
        Dx = X(K+1) - X(K);
        Dy = Y(K+1) - Y(K);
        L = hypot(Dx, Dy);
        if L == 0
            continue;
        end
        T = linspace(0, 1, max(2, ceil(2 * L)));
        CX = [CX, X(K) + Dx .* T]; %#ok<AGROW>
        CY = [CY, Y(K) + Dy .* T];
    end

    CXr = round(CX);
    CYr = round(CY);
    Inside = CXr >= 1 & CXr <= Ncols & CYr >= 1 & CYr <= Nrows;
    Skeleton = false(Nrows, Ncols);
    if any(Inside)
        Idx = sub2ind([Nrows, Ncols], CYr(Inside), CXr(Inside));
        Skeleton(unique(Idx)) = true;
    end
    if SemiWidth <= 0
        Flag = Skeleton;
    else
        Flag = bwdist(Skeleton) <= SemiWidth;
    end
end

function [Xe, Ye] = exitPoint(Xnear, Ynear, Xfar, Yfar, Nrows, Ncols)
    % Point just outside the image, continuing from Xnear through Xfar.
    Dx = Xfar - Xnear;
    Dy = Yfar - Ynear;
    L = hypot(Dx, Dy);
    if L == 0
        Xe = Xfar;
        Ye = Yfar;
        return;
    end
    Ux = Dx / L;
    Uy = Dy / L;
    Xe = Xfar;
    Ye = Yfar;
    for S = 1:ceil(hypot(Nrows, Ncols)) + 2
        Xt = Xfar + Ux * S;
        Yt = Yfar + Uy * S;
        if Xt < 1 || Xt > Ncols || Yt < 1 || Yt > Nrows
            Xe = Xt;
            Ye = Yt;
            return;
        end
    end
end
