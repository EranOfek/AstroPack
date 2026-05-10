function Flag = parfit2mask(ImageSize, Parfit, Args)
    % Logical mask of image pixels within SemiWidth of a parabolic streak curve.
    % Description:
    %   Given the parabolic transverse-offset parameters from detectStreaksLSD
    %   (or streak_photometry) and the streak base-segment endpoints, rasterise
    %   the curved streak onto an image of size ImageSize and mark every pixel
    %   whose Euclidean distance to the curve is <= SemiWidth.
    %
    %   The curve is sampled at sub-pixel spacing using parfit2curve, a sparse
    %   skeleton mask is built, then bwdist is used for an exact Euclidean
    %   distance transform so that non-integer SemiWidth is handled correctly.
    %
    %   When IsEdges is non-empty, the specified endpoint(s) are extended
    %   along the parabolic curve until they cross the image boundary, so the
    %   mask fully covers the streak across the chip.
    %
    % Input  : - ImageSize: [Nrows, Ncols] of the output mask.
    %          - Parfit: [a; b; c] (3x1) parabolic coefficients from
    %            detectStreaksLSD / streak_photometry.
    %          * ...,key,val,...
    %            'X' - [x1, x2] start/end x (column) pixel coordinates of
    %                   the base segment. Required.
    %            'Y' - [y1, y2] start/end y (row) pixel coordinates of the
    %                   base segment. Required.
    %            'SemiWidth' - Half-width of the flagged band around the curve
    %                   in pixels (Euclidean). May be non-integer.
    %                   Default is 3.
    %            'IsEdges' - 2-element logical vector [extendStart, extendEnd].
    %                   If extendStart is true, the curve is extrapolated
    %                   backwards from X(1),Y(1) until it exits the image.
    %                   If extendEnd   is true, the curve is extrapolated
    %                   forwards  from X(2),Y(2) until it exits the image.
    %                   Default is [] (no extension).
    %            'SubPixel' - Sub-pixel sampling density: number of curve
    %                   samples per pixel of segment length. Default is 2.
    % Output : - Flag: ImageSize logical array. True where a pixel centre is
    %            within SemiWidth pixels (Euclidean) of the parabolic curve.
    % Notes  : - Uses bwdist for exact Euclidean distance transform.
    %          - Extension for IsEdges is computed by searching along t until
    %            the curve exits the image box [1,Ncols] x [1,Nrows].
    %          - Requires Image Processing Toolbox (bwdist).
    % Author : Eran Ofek + Cursor (May 2026)
    % Example:
    %   % Basic mask from detectStreaksLSD output, streak i:
    %   F = imUtil.streaks.parfit2mask(size(Im), Parfit(:,i), ...
    %           'X', [segs(1,i), segs(3,i)], 'Y', [segs(2,i), segs(4,i)]);
    %
    %   % Extend both ends to image edges:
    %   F = imUtil.streaks.parfit2mask(size(Im), Parfit(:,i), ...
    %           'X', [segs(1,i), segs(3,i)], 'Y', [segs(2,i), segs(4,i)], ...
    %           'SemiWidth', 5, 'IsEdges', [true, true]);

    arguments
        ImageSize (1,2) double
        Parfit    (3,1) double
        Args.X          (1,2) double
        Args.Y          (1,2) double
        Args.SemiWidth  (1,1) double  = 3;
        Args.IsEdges                  = [];    % [] | [logStart, logEnd]
        Args.SubPixel   (1,1) double  = 2;     % samples per pixel of length
    end

    Nrows = ImageSize(1);
    Ncols = ImageSize(2);
    X     = Args.X;
    Y     = Args.Y;

    dx = X(2) - X(1);
    dy = Y(2) - Y(1);
    L  = sqrt(dx.^2 + dy.^2);

    if L == 0
        Flag = false(ImageSize);
        return;
    end

    % --- Determine t range [t_start, t_end] ---
    t_start = 0;
    t_end   = 1;

    if ~isempty(Args.IsEdges) && numel(Args.IsEdges) == 2
        % Maximum number of segment-lengths needed to cross the image
        MaxExt = (max(ImageSize) + Args.SemiWidth) / L;

        if Args.IsEdges(1)
            % Search backwards from t=0 until curve exits the image
            t_start = -findExitT(Parfit, X, Y, L, 0, -1, MaxExt, Nrows, Ncols);
        end
        if Args.IsEdges(2)
            % Search forwards from t=1 until curve exits the image
            t_end = 1 + findExitT(Parfit, X, Y, L, 1, +1, MaxExt, Nrows, Ncols);
        end
    end

    % --- Sample curve at sub-pixel density ---
    Nsamples = max(10, ceil(Args.SubPixel .* L .* (t_end - t_start)));
    t        = linspace(t_start, t_end, Nsamples);

    [CX, CY] = imUtil.streaks.parfit2curve(Parfit, X, Y, 'T', t);

    % --- Build sparse skeleton mask (rasterise) ---
    % Round to nearest pixel and keep only those inside the image.
    CXr = round(CX);
    CYr = round(CY);
    inside = CXr >= 1 & CXr <= Ncols & CYr >= 1 & CYr <= Nrows;
    CXr = CXr(inside);
    CYr = CYr(inside);

    Skeleton = false(ImageSize);
    if ~isempty(CXr)
        % sub2ind: (row, col) = (CYr, CXr)
        idx = sub2ind(ImageSize, CYr, CXr);
        Skeleton(unique(idx)) = true;
    end

    % --- Euclidean distance transform -> flag within SemiWidth ---
    if Args.SemiWidth <= 0
        Flag = Skeleton;
    else
        D    = bwdist(Skeleton);   % distance from each pixel to nearest skeleton px
        Flag = D <= Args.SemiWidth;
    end

end


%% -----------------------------------------------------------------------
function dt = findExitT(Parfit, X, Y, L, t0, direction, maxDT, Nrows, Ncols)
    % Binary search for the first dt > 0 such that the curve at (t0 +
    % direction*dt) lies outside [1,Ncols] x [1,Nrows].
    % Returns dt (positive scalar).

    % First, find an upper bound by doubling the step.
    dt_lo = 0;
    dt_hi = 1 / L;  % start with a small step
    for k = 1:30
        t_test = t0 + direction * dt_hi;
        [cx, cy] = evalCurve(Parfit, X, Y, L, t_test);
        if cx < 1 || cx > Ncols || cy < 1 || cy > Nrows
            break;
        end
        dt_hi = dt_hi * 2;
        if dt_hi > maxDT
            dt = maxDT;
            return;
        end
    end

    % Binary search between dt_lo (inside) and dt_hi (outside).
    for k = 1:25
        dt_mid = 0.5 * (dt_lo + dt_hi);
        t_test = t0 + direction * dt_mid;
        [cx, cy] = evalCurve(Parfit, X, Y, L, t_test);
        if cx < 1 || cx > Ncols || cy < 1 || cy > Nrows
            dt_hi = dt_mid;
        else
            dt_lo = dt_mid;
        end
        if dt_hi - dt_lo < 0.5 / L
            break;   % converged to ~0.5 pixel accuracy
        end
    end
    dt = dt_hi;
end


function [cx, cy] = evalCurve(Parfit, X, Y, L, t)
    % Evaluate a single curve point (scalar t).
    dx = X(2) - X(1);
    dy = Y(2) - Y(1);
    h  = Parfit(1).*t.^2 + Parfit(2).*t + Parfit(3);
    cx = X(1) + dx.*t - dy.*h./L;
    cy = Y(1) + dy.*t + dx.*h./L;
end
