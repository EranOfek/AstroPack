function [CurveX, CurveY] = parfit2curve(Parfit, X, Y, Args)
    % Compute pixel-coordinate curve from parabolic offset parameters.
    % Description:
    %   Given the parabolic transverse-offset model h(t) = a*t^2 + b*t + c
    %   (as returned in the Parfit output of detectStreaksLSD / streak_photometry)
    %   and the start/end pixel coordinates of the base segment, return a set
    %   of (X,Y) pixel coordinates that trace the curved streak on the image.
    %
    %   The model uses an intrinsic coordinate t in [0,1]:
    %     t=0 corresponds to the start point [X(1), Y(1)]
    %     t=1 corresponds to the end   point [X(2), Y(2)]
    %   The transverse (perpendicular) offset from the base segment at t is:
    %     h(t) = Parfit(1)*t^2 + Parfit(2)*t + Parfit(3)
    %   The orthogonal unit vector is (-dy, dx)/L, so:
    %     CurveX(t) = X(1) + (X(2)-X(1))*t - (Y(2)-Y(1))*h(t)/L
    %     CurveY(t) = Y(1) + (Y(2)-Y(1))*t + (X(2)-X(1))*h(t)/L
    %
    % Input  : - Parfit: [a; b; c] (3x1 column vector) or [a, b, c] (1x3 row).
    %          - X: [x1, x2] start/end x-coordinates (pixel column indices).
    %          - Y: [y1, y2] start/end y-coordinates (pixel row indices).
    %          * ...,key,val,...
    %            'Npoints' - Number of equally spaced t samples along the
    %                   streak (inclusive of endpoints).
    %                   Default is 200.
    %            'T' - Explicit vector of t values in [0,1]. When provided,
    %                   overrides Npoints. Default is [].
    %            'ExtendFrac' - Fractional extension beyond [0,1] at both ends.
    %                   E.g. 0.05 extends the curve by 5% of the segment length
    %                   at each side. Default is 0.
    % Output : - CurveX: 1 x Npoints vector of x pixel coordinates.
    %          - CurveY: 1 x Npoints vector of y pixel coordinates.
    % Notes  : - Parfit coefficients are in the intrinsic coordinate system of
    %            the segment; they are NOT in pixels directly.
    %          - A straight streak has Parfit ≈ [0; 0; 0], so CurveX/CurveY
    %            reduce to the straight line between the two endpoints.
    %          - The function is vectorised: t is a row vector and all
    %            arithmetic is element-wise.
    % Author : Eran Ofek + Cursor (May 2026)
    % Example:
    %   % Single streak from detectStreaksLSD output:
    %   i = 1;
    %   [CX, CY] = imUtil.streaks.parfit2curve(Parfit(:,i), ...
    %                   [segs(1,i), segs(3,i)], [segs(2,i), segs(4,i)]);
    %   plot(CX, CY, 'r-');
    %
    %   % With extension and explicit step size:
    %   [CX, CY] = imUtil.streaks.parfit2curve(Parfit(:,i), ...
    %                   [segs(1,i), segs(3,i)], [segs(2,i), segs(4,i)], ...
    %                   'Npoints', 500, 'ExtendFrac', 0.05);

    arguments
        Parfit  (3,1)           % [a; b; c] parabolic coefficients
        X       (1,2) double    % [x1, x2] start/end x coordinates
        Y       (1,2) double    % [y1, y2] start/end y coordinates
        Args.Npoints   (1,1) double  = 200;
        Args.T         (1,:) double  = [];
        Args.ExtendFrac (1,1) double = 0;
    end

    % --- Build the intrinsic t grid ---
    if ~isempty(Args.T)
        t = Args.T(:).';                        % force row
    else
        t0 = -Args.ExtendFrac;
        t1 =  1 + Args.ExtendFrac;
        t  = linspace(t0, t1, Args.Npoints);
    end

    % --- Base segment geometry ---
    dx = X(2) - X(1);
    dy = Y(2) - Y(1);
    L  = sqrt(dx.^2 + dy.^2);

    if L == 0
        error('imUtil:streaks:parfit2curve:zeroLength', ...
              'Segment has zero length (X and Y start/end are identical).');
    end

    % --- Transverse offset h(t) = a*t^2 + b*t + c ---
    h = Parfit(1).*t.^2 + Parfit(2).*t + Parfit(3);

    % --- Convert to pixel coordinates ---
    % Along-segment component: X(1) + dx*t, Y(1) + dy*t
    % Perpendicular unit vector: (-dy, dx)/L
    CurveX = X(1) + dx.*t - dy.*h./L;
    CurveY = Y(1) + dy.*t + dx.*h./L;

end
