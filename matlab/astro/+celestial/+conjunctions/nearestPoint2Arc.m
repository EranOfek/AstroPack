function [Time_Nearest, Dist_Nearest, Arc_RA_Nearest, Arc_Dec_Nearest, Segment_Index] = nearestPoint2Arc(Arc_Time, Arc_RA, Arc_Dec, Point_RA, Point_Dec, Args)
    % Find nearest point on a continuous spherical arc to sky positions.
    % Package: celestial.coo
    % Description: Given an ordered list of arc points in celestial coordinates
    %              (RA, Dec), find for each input sky position the nearest point
    %              on the continuous arc. The arc is treated as a sequence of
    %              consecutive great-circle segments connecting the arc samples.
    %              Hence, the nearest point may be:
    %              (1) one of the arc vertices, or
    %              (2) an interior point on one of the great-circle segments.
    % Input  : - Vector of times associated with the arc samples.
    %          - Vector of arc right ascensions.
    %          - Vector of arc declinations.
    %          - Scalar/vector/matrix of point right ascensions.
    %          - Scalar/vector/matrix of point declinations.
    %          * ...,key,val,...
    %            'CooUnits'            - Coordinate units. Options are:
    %                                       'deg' | 'rad'. Default is 'deg'.
    %            'Tol'                 - Tolerance, in radians, used to test
    %                                       whether the projected point lies on
    %                                       the segment. Default is 1e-10.
    %            'DegenerateSegTol'    - Threshold, in radians, below which a
    %                                       segment is treated as zero length.
    %                                       Default is 1e-12.
    %            'NormalTol'           - Threshold for ill-conditioned great-
    %                                       circle normals / projections.
    %                                       Default is 1e-14.
    % Output : - Array of nearest times, one per input point. If the nearest
    %            point lies inside a segment, then the time is interpolated
    %            linearly with angular distance along the segment.
    %          - Array of nearest angular distances. Returned in the same units
    %            as the requested coordinate units.
    %          - Array of nearest-point right ascensions.
    %          - Array of nearest-point declinations.
    %          - Array of nearest segment indices. If the nearest point is on
    %            segment I connecting arc points I and I+1, then the returned
    %            segment index is I. If the nearest point is exactly a vertex,
    %            then one of the adjacent segment indices may be returned.
    %            For a single-point arc, the segment index is NaN.
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % Example: 
    %{       
               Arc_Time = [1; 2; 3];
               Arc_RA   = [358; 2; 10];
               Arc_Dec  = [  0; 1;  2];
               Point_RA = [359; 1; 7];
               Point_Dec= [0.2; 0.5; 1.5];
               [Time_Nearest, Dist_Nearest, Arc_RA_Nearest, Arc_Dec_Nearest, Segment_Index] = ...
                   celestial.conjunctions.nearestPoint2Arc(Arc_Time, Arc_RA, Arc_Dec, Point_RA, Point_Dec);
    %}
    % Notes  : - The arc is treated as a continuous piecewise great-circle arc.
    %          - The function does not fit a spline/interpolant in RA/Dec.
    %          - The implementation is robust to RA wrap (e.g., near RA=0),
    %            because all geometry is done in Cartesian unit vectors.
    %          - Consecutive identical arc points are treated as zero-length
    %            segments and are ignored as continuous segments.
    %          - Internally all geometric calculations are performed in radians.
    %          - Point_RA and Point_Dec must have identical size.
    
    arguments
        Arc_Time (:,1) double
        Arc_RA   (:,1) double
        Arc_Dec  (:,1) double
        Point_RA {mustBeNumeric}
        Point_Dec {mustBeNumeric}
        Args.CooUnits (1,:) char {mustBeMember(Args.CooUnits, {'deg','rad'})} = 'deg'
        Args.Tol (1,1) double {mustBePositive} = 1e-10
        Args.DegenerateSegTol (1,1) double {mustBePositive} = 1e-12
        Args.NormalTol (1,1) double {mustBePositive} = 1e-14
    end

    %----------------------%
    % Input validation
    %----------------------%
    if ~isequal(size(Point_RA), size(Point_Dec))
        error('Point_RA and Point_Dec must have identical size');
    end

    Narc = numel(Arc_Time);
    if numel(Arc_RA)~=Narc || numel(Arc_Dec)~=Narc
        error('Arc_Time, Arc_RA, and Arc_Dec must have the same number of elements');
    end

    if Narc < 1
        error('Input arc must contain at least one point');
    end

    Size_Point = size(Point_RA);

    % Keep only finite arc samples
    FlagArc = isfinite(Arc_Time) & isfinite(Arc_RA) & isfinite(Arc_Dec);
    Arc_Time = Arc_Time(FlagArc);
    Arc_RA   = Arc_RA(FlagArc);
    Arc_Dec  = Arc_Dec(FlagArc);

    if isempty(Arc_Time)
        error('No finite arc points remain after filtering NaN/Inf values');
    end

    Narc = numel(Arc_Time);

    % Flatten point arrays
    Point_RA  = Point_RA(:);
    Point_Dec = Point_Dec(:);
    Np = numel(Point_RA);

    %----------------------%
    % Convert to radians
    %----------------------%
    switch lower(Args.CooUnits)
        case 'deg'
            Factor = pi./180;
        case 'rad'
            Factor = 1;
    end

    Arc_RA   = Arc_RA   .* Factor;
    Arc_Dec  = Arc_Dec  .* Factor;
    Point_RA = Point_RA .* Factor;
    Point_Dec= Point_Dec.* Factor;

    %----------------------%
    % Initialize outputs
    %----------------------%
    Time_Nearest   = nan(Np, 1);
    Dist_Nearest   = nan(Np, 1);
    Arc_RA_Nearest = nan(Np, 1);
    Arc_Dec_Nearest= nan(Np, 1);
    Segment_Index  = nan(Np, 1);

    FlagPoint = isfinite(Point_RA) & isfinite(Point_Dec);
    if ~any(FlagPoint)
        Time_Nearest   = reshape(Time_Nearest,   Size_Point);
        Dist_Nearest   = reshape(Dist_Nearest,   Size_Point);
        Arc_RA_Nearest = reshape(Arc_RA_Nearest, Size_Point);
        Arc_Dec_Nearest= reshape(Arc_Dec_Nearest,Size_Point);
        Segment_Index  = reshape(Segment_Index,  Size_Point);
        return;
    end

    %----------------------%
    % Special case: one-point arc
    %----------------------%
    if Narc == 1
        Igood = find(FlagPoint);
        Dist_Nearest(Igood)   = celestial.coo.sphere_dist_fast(Point_RA(Igood), Point_Dec(Igood), Arc_RA(1), Arc_Dec(1));
        Time_Nearest(Igood)   = Arc_Time(1);
        Arc_RA_Nearest(Igood) = mod(Arc_RA(1), 2.*pi);
        Arc_Dec_Nearest(Igood)= Arc_Dec(1);
        % Segment_Index remains NaN

        if strcmpi(Args.CooUnits, 'deg')
            Dist_Nearest   = Dist_Nearest .* 180./pi;
            Arc_RA_Nearest = Arc_RA_Nearest .* 180./pi;
            Arc_Dec_Nearest= Arc_Dec_Nearest .* 180./pi;
        end

        Time_Nearest   = reshape(Time_Nearest,   Size_Point);
        Dist_Nearest   = reshape(Dist_Nearest,   Size_Point);
        Arc_RA_Nearest = reshape(Arc_RA_Nearest, Size_Point);
        Arc_Dec_Nearest= reshape(Arc_Dec_Nearest,Size_Point);
        Segment_Index  = reshape(Segment_Index,  Size_Point);
        return;
    end

    %----------------------%
    % Cartesian coordinates
    %----------------------%
    [Arc_CD1, Arc_CD2, Arc_CD3] = celestial.coo.coo2cosined(Arc_RA, Arc_Dec);
    Arc_Coo = [Arc_CD1(:), Arc_CD2(:), Arc_CD3(:)];

    [Point_CD1, Point_CD2, Point_CD3] = celestial.coo.coo2cosined(Point_RA(FlagPoint), Point_Dec(FlagPoint));
    Point_Coo = [Point_CD1(:), Point_CD2(:), Point_CD3(:)];

    % Work only on finite points
    Igood = find(FlagPoint);
    Ngood = numel(Igood);

    %----------------------%
    % Initialize with nearest vertex
    %----------------------%
    Dist_Best = inf(Ngood, 1);
    Time_Best = nan(Ngood, 1);
    RA_Best   = nan(Ngood, 1);
    Dec_Best  = nan(Ngood, 1);
    Seg_Best  = nan(Ngood, 1);

    for Iarc = 1:Narc
        Dist_I = celestial.coo.sphere_dist_fast(Point_RA(Igood), Point_Dec(Igood), Arc_RA(Iarc), Arc_Dec(Iarc));
        FlagBetter = Dist_I < Dist_Best;

        if any(FlagBetter)
            Dist_Best(FlagBetter) = Dist_I(FlagBetter);
            Time_Best(FlagBetter) = Arc_Time(Iarc);
            RA_Best(FlagBetter)   = Arc_RA(Iarc);
            Dec_Best(FlagBetter)  = Arc_Dec(Iarc);

            if Iarc==1
                Seg_Best(FlagBetter) = 1;
            elseif Iarc==Narc
                Seg_Best(FlagBetter) = Narc - 1;
            else
                Seg_Best(FlagBetter) = Iarc;
            end
        end
    end

    %----------------------%
    % Test all segment interiors
    %----------------------%
    for Iseg = 1:(Narc - 1)
        A = Arc_Coo(Iseg,   :);
        B = Arc_Coo(Iseg+1, :);

        RA_A  = Arc_RA(Iseg);
        Dec_A = Arc_Dec(Iseg);
        RA_B  = Arc_RA(Iseg+1);
        Dec_B = Arc_Dec(Iseg+1);

        Dist_AB = celestial.coo.sphere_dist_fast(RA_A, Dec_A, RA_B, Dec_B);

        % Zero-length or numerically degenerate segment
        if ~(isfinite(Dist_AB) && Dist_AB > Args.DegenerateSegTol)
            continue;
        end

        % Great-circle normal
        Nvec = cross(A, B);
        Nnorm = hypot(hypot(Nvec(1), Nvec(2)), Nvec(3));
        if Nnorm < Args.NormalTol
            continue;
        end
        Nvec = Nvec ./ Nnorm;

        % Orthogonal projection of all query points onto great-circle plane
        Proj = Point_Coo - (Point_Coo * Nvec.').*Nvec;
        ProjNorm = sqrt(sum(Proj.^2, 2));

        FlagProj = ProjNorm > Args.NormalTol;
        if ~any(FlagProj)
            continue;
        end

        Q = Proj(FlagProj, :) ./ ProjNorm(FlagProj);
        [Q_RA, Q_Dec] = celestial.coo.cosined2coo(Q(:,1), Q(:,2), Q(:,3));
        Q_RA = mod(Q_RA, 2.*pi);

        Dist_AQ = celestial.coo.sphere_dist_fast(RA_A, Dec_A, Q_RA, Q_Dec);
        Dist_QB = celestial.coo.sphere_dist_fast(Q_RA, Q_Dec, RA_B, Dec_B);

        % Point lies on minor arc AB
        FlagOnSeg_Local = abs((Dist_AQ + Dist_QB) - Dist_AB) <= Args.Tol;
        if ~any(FlagOnSeg_Local)
            continue;
        end

        Iproj = find(FlagProj);
        Iuse  = Iproj(FlagOnSeg_Local);

        Dist_PQ = celestial.coo.sphere_dist_fast(Point_RA(Igood(Iuse)), Point_Dec(Igood(Iuse)), Q_RA(FlagOnSeg_Local), Q_Dec(FlagOnSeg_Local));
        FlagBetter = Dist_PQ < Dist_Best(Iuse);

        if any(FlagBetter)
            Iupd = Iuse(FlagBetter);

            Frac = Dist_AQ(FlagOnSeg_Local) ./ Dist_AB;
            Frac = Frac(FlagBetter);

            Dist_Best(Iupd) = Dist_PQ(FlagBetter);
            Time_Best(Iupd) = Arc_Time(Iseg) + Frac.*(Arc_Time(Iseg+1) - Arc_Time(Iseg));
            RA_Best(Iupd)   = Q_RA(FlagOnSeg_Local);
            RA_Best(Iupd)   = RA_Best(Iupd);
            Dec_Best(Iupd)  = Q_Dec(FlagOnSeg_Local);
            Dec_Best(Iupd)  = Dec_Best(Iupd);
            Seg_Best(Iupd)  = Iseg;

            % Fix indexing explicitly
            Q_RA_Use  = Q_RA(FlagOnSeg_Local);
            Q_Dec_Use = Q_Dec(FlagOnSeg_Local);
            RA_Best(Iupd)  = Q_RA_Use(FlagBetter);
            Dec_Best(Iupd) = Q_Dec_Use(FlagBetter);
        end
    end

    %----------------------%
    % Store good-point results
    %----------------------%
    Time_Nearest(Igood)   = Time_Best;
    Dist_Nearest(Igood)   = Dist_Best;
    Arc_RA_Nearest(Igood) = mod(RA_Best, 2.*pi);
    Arc_Dec_Nearest(Igood)= Dec_Best;
    Segment_Index(Igood)  = Seg_Best;

    %----------------------%
    % Convert back to requested units
    %----------------------%
    if strcmpi(Args.CooUnits, 'deg')
        Dist_Nearest   = Dist_Nearest .* 180./pi;
        Arc_RA_Nearest = Arc_RA_Nearest .* 180./pi;
        Arc_Dec_Nearest= Arc_Dec_Nearest .* 180./pi;
    end

    %----------------------%
    % Reshape to input size
    %----------------------%
    Time_Nearest   = reshape(Time_Nearest,   Size_Point);
    Dist_Nearest   = reshape(Dist_Nearest,   Size_Point);
    Arc_RA_Nearest = reshape(Arc_RA_Nearest, Size_Point);
    Arc_Dec_Nearest= reshape(Arc_Dec_Nearest,Size_Point);
    Segment_Index  = reshape(Segment_Index,  Size_Point);

end