function [PolyFlag] = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2)
    % Determine a minimal subset of spherical convex polygons Poly2(:,I) whose union fully covers the spherical convex polygon Poly1.
    %   See also mex version: celestial.polygon.mex.whichPolygonsTilesPoly_mex
    %   The two functions are not identical and may return slightly different
    %   results.
    %   Algorithm:
    %   1) Project the sphere to a plane using a gnomonic projection centered
    %      inside Poly1. Great circles become straight lines.
    %   2) Convert Poly1 and all Poly2 polygons to planar polyshape objects.
    %   3) Intersect each Poly2 with Poly1.
    %   4) Test whether the union of all intersections covers Poly1.
    %   5) Search subsets in increasing size until a full cover is found.
    %
    %   Assumptions:
    %   All relevant vertices must lie in the same open hemisphere as Poly1
    %   with respect to the chosen projection center. This is the standard case
    %   for convex spherical polygons smaller than a hemisphere.
    %
    % Input  : - (LonPoly1) A vector of Poly1 longitude verteces [radians].
    %          - (LatPoly1) A vector of Poly1 latitude verteces [radians].
    %          - A matrix of Poly2 longitude vertces (column per polygon).
    %            [radians].
    %          - A matrix of Poly2 latitude vertces (column per polygon).
    %            [radians].
    %            NaNs are allowed as row padding in each column
    %
    % Output : - (PolyFlag) logical row vector, length = size(LonPoly2,2)
    %            true for polygons selected in one minimum-cardinality
    %            exact cover of Poly1.
    %            If full tiling is impossible, PolyFlag = [].
    %
    % Author : ChatGPT + Eran Ofek (Mar 2026)
    % Example: [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.10, 0.10, CenterLon, CenterLat);
    %          LonPoly2 = nan(numel(LonPoly1), 1); LatPoly2 = nan(numel(LatPoly1), 1);
    %          LonPoly2(:,1) = LonPoly1;  LatPoly2(:,1) = LatPoly1;
    %          LonPoly2 = LonPoly2 + [0.05, 0.05, -0.05, -0.05, 1, 1, 0.01];
    %          LatPoly2 = LatPoly2 + [-0.05 0.05 -0.05 0.05, 1 1, 0.01];
    %          tic; for i=1:1e2, PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2); end, T1=toc;

    %----------------------------
    % Basic checks and formatting
    %----------------------------
    PolyFlag = [];

    if nargin < 4
        error('isPolygonsTilesPolygon requires four input arguments.');
    end

    LonPoly1 = LonPoly1(:);
    LatPoly1 = LatPoly1(:);

    if numel(LonPoly1) ~= numel(LatPoly1)
        error('LonPoly1 and LatPoly1 must have the same number of elements.');
    end

    if ~isequal(size(LonPoly2), size(LatPoly2))
        error('LonPoly2 and LatPoly2 must have identical sizes.');
    end

    Npoly2 = size(LonPoly2, 2);

    if numel(LonPoly1) < 3
        error('Poly1 must have at least 3 vertices.');
    end

    if Npoly2 == 0
        return;
    end

    %---------------------------------------
    % Convert Poly1 to Cartesian unit vectors
    %---------------------------------------
    Xyz1 = localLonLat2xyz(LonPoly1, LatPoly1);

    %---------------------------------------------------------
    % Choose a projection center inside / near the middle of P1
    %---------------------------------------------------------
    Center = sum(Xyz1, 1);
    Nrm = norm(Center);
    if Nrm < 1e-14
        error('Failed to determine a stable projection center for Poly1.');
    end
    Center = Center ./ Nrm;

    %-------------------------------------------------------
    % Construct local tangent-plane orthonormal basis at Center
    %-------------------------------------------------------
    Ref = [0 0 1];
    if abs(dot(Center, Ref)) > 0.95
        Ref = [1 0 0];
    end

    E1 = cross(Ref, Center);
    E1 = E1 ./ norm(E1);
    E2 = cross(Center, E1);
    E2 = E2 ./ norm(E2);

    %--------------------------------------
    % Project Poly1 using gnomonic geometry
    %--------------------------------------
    [X1, Y1, Ok1] = localProjectGnomonic(Xyz1, Center, E1, E2);
    if ~all(Ok1)
        error(['Poly1 is not fully contained in the projection hemisphere. ', ...
               'This implementation assumes Poly1 is smaller than a hemisphere.']);
    end

    P1 = polyshape(X1, Y1, 'Simplify', true);
    P1 = rmholes(P1);

    if isempty(P1.Vertices) || area(P1) <= 0
        error('Poly1 is degenerate after projection.');
    end

    %-----------------------------------------------------
    % Project, clip, and keep only relevant Poly2 polygons
    %-----------------------------------------------------
    P2List      = cell(1, Npoly2);
    KeepFlag    = false(1, Npoly2);
    OrigIndex   = zeros(1, Npoly2);

    Nkeep = 0;
    for Ipoly = 1:Npoly2
        Lon = LonPoly2(:, Ipoly);
        Lat = LatPoly2(:, Ipoly);

        Good = ~(isnan(Lon) | isnan(Lat));
        Lon = Lon(Good);
        Lat = Lat(Good);

        if numel(Lon) < 3
            continue;
        end

        Xyz2 = localLonLat2xyz(Lon, Lat);
        [X2, Y2, Ok2] = localProjectGnomonic(Xyz2, Center, E1, E2);

        % Any vertex outside the hemisphere makes the projection invalid
        % for this simple exact implementation.
        if ~all(Ok2)
            continue;
        end

        P2 = polyshape(X2, Y2, 'Simplify', true);
        P2 = rmholes(P2);

        if isempty(P2.Vertices) || area(P2) <= 0
            continue;
        end

        % Clip to Poly1: only the part inside Poly1 matters.
        Pint = intersect(P1, P2);
        Pint = rmholes(Pint);

        if isempty(Pint.Vertices) || area(Pint) <= 0
            continue;
        end

        Nkeep = Nkeep + 1;
        KeepFlag(Ipoly) = true;
        OrigIndex(Nkeep) = Ipoly;
        P2List{Nkeep} = Pint;
    end

    if Nkeep == 0
        return;
    end

    P2List = P2List(1:Nkeep);
    OrigIndex = OrigIndex(1:Nkeep);

    %----------------------------------------
    % Quick impossibility test: union of all P2
    %----------------------------------------
    UnionAll = P2List{1};
    for I = 2:Nkeep
        UnionAll = union(UnionAll, P2List{I});
    end
    UnionAll = rmholes(UnionAll);

    if ~localIsCovered(P1, UnionAll)
        PolyFlag = [];
        return;
    end

    %---------------------------------------------------------
    % Remove redundant exact duplicates / contained duplicates
    %---------------------------------------------------------
    Active = true(1, Nkeep);
    Areas = zeros(1, Nkeep);
    for I = 1:Nkeep
        Areas(I) = area(P2List{I});
    end

    % If one polygon is completely contained in another clipped polygon,
    % the smaller one is never required in a minimum-cardinality cover.
    for I = 1:Nkeep
        if ~Active(I), continue; end
        for J = 1:Nkeep
            if I == J || ~Active(J), continue; end

            if Areas(I) <= Areas(J)
                if localIsCovered(P2List{I}, P2List{J})
                    Active(I) = false;
                    break;
                end
            end
        end
    end

    P2Work    = P2List(Active);
    OrigWork  = OrigIndex(Active);
    Nwork     = numel(P2Work);

    % Re-check global coverage after pruning
    UnionAll = P2Work{1};
    for I = 2:Nwork
        UnionAll = union(UnionAll, P2Work{I});
    end
    UnionAll = rmholes(UnionAll);

    if ~localIsCovered(P1, UnionAll)
        % This should not usually happen, but keep it safe.
        PolyFlag = [];
        return;
    end

    %---------------------------------------
    % Search minimum-cardinality exact cover
    %---------------------------------------
    BestSubset = [];

    % First try a greedy upper bound, so combinations larger than this
    % need not be tested.
    GreedySubset = localGreedyCover(P1, P2Work);
    if isempty(GreedySubset)
        PolyFlag = [];
        return;
    end
    MaxK = numel(GreedySubset);

    % Test subsets in increasing cardinality.
    for K = 1:MaxK
        Comb = nchoosek(1:Nwork, K);
        for Icomb = 1:size(Comb, 1)
            Idx = Comb(Icomb, :);

            U = P2Work{Idx(1)};
            for J = 2:numel(Idx)
                U = union(U, P2Work{Idx(J)});
            end
            U = rmholes(U);

            if localIsCovered(P1, U)
                BestSubset = Idx;
                break;
            end
        end

        if ~isempty(BestSubset)
            break;
        end
    end

    if isempty(BestSubset)
        PolyFlag = [];
        return;
    end

    %-------------------------------
    % Map back to original Poly2 list
    %-------------------------------
    PolyFlag = false(1, Npoly2);
    PolyFlag(OrigWork(BestSubset)) = true;

end


% =========================================================================
function Xyz = localLonLat2xyz(Lon, Lat)
% Convert spherical lon/lat [rad] to 3D unit vectors.

    Clat = cos(Lat);
    Xyz = [Clat .* cos(Lon), ...
           Clat .* sin(Lon), ...
           sin(Lat)];
end


% =========================================================================
function [X, Y, Ok] = localProjectGnomonic(Xyz, Center, E1, E2)
% Gnomonic projection onto tangent plane at Center.
% For unit vector P:
%   X = dot(P,E1)/dot(P,Center)
%   Y = dot(P,E2)/dot(P,Center)
% Valid only if dot(P,Center) > 0.

    Den = Xyz * Center(:);
    Ok = Den > 1e-12;

    X = nan(size(Den));
    Y = nan(size(Den));

    X(Ok) = (Xyz(Ok,:) * E1(:)) ./ Den(Ok);
    Y(Ok) = (Xyz(Ok,:) * E2(:)) ./ Den(Ok);
end


% =========================================================================
function Flag = localIsCovered(Ptarget, Pcover)
% True if Pcover fully covers Ptarget, up to numerical tolerance.

    Diff = subtract(Ptarget, Pcover);
    Diff = rmholes(Diff);

    Atarget = area(Ptarget);
    Adiff   = area(Diff);

    TolArea = max(1e-12, 1e-10 * max(1, Atarget));
    Flag = (Adiff <= TolArea);
end


% =========================================================================
function Subset = localGreedyCover(P1, P2List)
% Greedy cover used only to obtain an upper bound for exact search.

    N = numel(P2List);
    Remaining = P1;
    Used = false(1, N);
    Subset = [];

    while true
        Arem = area(Remaining);
        if Arem <= max(1e-12, 1e-10 * max(1, area(P1)))
            return;
        end

        BestGain = -inf;
        BestI = 0;

        for I = 1:N
            if Used(I)
                continue;
            end
            GainPoly = intersect(Remaining, P2List{I});
            GainVal = area(GainPoly);

            if GainVal > BestGain
                BestGain = GainVal;
                BestI = I;
            end
        end

        if BestI == 0 || BestGain <= max(1e-14, 1e-12 * max(1, area(P1)))
            Subset = [];
            return;
        end

        Used(BestI) = true;
        Subset(end+1) = BestI; %#ok<AGROW>
        Remaining = subtract(Remaining, P2List{BestI});
        Remaining = rmholes(Remaining);
    end
end
