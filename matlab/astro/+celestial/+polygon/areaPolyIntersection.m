function [Area, AreaRefPoly] = areaPolyIntersection(PolyRefLon, PolyRefLat, PolysLon, PolysLat, Args)
    % Calculate the intersection area of convex spherical polygons.
    %   See also: celestial.polygon.mex.areaPolyIntersection
    %   Calculate the intersection area between a single reference polygon
    %   and one or more polygons on the celestial sphere. Polygon edges are
    %   assumed to be minor great-circle arcs. The calculation is performed
    %   using three-dimensional unit vectors and therefore correctly treats
    %   longitude zero crossing and polygons containing a celestial pole.
    %
    %   The reference polygon is specified by vectors of longitude and
    %   latitude. The polygons to test may be specified by vectors, in which
    %   case a single polygon is assumed, or by matrices in which every
    %   column contains the vertices of one polygon. NaN-padded polygon
    %   columns are supported.
    %
    %   All polygons must be convex and should normally be contained within
    %   a hemisphere. Consecutive vertices are connected using the shorter
    %   great-circle arc. Consecutive antipodal vertices are not allowed.
    %
    % Input  : - Vector of longitudes of the reference polygon vertices.
    %          - Vector of latitudes of the reference polygon vertices.
    %          - Vector or matrix of polygon vertex longitudes. If this is a
    %            matrix, every column represents one polygon.
    %          - Vector or matrix of polygon vertex latitudes, with the same
    %            size as the polygon-longitude input.
    %          * ...,key,val,...
    %            'CooUnits' - Coordinate units of all input coordinates:
    %                   'rad' - Radians. Areas are returned in steradians.
    %                   'deg' - Degrees. Areas are returned in square
    %                           degrees.
    %                   Default is 'rad'.
    %            'TolInside' - Numerical tolerance used for spherical
    %                   half-space inclusion tests. Default is 1e-12.
    %            'TolParallel' - Tolerance for identifying parallel or
    %                   coincident great circles. Default is 1e-14.
    %            'TolDuplicate' - Euclidean chord-distance tolerance for
    %                   identifying duplicate vertices on the unit sphere.
    %                   Default is 1e-10.
    %            'UseMex' - If true use fast mex function. Default is true.
    % Output : - Row vector containing the intersection area between the
    %            reference polygon and every polygon. The output is in
    %            steradians for CooUnits='rad', and in square degrees for
    %            CooUnits='deg'.
    %          - Area of the reference polygon, in the same area units as
    %            the first output.
    % Author : ChatGPT + Eran Ofek (2026 Jul)
    % Example:
    %{
        RefLon = [350; 10; 10; 350];
        RefLat = [-10; -10; 10; 10];
        Lon    = [355; 15; 15; 355];
        Lat    = [-5; -5; 15; 15];
        [Area, AreaRefPoly] = celestial.polygon.areaPolyIntersection(RefLon, RefLat, Lon, Lat, 'CooUnits','deg');
    %}

    arguments
        PolyRefLon
        PolyRefLat
        PolysLon
        PolysLat
        Args.CooUnits        = 'rad';
        Args.TolInside       = 1e-12;
        Args.TolParallel     = 1e-14;
        Args.TolDuplicate    = 1e-10;
        Args.UseMex          = true;
    end

    if Args.UseMex
        [Area, AreaRefPoly] = celestial.polygon.mex.areaPolyIntersection(PolyRefLon, PolyRefLat, PolysLon, PolysLat, 'CooUnits',Args.CooUnits, 'TolInside',Args.TolInside, 'TolParallel',Args.TolParallel, 'TolDuplicate',Args.TolDuplicate);
        return;
    end
    %------------------------------
    % Validate basic input sizes
    %------------------------------
    if ~isvector(PolyRefLon) || ~isvector(PolyRefLat)
        error('areaPolyIntersection:ReferenceNotVector', ...
              'PolyRefLon and PolyRefLat must be vectors.');
    end

    if numel(PolyRefLon) ~= numel(PolyRefLat)
        error('areaPolyIntersection:ReferenceSizeMismatch', ...
              'PolyRefLon and PolyRefLat must have the same number of elements.');
    end

    if ~isequal(size(PolysLon),size(PolysLat))
        error('areaPolyIntersection:PolygonSizeMismatch', ...
              'PolysLon and PolysLat must have the same size.');
    end

    validateattributes(Args.TolInside,{'numeric'}, ...
                       {'real','scalar','finite','nonnegative'});
    validateattributes(Args.TolParallel,{'numeric'}, ...
                       {'real','scalar','finite','nonnegative'});
    validateattributes(Args.TolDuplicate,{'numeric'}, ...
                       {'real','scalar','finite','nonnegative'});

    CooUnits = lower(char(Args.CooUnits));

    switch CooUnits
        case 'rad'
            FactorToRad = 1;
            AreaFactor  = 1;

        case 'deg'
            FactorToRad = pi./180;
            AreaFactor  = (180./pi).^2;

        otherwise
            error('areaPolyIntersection:UnknownCooUnits', ...
                  'CooUnits must be either ''rad'' or ''deg''.');
    end

    % A vector always represents one polygon, regardless of whether it is
    % supplied as a row vector or a column vector.
    if isvector(PolysLon)
        PolysLon = PolysLon(:);
        PolysLat = PolysLat(:);
    end

    PolyRefLon = double(PolyRefLon(:)).*FactorToRad;
    PolyRefLat = double(PolyRefLat(:)).*FactorToRad;
    PolysLon   = double(PolysLon).*FactorToRad;
    PolysLat   = double(PolysLat).*FactorToRad;

    % Remove NaN entries from the reference polygon.
    GoodRef = isfinite(PolyRefLon) & isfinite(PolyRefLat);
    PolyRefLon = PolyRefLon(GoodRef);
    PolyRefLat = PolyRefLat(GoodRef);

    if numel(PolyRefLon) < 3
        error('areaPolyIntersection:TooFewReferenceVertices', ...
              'The reference polygon must contain at least three valid vertices.');
    end

    if any(abs(PolyRefLat) > pi./2 + 100.*eps)
        error('areaPolyIntersection:InvalidLatitude', ...
              'Reference-polygon latitudes must be between -pi/2 and pi/2.');
    end

    %------------------------------
    % Prepare reference polygon
    %------------------------------
    VRef = lonLatToUnitVector(PolyRefLon,PolyRefLat);
    VRef = cleanPolygonVertices(VRef,Args.TolDuplicate);

    if size(VRef,1) < 3
        error('areaPolyIntersection:DegenerateReferencePolygon', ...
              'The reference polygon has fewer than three distinct vertices.');
    end

    NRef = polygonInwardNormals(VRef,Args.TolParallel);

    % Area on the unit sphere, in steradians.
    AreaRefPoly = sphericalPolygonArea(VRef,Args.TolDuplicate);

    %------------------------------
    % Process all polygon columns
    %------------------------------
    Npoly = size(PolysLon,2);
    Area  = zeros(1,Npoly);

    for Ipoly = 1:Npoly
        Lon = PolysLon(:,Ipoly);
        Lat = PolysLat(:,Ipoly);

        Good = isfinite(Lon) & isfinite(Lat);
        Lon  = Lon(Good);
        Lat  = Lat(Good);

        if numel(Lon) < 3
            Area(Ipoly) = 0;
            continue;
        end

        if any(abs(Lat) > pi./2 + 100.*eps)
            error('areaPolyIntersection:InvalidLatitude', ...
                  'Polygon %d contains a latitude outside [-pi/2,pi/2].', ...
                  Ipoly);
        end

        VPoly = lonLatToUnitVector(Lon,Lat);
        VPoly = cleanPolygonVertices(VPoly,Args.TolDuplicate);

        if size(VPoly,1) < 3
            Area(Ipoly) = 0;
            continue;
        end

        try
            NPoly = polygonInwardNormals(VPoly,Args.TolParallel);
        catch ME
            error('areaPolyIntersection:InvalidPolygon', ...
                  'Invalid polygon in column %d: %s',Ipoly,ME.message);
        end

        Area(Ipoly) = convexIntersectionArea( ...
            VRef, NRef, VPoly, NPoly, ...
            Args.TolInside, Args.TolParallel, Args.TolDuplicate);
    end

    % Convert steradians to the requested output area units.
    Area       = Area.*AreaFactor;
    AreaRefPoly = AreaRefPoly.*AreaFactor;
end


function V = lonLatToUnitVector(Lon,Lat)
    % Convert spherical longitude and latitude to Cartesian unit vectors.

    CosLat = cos(Lat);

    V = [CosLat.*cos(Lon), ...
         CosLat.*sin(Lon), ...
         sin(Lat)];

    V = normalizeRows(V);
end


function V = cleanPolygonVertices(V,TolDuplicate)
    % Remove consecutive duplicate vertices and an optional repeated
    % closing vertex.

    if isempty(V)
        return;
    end

    V = normalizeRows(V);

    Keep = true(size(V,1),1);

    for I = 2:size(V,1)
        if norm(V(I,:) - V(I-1,:)) <= TolDuplicate
            Keep(I) = false;
        end
    end

    V = V(Keep,:);

    if size(V,1) > 1
        if norm(V(1,:) - V(end,:)) <= TolDuplicate
            V(end,:) = [];
        end
    end
end


function N = polygonInwardNormals(V,TolParallel)
    % Calculate inward-facing normals of the polygon edge great circles.

    Nv = size(V,1);

    if Nv < 3
        error('A polygon must contain at least three vertices.');
    end

    % For a convex polygon contained within a hemisphere, the normalized
    % mean of its vertices is an interior direction.
    Center = sum(V,1);
    CenterNorm = norm(Center);

    if CenterNorm <= TolParallel
        error(['Unable to determine the polygon interior. The polygon may ' ...
               'cover a hemisphere or may be degenerate.']);
    end

    Center = Center./CenterNorm;
    N      = zeros(Nv,3);

    for Iv = 1:Nv
        Jv = mod(Iv,Nv) + 1;

        EdgeNormal = cross(V(Iv,:),V(Jv,:));
        EdgeNorm   = norm(EdgeNormal);

        if EdgeNorm <= TolParallel
            error(['Two consecutive polygon vertices are identical or ' ...
                   'antipodal.']);
        end

        EdgeNormal = EdgeNormal./EdgeNorm;

        % Select the hemisphere containing the polygon center.
        if dot(EdgeNormal,Center) < 0
            EdgeNormal = -EdgeNormal;
        end

        N(Iv,:) = EdgeNormal;
    end

    % Verify convexity relative to the selected inward normals.
    Test = N*V.';

    if any(Test(:) < -1e-10)
        error(['The polygon is not convex, its vertices are not ordered, ' ...
               'or it is not contained within a hemisphere.']);
    end
end


function Area = convexIntersectionArea(V1,N1,V2,N2, ...
                                       TolInside,TolParallel,TolDuplicate)
    % Calculate the intersection area of two convex spherical polygons.

    Candidate = zeros(0,3);

    % Vertices of polygon 1 inside polygon 2.
    Inside1 = all(V1*N2.' >= -TolInside,2);

    if any(Inside1)
        Candidate = [Candidate; V1(Inside1,:)]; %#ok<AGROW>
    end

    % Vertices of polygon 2 inside polygon 1.
    Inside2 = all(V2*N1.' >= -TolInside,2);

    if any(Inside2)
        Candidate = [Candidate; V2(Inside2,:)]; %#ok<AGROW>
    end

    % Intersections between all pairs of boundary great circles.
    Nedge1 = size(N1,1);
    Nedge2 = size(N2,1);

    for I1 = 1:Nedge1
        for I2 = 1:Nedge2
            R  = cross(N1(I1,:),N2(I2,:));
            Nr = norm(R);

            if Nr > TolParallel
                Q = R./Nr;

                % The two great circles intersect at antipodal points.
                if all(N1*Q.' >= -TolInside) && ...
                   all(N2*Q.' >= -TolInside)
                    Candidate(end+1,:) = Q; %#ok<AGROW>
                end

                Q = -Q;

                if all(N1*Q.' >= -TolInside) && ...
                   all(N2*Q.' >= -TolInside)
                    Candidate(end+1,:) = Q; %#ok<AGROW>
                end
            end
        end
    end

    Candidate = uniqueUnitVectors(Candidate,TolDuplicate);

    if size(Candidate,1) < 3
        Area = 0;
        return;
    end

    Candidate = orderSphericalVertices(Candidate,TolParallel);

    Area = sphericalPolygonArea(Candidate,TolDuplicate);

    % Guard against tiny negative or nonzero numerical residuals in
    % zero-area tangential intersections.
    if Area < 100.*eps
        Area = 0;
    end
end


function V = uniqueUnitVectors(V,TolDuplicate)
    % Remove duplicate unit vectors using Euclidean chord distance.

    if isempty(V)
        return;
    end

    V = normalizeRows(V);

    UniqueV = zeros(size(V));
    Nunique = 0;

    for Iv = 1:size(V,1)
        IsDuplicate = false;

        for Iu = 1:Nunique
            if norm(V(Iv,:) - UniqueV(Iu,:)) <= TolDuplicate
                IsDuplicate = true;
                break;
            end
        end

        if ~IsDuplicate
            Nunique = Nunique + 1;
            UniqueV(Nunique,:) = V(Iv,:);
        end
    end

    V = UniqueV(1:Nunique,:);
end


function V = orderSphericalVertices(V,Tol)
    % Order convex spherical-polygon vertices around an interior direction.

    Center = sum(V,1);
    CenterNorm = norm(Center);

    if CenterNorm <= Tol
        error('Unable to determine an interior direction for the intersection.');
    end

    Center = Center./CenterNorm;

    % Choose the vertex having the largest tangent-plane component.
    ProjNorm2 = 1 - (V*Center.').^2;
    [~,Iref]  = max(ProjNorm2);

    E1 = V(Iref,:) - dot(V(Iref,:),Center).*Center;
    E1norm = norm(E1);

    if E1norm <= Tol
        error('Unable to construct a tangent-plane basis.');
    end

    E1 = E1./E1norm;
    E2 = cross(Center,E1);
    E2 = E2./norm(E2);

    Ang = atan2(V*E2.',V*E1.');

    [~,Ind] = sort(Ang);
    V = V(Ind,:);

    % Force positive orientation around Center.
    Orient = 0;
    Nv = size(V,1);

    for Iv = 1:Nv
        Jv = mod(Iv,Nv) + 1;
        Orient = Orient + dot(Center,cross(V(Iv,:),V(Jv,:)));
    end

    if Orient < 0
        V = flipud(V);
    end
end


function Area = sphericalPolygonArea(V,TolDuplicate)
    % Calculate the area of a convex spherical polygon in steradians.
    % The polygon is triangulated about an interior unit-vector direction.

    V = cleanPolygonVertices(V,TolDuplicate);

    Nv = size(V,1);

    if Nv < 3
        Area = 0;
        return;
    end

    Center = sum(V,1);
    CenterNorm = norm(Center);

    if CenterNorm <= 100.*eps
        error(['Unable to determine an interior point for the spherical ' ...
               'polygon area calculation.']);
    end

    Center = Center./CenterNorm;

    % Ensure cyclic ordering before triangulation.
    V = orderSphericalVertices(V,100.*eps);

    Omega = 0;

    for Iv = 1:Nv
        Jv = mod(Iv,Nv) + 1;

        B = V(Iv,:);
        C = V(Jv,:);

        Numerator   = dot(Center,cross(B,C));
        Denominator = 1 + dot(Center,B) + dot(B,C) + dot(C,Center);

        Omega = Omega + 2.*atan2(Numerator,Denominator);
    end

    Area = abs(Omega);
end


function V = normalizeRows(V)
    % Normalize every row of a matrix as a three-dimensional vector.

    NormV = sqrt(sum(V.^2,2));

    if any(~isfinite(NormV) | NormV == 0)
        error('areaPolyIntersection:InvalidVector', ...
              'Unable to normalize a zero or non-finite vector.');
    end

    V = V./NormV;
end