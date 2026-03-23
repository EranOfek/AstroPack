function Flag = isSpherePolyIntersect(LonPoly1, LatPoly1, LonPoly2, LatPoly2, Args)
    % Test intersection of convex spherical polygons.
    %   The function test if one polygon (in set 1) intersects all the polygons
    %   in set 2. If set 1 and set 2 has the same number of polygons, then test
    %   polygons one to one.
    %   The polygons must be convex and the function doesn't verify this.
    % Description:
    %     Test whether convex polygons on the celestial sphere intersect.
    %     The function compares polygon columns between two polygon sets:
    %       - If both inputs contain the same number of polygon columns, then
    %         polygon I in set 1 is compared with polygon I in set 2.
    %       - If one input contains a single polygon column and the other input
    %         contains multiple columns, then the single polygon is compared
    %         against every polygon in the other input.
    %
    % Input  : - Matrix of longitudes for polygon set 1. Size is [Nvert1,Npoly1].
    %            Each column contains one polygon.
    %          - Matrix of latitudes  for polygon set 1. Size is [Nvert1,Npoly1].
    %          - Matrix of longitudes for polygon set 2. Size is [Nvert2,Npoly2].
    %            Each column contains one polygon.
    %          - Matrix of latitudes  for polygon set 2. Size is [Nvert2,Npoly2].
    %          * ...,key,val,... 
    %            'IsDeg' - Logical scalar indicating if inputs are in degrees.
    %                       Default is true.
    %            'IncludeEdge' - Logical scalar indicating if touching at an edge or
    %                        a vertex counts as intersection.
    %                        Default is true.
    %            'UseMex' - A logical indicating if to use mex version.
    %                       Default is true.
    %
    % Output : Logical column vector of length Ncmp, where:
    %          Ncmp = max(Npoly1,Npoly2), provided one of them is 1 or both are equal.
    %
    % Method:
    %     The function tests intersection between convex polygons on the
    %     celestial sphere. Comparisons are performed column-wise, with scalar
    %     expansion in the polygon-column dimension when one side has a single
    %     polygon.
    %
    %     The algorithm works in 3-D Cartesian coordinates on the unit sphere,
    %     rather than directly in longitude/latitude. This avoids problems with
    %     longitude wrap-around and with behavior near the poles.
    %
    %     Main steps:
    %
    %     1) Convert vertices from spherical coordinates to 3-D unit vectors
    %        Each polygon vertex (Lon,Lat) is converted into a unit vector
    %        [X,Y,Z] on the sphere.
    %
    %     2) Construct spherical polygon edges
    %        Consecutive rows in each column are interpreted as consecutive
    %        polygon vertices, with wrap-around from the last row back to the
    %        first row.
    %
    %     3) Build inward edge normals
    %        For each edge, the code computes the great-circle normal using a
    %        cross product between adjacent vertex vectors.
    %        The normals are then oriented so that they point toward the polygon
    %        interior. This is done using the average polygon direction.
    %
    %     4) Vertex-in-polygon tests
    %        A point is inside a convex spherical polygon if it lies on the
    %        inner side of all its edge great circles.
    %        Therefore, the code first checks:
    %           - whether any vertex of polygon 1 lies inside polygon 2
    %           - whether any vertex of polygon 2 lies inside polygon 1
    %        These tests are vectorized over polygon columns.
    %
    %     5) Edge-edge intersection tests for unresolved cases
    %        If no vertex of one polygon lies inside the other, the polygons may
    %        still intersect through crossing edges.
    %        For such unresolved polygon pairs, the code tests all edge pairs.
    %
    %        For two spherical edges:
    %           - each edge defines a great circle
    %           - the two great circles intersect at two antipodal points
    %           - each candidate intersection point is tested to determine
    %             whether it lies on both finite spherical edge arcs
    %
    %        If any such point lies on both arcs, the polygons intersect.
    %
    %     6) Boundary handling
    %        The behavior at polygon boundaries is controlled by
    %        Args.IncludeEdge:
    %           - true  : touching at an edge or vertex counts as intersection
    %           - false : only strict overlap counts
    %
    %     Efficiency:
    %        - Vertex-in-polygon tests are vectorized over all polygon columns.
    %        - The more expensive edge-edge tests are only applied to polygon
    %          pairs not already classified by the vertex tests.
    %        - This makes the function efficient for batches of convex polygons,
    %          especially when many pairs are resolved early.
    %
    %     Assumptions:
    %        - Polygons are convex spherical polygons.
    %        - The rows of each column contain the polygon vertices in order.
    %        - Within each polygon set, Lon/Lat matrices must have identical size.
    %        - The two sets may have different numbers of rows.
    %        - The number of columns must either be equal, or one of them must be 1.
    %
    % Author : ChatGPT + Eran Ofek (Mar 2026)
    % Example:
    %    LonPoly1=[1;1;0;0]; LatPoly1=[1;0;1;0]; LonPoly2=rand(4,100); LatPoly2=rand(4,100);           
    %    Flag = celestial.polygon.isSpherePolyIntersect(LonPoly1, LatPoly1, LonPoly2, LatPoly2);

    
    arguments
        LonPoly1 
        LatPoly1 
        LonPoly2 
        LatPoly2 
        Args.IsDeg          = true;
        Args.IncludeEdge    = true;
        Args.UseMex         = true;
    end

    if Args.UseMex
        Flag = celestial.polygon.mex.isSpherePolyIntersect_mex(LonPoly1, LatPoly1, LonPoly2, LatPoly2, Args.IsDeg, Args.IncludeEdge);
    else
    
        
        if ~isequal(size(LonPoly1), size(LatPoly1))
            error('LonPoly1 and LatPoly1 must have identical size.');
        end
        if ~isequal(size(LonPoly2), size(LatPoly2))
            error('LonPoly2 and LatPoly2 must have identical size.');
        end
        
        Nvert1 = size(LonPoly1, 1);
        Npoly1 = size(LonPoly1, 2);
        Nvert2 = size(LonPoly2, 1);
        Npoly2 = size(LonPoly2, 2);
        
        if Nvert1 < 3 || Nvert2 < 3
            error('Each polygon must have at least 3 rows.');
        end
        
        if ~(Npoly1 == Npoly2 || Npoly1 == 1 || Npoly2 == 1)
            error('Number of polygon columns must be equal, or one input must contain a single polygon column.');
        end
        
        Ncmp = max(Npoly1, Npoly2);
        
        if Npoly1 == 1 && Ncmp > 1
            LonPoly1 = repmat(LonPoly1, 1, Ncmp);
            LatPoly1 = repmat(LatPoly1, 1, Ncmp);
        end
        
        if Npoly2 == 1 && Ncmp > 1
            LonPoly2 = repmat(LonPoly2, 1, Ncmp);
            LatPoly2 = repmat(LatPoly2, 1, Ncmp);
        end
        
        if Args.IsDeg
            Factor = pi ./ 180;
            LonPoly1 = LonPoly1 .* Factor;
            LatPoly1 = LatPoly1 .* Factor;
            LonPoly2 = LonPoly2 .* Factor;
            LatPoly2 = LatPoly2 .* Factor;
        end
        
        [X1, Y1, Z1] = sph2cartUnit(LonPoly1, LatPoly1);
        [X2, Y2, Z2] = sph2cartUnit(LonPoly2, LatPoly2);
        
        X1n = X1([2:end 1], :);
        Y1n = Y1([2:end 1], :);
        Z1n = Z1([2:end 1], :);
        
        X2n = X2([2:end 1], :);
        Y2n = Y2([2:end 1], :);
        Z2n = Z2([2:end 1], :);
        
        Nx1 = Y1 .* Z1n - Z1 .* Y1n;
        Ny1 = Z1 .* X1n - X1 .* Z1n;
        Nz1 = X1 .* Y1n - Y1 .* X1n;
        
        Nx2 = Y2 .* Z2n - Z2 .* Y2n;
        Ny2 = Z2 .* X2n - X2 .* Z2n;
        Nz2 = X2 .* Y2n - Y2 .* X2n;
        
        Cx1 = sum(X1, 1, 'omitnan');
        Cy1 = sum(Y1, 1, 'omitnan');
        Cz1 = sum(Z1, 1, 'omitnan');
        NormC1 = sqrt(Cx1.^2 + Cy1.^2 + Cz1.^2);
        Cx1 = Cx1 ./ NormC1;
        Cy1 = Cy1 ./ NormC1;
        Cz1 = Cz1 ./ NormC1;
        
        Cx2 = sum(X2, 1, 'omitnan');
        Cy2 = sum(Y2, 1, 'omitnan');
        Cz2 = sum(Z2, 1, 'omitnan');
        NormC2 = sqrt(Cx2.^2 + Cy2.^2 + Cz2.^2);
        Cx2 = Cx2 ./ NormC2;
        Cy2 = Cy2 ./ NormC2;
        Cz2 = Cz2 ./ NormC2;
        
        Sign1 = sign(sum(Nx1 .* Cx1 + Ny1 .* Cy1 + Nz1 .* Cz1, 1, 'omitnan'));
        Sign1(Sign1 == 0) = 1;
        Nx1 = Nx1 .* Sign1;
        Ny1 = Ny1 .* Sign1;
        Nz1 = Nz1 .* Sign1;
        
        Sign2 = sign(sum(Nx2 .* Cx2 + Ny2 .* Cy2 + Nz2 .* Cz2, 1, 'omitnan'));
        Sign2(Sign2 == 0) = 1;
        Nx2 = Nx2 .* Sign2;
        Ny2 = Ny2 .* Sign2;
        Nz2 = Nz2 .* Sign2;
        
        NormN1 = sqrt(Nx1.^2 + Ny1.^2 + Nz1.^2);
        NormN2 = sqrt(Nx2.^2 + Ny2.^2 + Nz2.^2);
        
        Nx1 = Nx1 ./ NormN1;
        Ny1 = Ny1 ./ NormN1;
        Nz1 = Nz1 ./ NormN1;
        
        Nx2 = Nx2 ./ NormN2;
        Ny2 = Ny2 ./ NormN2;
        Nz2 = Nz2 ./ NormN2;
        
        Tol = 1e-12;
        if ~isa(LonPoly1, 'double')
            Tol = 1e-6;
        end
        
        Inside12 = false(1, Ncmp);
        for Ivert = 1:Nvert1
            S12 = Nx2 .* X1(Ivert, :) + Ny2 .* Y1(Ivert, :) + Nz2 .* Z1(Ivert, :);
            if Args.IncludeEdge
                Inside12 = Inside12 | all(S12 >= -Tol, 1);
            else
                Inside12 = Inside12 | all(S12 > Tol, 1);
            end
        end
        
        Inside21 = false(1, Ncmp);
        for Ivert = 1:Nvert2
            S21 = Nx1 .* X2(Ivert, :) + Ny1 .* Y2(Ivert, :) + Nz1 .* Z2(Ivert, :);
            if Args.IncludeEdge
                Inside21 = Inside21 | all(S21 >= -Tol, 1);
            else
                Inside21 = Inside21 | all(S21 > Tol, 1);
            end
        end
        
        Flag = Inside12 | Inside21;
        ToDo = find(~Flag);
        
        if isempty(ToDo)
            Flag = Flag(:);
            return;
        end
        
        for Ipoly = ToDo(:).'
            Found = false;
        
            for Iedge1 = 1:Nvert1
                U1 = [X1(Iedge1, Ipoly), Y1(Iedge1, Ipoly), Z1(Iedge1, Ipoly)];
                U2 = [X1n(Iedge1, Ipoly), Y1n(Iedge1, Ipoly), Z1n(Iedge1, Ipoly)];
                N1 = [Nx1(Iedge1, Ipoly), Ny1(Iedge1, Ipoly), Nz1(Iedge1, Ipoly)];
        
                for Iedge2 = 1:Nvert2
                    V1 = [X2(Iedge2, Ipoly), Y2(Iedge2, Ipoly), Z2(Iedge2, Ipoly)];
                    V2 = [X2n(Iedge2, Ipoly), Y2n(Iedge2, Ipoly), Z2n(Iedge2, Ipoly)];
                    N2 = [Nx2(Iedge2, Ipoly), Ny2(Iedge2, Ipoly), Nz2(Iedge2, Ipoly)];
        
                    Xint = cross(N1, N2);
                    NormX = norm(Xint);
                    if NormX <= Tol
                        continue;
                    end
        
                    Xint = Xint ./ NormX;
        
                    if pointOnArc(Xint, U1, U2, N1, Tol, Args.IncludeEdge) && ...
                       pointOnArc(Xint, V1, V2, N2, Tol, Args.IncludeEdge)
                        Found = true;
                        break;
                    end
        
                    Xint = -Xint;
        
                    if pointOnArc(Xint, U1, U2, N1, Tol, Args.IncludeEdge) && ...
                       pointOnArc(Xint, V1, V2, N2, Tol, Args.IncludeEdge)
                        Found = true;
                        break;
                    end
                end
        
                if Found
                    break;
                end
            end
        
            Flag(Ipoly) = Found;
        end
        
        Flag = Flag(:);
    end

end

% Aux functions:

function [X, Y, Z] = sph2cartUnit(Lon, Lat)
    CosLat = cos(Lat);
    X = CosLat .* cos(Lon);
    Y = CosLat .* sin(Lon);
    Z = sin(Lat);
end

function Result = pointOnArc(P, A, B, N, Tol, IncludeEdge)
    OnGreatCircle = abs(dot(N, P)) <= Tol;
    if ~OnGreatCircle
        Result = false;
        return;
    end
    
    S1 = dot(cross(A, P), N);
    S2 = dot(cross(P, B), N);
    
    if IncludeEdge
        Result = (S1 >= -Tol) && (S2 >= -Tol);
    else
        Result = (S1 > Tol) && (S2 > Tol);
    end
end
