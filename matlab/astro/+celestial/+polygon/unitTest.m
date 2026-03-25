function [Result] = unitTest()
    % unitTest for celestial.polygon
    % Example: celestial.polygon.unitTest
   
    Result=test_whichPolygonsTilesPoly();

    Result = true;
end



function Result=test_whichPolygonsTilesPoly()

    CenterLon = 0.0;
    CenterLat = 0.0;

    %% Test 1: Single polygon exact cover
    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.10, 0.10, CenterLon, CenterLat);

    LonPoly2 = nan(numel(LonPoly1), 1);
    LatPoly2 = nan(numel(LatPoly1), 1);
    LonPoly2(:,1) = LonPoly1;
    LatPoly2(:,1) = LatPoly1;

    PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1])), 'Test 1 failed');


    %% Test 2: Two tiles needed
    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.05, 0.05, CenterLon, CenterLat);
    [LonA, LatA] = localPlaneRectToSphere(-0.10, 0.00, -0.05, 0.05, CenterLon, CenterLat);
    [LonB, LatB] = localPlaneRectToSphere( 0.00, 0.10, -0.05, 0.05, CenterLon, CenterLat);

    Nrow = max([numel(LonA), numel(LonB)]);
    LonPoly2 = nan(Nrow, 2);
    LatPoly2 = nan(Nrow, 2);

    LonPoly2(1:numel(LonA),1) = LonA;
    LatPoly2(1:numel(LatA),1) = LatA;

    LonPoly2(1:numel(LonB),2) = LonB;
    LatPoly2(1:numel(LatB),2) = LatB;

    PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1 1])), 'Test 2 failed');


    %% Test 3: Redundant polygon not required
    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.05, 0.05, CenterLon, CenterLat);
    [LonA, LatA] = localPlaneRectToSphere(-0.10, 0.00, -0.05, 0.05, CenterLon, CenterLat);
    [LonB, LatB] = localPlaneRectToSphere( 0.00, 0.10, -0.05, 0.05, CenterLon, CenterLat);
    [LonC, LatC] = localPlaneRectToSphere(-0.02, 0.02, -0.02, 0.02, CenterLon, CenterLat);

    Nrow = max([numel(LonA), numel(LonB), numel(LonC)]);
    LonPoly2 = nan(Nrow, 3);
    LatPoly2 = nan(Nrow, 3);

    LonPoly2(1:numel(LonA),1) = LonA;
    LatPoly2(1:numel(LatA),1) = LatA;

    LonPoly2(1:numel(LonB),2) = LonB;
    LatPoly2(1:numel(LatB),2) = LatB;

    LonPoly2(1:numel(LonC),3) = LonC;
    LatPoly2(1:numel(LatC),3) = LatC;

    PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1 1 0])), 'Test 3 failed');


    %% Test 4: Impossible cover returns empty
    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.05, 0.05, CenterLon, CenterLat);
    [LonA, LatA] = localPlaneRectToSphere(-0.10, -0.01, -0.05, 0.05, CenterLon, CenterLat);
    [LonB, LatB] = localPlaneRectToSphere( 0.01,  0.10, -0.05, 0.05, CenterLon, CenterLat);

    Nrow = max([numel(LonA), numel(LonB)]);
    LonPoly2 = nan(Nrow, 2);
    LatPoly2 = nan(Nrow, 2);

    LonPoly2(1:numel(LonA),1) = LonA;
    LatPoly2(1:numel(LatA),1) = LatA;

    LonPoly2(1:numel(LonB),2) = LonB;
    LatPoly2(1:numel(LatB),2) = LatB;

    PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isempty(PolyFlag), 'Test 4 failed');


    %% Test 5: Disjoint polygon ignored
    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.10, 0.10, CenterLon, CenterLat);
    [LonFar, LatFar]     = localPlaneRectToSphere( 0.70, 0.80,  0.40, 0.50, CenterLon, CenterLat);

    Nrow = max([numel(LonPoly1), numel(LonFar)]);
    LonPoly2 = nan(Nrow, 2);
    LatPoly2 = nan(Nrow, 2);

    LonPoly2(1:numel(LonPoly1),1) = LonPoly1;
    LatPoly2(1:numel(LatPoly1),1) = LatPoly1;

    LonPoly2(1:numel(LonFar),2) = LonFar;
    LatPoly2(1:numel(LatFar),2) = LatFar;

    PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1 0])), 'Test 5 failed');


    %% Test 6: NaN padding in Poly2
    [LonPoly1, LatPoly1] = localPlaneTriToSphere([0.00 0.08 0.02], [0.00 0.01 0.09], CenterLon, CenterLat);

    LonPoly2 = nan(6, 1);
    LatPoly2 = nan(6, 1);
    LonPoly2(1:3,1) = LonPoly1;
    LatPoly2(1:3,1) = LatPoly1;

    PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1])), 'Test 6 failed');


    %% Test 7: Degenerate Poly2 column ignored
    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.10, 0.10, CenterLon, CenterLat);

    LonPoly2 = nan(4, 2);
    LatPoly2 = nan(4, 2);

    LonPoly2(:,1) = LonPoly1;
    LatPoly2(:,1) = LatPoly1;

    LonPoly2(1:2,2) = [0.1; 0.2];
    LatPoly2(1:2,2) = [0.1; 0.2];

    PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1 0])), 'Test 7 failed');


    Result = true;

end


%% Local functions

function [Lon, Lat] = localPlaneRectToSphere(Xmin, Xmax, Ymin, Ymax, Lon0, Lat0)
    X = [Xmin; Xmax; Xmax; Xmin];
    Y = [Ymin; Ymin; Ymax; Ymax];
    [Lon, Lat] = localInvGnomonic(X, Y, Lon0, Lat0);
end

function [Lon, Lat] = localPlaneTriToSphere(X, Y, Lon0, Lat0)
    [Lon, Lat] = localInvGnomonic(X(:), Y(:), Lon0, Lat0);
end

function [Lon, Lat] = localInvGnomonic(X, Y, Lon0, Lat0)
    % Inverse gnomonic projection from tangent plane centered at Lon0,Lat0
    %
    % Plane coordinates X,Y map to points on the unit sphere such that
    % straight lines in the plane correspond to great circles on the sphere.

    X = X(:);
    Y = Y(:);

    Clat0 = cos(Lat0);
    X0 = Clat0 .* cos(Lon0);
    Y0 = Clat0 .* sin(Lon0);
    Z0 = sin(Lat0);
    Center = [X0, Y0, Z0];

    Ref = [0 0 1];
    if abs(dot(Center, Ref)) > 0.95
        Ref = [1 0 0];
    end

    E1 = cross(Ref, Center);
    E1 = E1 ./ norm(E1);
    E2 = cross(Center, E1);
    E2 = E2 ./ norm(E2);

    N = numel(X);
    Xyz = nan(N, 3);

    for I = 1:N
        V = Center + X(I).*E1 + Y(I).*E2;
        V = V ./ norm(V);
        Xyz(I,:) = V;
    end

    Lon = atan2(Xyz(:,2), Xyz(:,1));
    Lat = asin(Xyz(:,3));
end