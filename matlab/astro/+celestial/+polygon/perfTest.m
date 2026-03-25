function [Result] = perfTest(X, Y, Args)
    % perfTest for celestial.polygon
    % Example: celestila.polygon.perfTest

    %% celestial.polygon.whichPolygonsTilesPoly (and mex)
    CenterLon = 0.0;
    CenterLat = 0.0;

    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.10, 0.10, CenterLon, CenterLat);

    LonPoly2 = nan(numel(LonPoly1), 1);
    LatPoly2 = nan(numel(LatPoly1), 1);
    LonPoly2(:,1) = LonPoly1;
    LatPoly2(:,1) = LatPoly1;

    %LonPoly2 = LonPoly2+(rand(1,6)-0.5).*0.03;
    %LatPoly2 = LatPoly2+(rand(1,6)-0.5).*0.03;

    LonPoly2 = LonPoly2 + [0.05, 0.05, -0.05, -0.05, 1, 1, 0.01];
    LatPoly2 = LatPoly2 + [-0.05 0.05 -0.05 0.05, 1 1, 0.01];

    tic; for i=1:1e2, PolyFlag = whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2); end, T1=toc;
    tic; for i=1:1e2, PolyFlag1 = whichPolygonsTilesPoly_mex(LonPoly1, LatPoly1, LonPoly2, LatPoly2); end, T2=toc;

    fprintf('celestial.polygon.whichPolygonsTilesPoly mex version is x %f faster than non mex\n',T1./T2);

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
