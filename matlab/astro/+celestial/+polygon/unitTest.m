function [Result] = unitTest()
    % unitTest for celestial.polygon
    % Example: celestial.polygon.unitTest
   
    %%
    Result=test_whichPolygonsTilesPoly();


    %% celestial.polygon.areaPolyIntersection
    unitTest_areaPolyIntersection()

    %% celestial.polygon.areaPolyIntersection

    RefLon = [350; 10; 10; 350];
    RefLat = [-10; -10; 10; 10];
    Lon    = [355; 15; 15; 355];
    Lat    = [-5; -5; 15; 15];
    [Area, AreaRefPoly] = celestial.polygon.areaPolyIntersection(RefLon, RefLat, Lon, Lat, 'CooUnits','deg', 'UseMex',true);
    [Area1, AreaRefPoly1] = celestial.polygon.areaPolyIntersection(RefLon, RefLat, Lon, Lat, 'CooUnits','deg', 'UseMex',false);

    if max(abs(Area-Area1))>1e-11 || max(abs(AreaRefPoly-AreaRefPoly1))>1e-11
        error('Problem wih celestial.polygon.areaPolyIntersection');
    end
    %%

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

    PolyFlag = celestial.polygon.whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
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

    PolyFlag = celestial.polygon.whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
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

    PolyFlag = celestial.polygon.whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
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

    PolyFlag = celestial.polygon.whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
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

    PolyFlag = celestial.polygon.whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1 0])), 'Test 5 failed');


    %% Test 6: NaN padding in Poly2
    [LonPoly1, LatPoly1] = localPlaneTriToSphere([0.00 0.08 0.02], [0.00 0.01 0.09], CenterLon, CenterLat);

    LonPoly2 = nan(6, 1);
    LatPoly2 = nan(6, 1);
    LonPoly2(1:3,1) = LonPoly1;
    LatPoly2(1:3,1) = LatPoly1;

    PolyFlag = celestial.polygon.whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
    assert(isequal(PolyFlag(:).', logical([1])), 'Test 6 failed');


    %% Test 7: Degenerate Poly2 column ignored
    [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.10, 0.10, CenterLon, CenterLat);

    LonPoly2 = nan(4, 2);
    LatPoly2 = nan(4, 2);

    LonPoly2(:,1) = LonPoly1;
    LatPoly2(:,1) = LatPoly1;

    LonPoly2(1:2,2) = [0.1; 0.2];
    LatPoly2(1:2,2) = [0.1; 0.2];

    PolyFlag = celestial.polygon.whichPolygonsTilesPoly(LonPoly1, LatPoly1, LonPoly2, LatPoly2);
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




function unitTest_areaPolyIntersection
%% unitTest_areaPolyIntersection.m
% Unit tests for celestial.polygon.areaPolyIntersection.
%
% The tests compare the calculated intersection areas against analytical
% spherical-polygon cases. The main analytical shape is a spherical
% triangle bounded by the equator and two meridians.
%
% For a triangle with vertices:
%   [Lon1, 0], [Lon2, 0], [arbitrary longitude, 90 deg],
%
% the area on the unit sphere is equal to the angular separation between
% the meridians:
%
%   Area = abs(Lon2 - Lon1) [steradians],
%
% provided that the minor longitudinal separation is used.
%
% Author : Eran Ofek (2026 Jul)

clear;
clc;

fprintf('Testing celestial.polygon.areaPolyIntersection...\n\n');

TolAbs = 5e-11;
Ntest  = 0;


%% Test 1: Identical spherical triangles
% Triangle bounded by longitude 0 deg, longitude 60 deg, and the equator.
% Exact area is 60 deg in radians, i.e., pi/3 steradians.

RefLon = deg2rad([0; 60; 0]);
RefLat = deg2rad([0;  0; 90]);

PolyLon = RefLon;
PolyLat = RefLat;

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = pi./3;
ExpectedAreaRef = pi./3;

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 1: identical polygons, intersection area');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 1: identical polygons, reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: identical spherical triangles.\n',Ntest);


%% Test 2: Partial overlap
% Reference triangle covers longitude [0,60] deg.
% Second triangle covers longitude [30,90] deg.
% Intersection covers longitude [30,60] deg.
% Exact overlap area is 30 deg = pi/6 steradians.

RefLon = deg2rad([0; 60; 0]);
RefLat = deg2rad([0;  0; 90]);

PolyLon = deg2rad([30; 90; 30]);
PolyLat = deg2rad([ 0;  0; 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = pi./6;
ExpectedAreaRef = pi./3;

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 2: partial overlap');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 2: reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: partial overlap.\n',Ntest);


%% Test 3: Polygon fully contained in reference polygon
% Reference covers longitude [0,60] deg.
% Test polygon covers longitude [10,20] deg.
% The test polygon is fully contained in the reference polygon.
% Exact intersection area is 10 deg.

RefLon = deg2rad([0; 60; 0]);
RefLat = deg2rad([0;  0; 90]);

PolyLon = deg2rad([10; 20; 10]);
PolyLat = deg2rad([ 0;  0; 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(10);
ExpectedAreaRef = deg2rad(60);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 3: contained polygon');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 3: reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: contained polygon.\n',Ntest);


%% Test 4: Reference polygon fully contained in test polygon
% Reference covers longitude [20,40] deg.
% Test polygon covers longitude [0,60] deg.
% The intersection is exactly the reference polygon.

RefLon = deg2rad([20; 40; 20]);
RefLat = deg2rad([ 0;  0; 90]);

PolyLon = deg2rad([0; 60; 0]);
PolyLat = deg2rad([0;  0; 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(20);
ExpectedAreaRef = deg2rad(20);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 4: reference polygon contained in test polygon');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 4: reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: reference polygon contained in polygon.\n',Ntest);


%% Test 5: Disjoint polygons
% Reference covers longitude [0,30] deg.
% Test polygon covers longitude [60,90] deg.
% The polygons have zero overlap area.

RefLon = deg2rad([0; 30; 0]);
RefLat = deg2rad([0;  0; 90]);

PolyLon = deg2rad([60; 90; 60]);
PolyLat = deg2rad([ 0;  0; 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = 0;
ExpectedAreaRef = deg2rad(30);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 5: disjoint polygons');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 5: reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: disjoint polygons.\n',Ntest);


%% Test 6: Touching along one boundary
% Reference covers longitude [0,30] deg.
% Test polygon covers longitude [30,60] deg.
% The polygons share one meridian but their overlap area is zero.

RefLon = deg2rad([0; 30; 0]);
RefLat = deg2rad([0;  0; 90]);

PolyLon = deg2rad([30; 60; 30]);
PolyLat = deg2rad([ 0;  0; 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = 0;
ExpectedAreaRef = deg2rad(30);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 6: polygons touching along an edge');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 6: reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: edge-only contact.\n',Ntest);


%% Test 7: Longitude-zero crossing
% Reference covers longitude [350,10] deg across longitude zero.
% Its angular width is 20 deg.
%
% Test polygon covers longitude [355,5] deg.
% It is fully contained in the reference polygon and has width 10 deg.

RefLon = deg2rad([350; 10; 350]);
RefLat = deg2rad([  0;  0;  90]);

PolyLon = deg2rad([355; 5; 355]);
PolyLat = deg2rad([  0; 0;  90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(10);
ExpectedAreaRef = deg2rad(20);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 7: longitude-zero-crossing overlap');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 7: longitude-zero-crossing reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: longitude-zero crossing.\n',Ntest);


%% Test 8: Partial overlap across longitude zero
% Reference covers longitude [350,10] deg.
% Test polygon covers longitude [0,20] deg.
% Their overlap covers longitude [0,10] deg.

RefLon = deg2rad([350; 10; 350]);
RefLat = deg2rad([  0;  0;  90]);

PolyLon = deg2rad([0; 20; 0]);
PolyLat = deg2rad([0;  0; 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(10);
ExpectedAreaRef = deg2rad(20);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 8: partial overlap across longitude zero');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 8: reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: partial zero-crossing overlap.\n',Ntest);


%% Test 9: Several polygons supplied simultaneously as matrix columns
% All polygons have three vertices, with one polygon per matrix column.
%
% Column 1: identical to reference, expected area 60 deg.
% Column 2: partial overlap, expected area 30 deg.
% Column 3: contained polygon, expected area 10 deg.
% Column 4: disjoint polygon, expected area 0.

RefLon = deg2rad([0; 60; 0]);
RefLat = deg2rad([0;  0; 90]);

PolysLon = deg2rad([ ...
     0, 30, 10,  90; ...
    60, 90, 20, 120; ...
     0, 30, 10,  90]);

PolysLat = deg2rad([ ...
     0,  0,  0,  0; ...
     0,  0,  0,  0; ...
    90, 90, 90, 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolysLon, PolysLat);

ExpectedArea    = deg2rad([60,30,10,0]);
ExpectedAreaRef = deg2rad(60);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 9: simultaneous matrix polygons');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 9: matrix-input reference area');

assert(isequal(size(Area),[1,4]), ...
    'Test 9 failed: Area must be a 1-by-Npoly row vector.');

Ntest = Ntest + 1;
fprintf('Test %2d passed: simultaneous matrix input.\n',Ntest);


%% Test 10: Row-vector polygon input
% A row vector must be interpreted as one polygon, not as three polygons.

RefLon = deg2rad([0; 60; 0]);
RefLat = deg2rad([0;  0; 90]);

PolyLon = deg2rad([30, 90, 30]);
PolyLat = deg2rad([ 0,  0, 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(30);
ExpectedAreaRef = deg2rad(60);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 10: row-vector polygon');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 10: row-vector reference area');

assert(isscalar(Area), ...
    'Test 10 failed: row-vector input must represent one polygon.');

Ntest = Ntest + 1;
fprintf('Test %2d passed: row-vector polygon input.\n',Ntest);


%% Test 11: Reverse polygon vertex orientation
% Reversing clockwise/counterclockwise order must not change the result.

RefLon = deg2rad([0; 60; 0]);
RefLat = deg2rad([0;  0; 90]);

PolyLonForward = deg2rad([30; 90; 30]);
PolyLatForward = deg2rad([ 0;  0; 90]);

PolyLonReverse = flipud(PolyLonForward);
PolyLatReverse = flipud(PolyLatForward);

AreaForward = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLonForward, PolyLatForward);

AreaReverse = celestial.polygon.areaPolyIntersection( ...
    flipud(RefLon), flipud(RefLat), ...
    PolyLonReverse, PolyLatReverse);

ExpectedArea = deg2rad(30);

assertClose(AreaForward,ExpectedArea,TolAbs, ...
    'Test 11: forward vertex order');

assertClose(AreaReverse,AreaForward,TolAbs, ...
    'Test 11: reversed vertex order');

Ntest = Ntest + 1;
fprintf('Test %2d passed: reversed vertex ordering.\n',Ntest);


%% Test 12: Degree input and square-degree output
% In degree mode, the output area is in square degrees:
%
%   Area_deg2 = Area_sr .* (180/pi)^2.
%
% For a spherical triangle with longitudinal width DeltaLon:
%
%   Area_sr   = DeltaLon .* pi/180
%   Area_deg2 = DeltaLon .* 180/pi.

RefLon = [0; 60; 0];
RefLat = [0;  0; 90];

PolyLon = [30; 90; 30];
PolyLat = [ 0;  0; 90];

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat, ...
    'CooUnits','deg');

ExpectedArea    = 30.*180./pi;
ExpectedAreaRef = 60.*180./pi;

assertClose(Area,ExpectedArea,1e-8, ...
    'Test 12: square-degree overlap area');

assertClose(AreaRef,ExpectedAreaRef,1e-8, ...
    'Test 12: square-degree reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: degree input and square-degree output.\n',Ntest);


%% Test 13: NaN-padded matrix columns
% Different polygons may contain different numbers of vertices when shorter
% polygon columns are padded with NaNs.
%
% Column 1 is identical to the reference polygon.
% Column 2 partially overlaps the reference polygon and repeats its first
% vertex as a closing vertex.

RefLon = deg2rad([0; 60; 0]);
RefLat = deg2rad([0;  0; 90]);

PolysLon = deg2rad([ ...
     0,  30; ...
    60,  90; ...
     0,  30; ...
   NaN,  30; ...
   NaN, NaN]);

PolysLat = deg2rad([ ...
     0,   0; ...
     0,   0; ...
    90,  90; ...
   NaN,   0; ...
   NaN, NaN]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolysLon, PolysLat);

ExpectedArea    = deg2rad([60,30]);
ExpectedAreaRef = deg2rad(60);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 13: NaN-padded polygon matrix');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 13: NaN-padded reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: NaN-padded matrix polygons.\n',Ntest);


%% Test 14: Repeated closing vertex
% Supplying the first vertex again as the final vertex must not change the
% polygon or its area.

RefLon = deg2rad([0; 60; 0; 0]);
RefLat = deg2rad([0;  0; 90; 0]);

PolyLon = deg2rad([30; 90; 30; 30]);
PolyLat = deg2rad([ 0;  0; 90;  0]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(30);
ExpectedAreaRef = deg2rad(60);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 14: repeated closing vertex overlap');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 14: repeated closing vertex reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: repeated closing vertices.\n',Ntest);


%% Test 15: Analytical spherical octant
% Vertices:
%   (0,0), (90,0), north pole.
%
% This is one octant of the sphere. The three internal angles are all pi/2,
% so its spherical excess and area are:
%
%   3*pi/2 - pi = pi/2 steradians.

RefLon = deg2rad([0; 90; 0]);
RefLat = deg2rad([0;  0; 90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, RefLon, RefLat);

ExpectedArea    = pi./2;
ExpectedAreaRef = pi./2;

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 15: spherical octant intersection');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 15: spherical octant reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: analytical spherical octant.\n',Ntest);


%% Test 16: North-pole inclusion
% Both polygons contain the north pole and cross the longitude origin.
%
% Reference covers longitude [330,30] deg, width 60 deg.
% Test polygon covers longitude [350,20] deg, width 30 deg.
% The test polygon is fully contained in the reference polygon.

RefLon = deg2rad([330; 30; 330]);
RefLat = deg2rad([  0;  0;  90]);

PolyLon = deg2rad([350; 20; 350]);
PolyLat = deg2rad([  0;  0;  90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(30);
ExpectedAreaRef = deg2rad(60);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 16: north-pole inclusion');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 16: north-pole reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: north-pole inclusion.\n',Ntest);


%% Test 17: South-pole inclusion
% The same construction as Test 16, but reflected into the southern
% hemisphere.

RefLon = deg2rad([330; 30; 330]);
RefLat = deg2rad([  0;  0; -90]);

PolyLon = deg2rad([350; 20; 350]);
PolyLat = deg2rad([  0;  0; -90]);

[Area, AreaRef] = celestial.polygon.areaPolyIntersection( ...
    RefLon, RefLat, PolyLon, PolyLat);

ExpectedArea    = deg2rad(30);
ExpectedAreaRef = deg2rad(60);

assertClose(Area,ExpectedArea,TolAbs, ...
    'Test 17: south-pole inclusion');

assertClose(AreaRef,ExpectedAreaRef,TolAbs, ...
    'Test 17: south-pole reference area');

Ntest = Ntest + 1;
fprintf('Test %2d passed: south-pole inclusion.\n',Ntest);


%% Summary

fprintf('\nAll %d tests passed successfully.\n',Ntest);

end

%% Local functions

function assertClose(Value,Expected,Tolerance,Description)
    % Assert that numeric values agree within an absolute tolerance.

    if ~isequal(size(Value),size(Expected))
        error('unitTest_areaPolyIntersection:SizeMismatch', ...
              ['%s failed.\n' ...
               'Expected size: %s\n' ...
               'Actual size:   %s'], ...
              Description,mat2str(size(Expected)),mat2str(size(Value)));
    end

    Difference    = abs(Value - Expected);
    MaxDifference = max(Difference(:));

    if isempty(MaxDifference)
        MaxDifference = 0;
    end

    if any(~isfinite(Value(:))) || MaxDifference > Tolerance
        error('unitTest_areaPolyIntersection:ValueMismatch', ...
              ['%s failed.\n' ...
               'Expected:      %s\n' ...
               'Actual:        %s\n' ...
               'Max abs error: %.16g\n' ...
               'Tolerance:     %.16g'], ...
              Description,mat2str(Expected,16),mat2str(Value,16), ...
              MaxDifference,Tolerance);
    end
end
