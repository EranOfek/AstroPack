function tests = test_area_sphere_polygon
    % Unit tests for the area_sphere_polygon function, which calculates the area of a polygon on a sphere.
    %
    % Author: Yarin Shani
    % 

    tests = functiontests(localfunctions);
end

%% Test Functions

function testTriangleArea(testCase)
    % Test if the function correctly calculates the area of a simple spherical triangle.
    PolyLon = [0; pi/2; pi/4];  % Longitude values in radians
    PolyLat = [0; 0; pi/2];     % Latitude values in radians
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is within a reasonable expected range
    expectedArea = pi / 2;  % Expected approximate area for the triangle
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to calculate correct area for spherical triangle.');
end

function testClosedPolygon(testCase)
    % Test if the function handles closed polygons properly.
    PolyLon = [0; pi/3; pi/3; 0; 0];  % Longitude values in radians (closed polygon)
    PolyLat = [0; 0; pi/3; pi/3; 0];  % Latitude values in radians (closed polygon)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is within the expected range
    verifyGreaterThanOrEqual(testCase, Area, 0, 'Failed to calculate non-negative area for closed polygon.');
end

function testOpenPolygon(testCase)
    % Test if the function handles an open polygon by automatically closing it.
    PolyLon = [0; pi/3; pi/3; 0];  % Longitude values in radians (not closed)
    PolyLat = [0; 0; pi/3; pi/3];  % Latitude values in radians (not closed)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is within the expected range
    verifyGreaterThanOrEqual(testCase, Area, 0, 'Failed to calculate non-negative area for open polygon.');
end




function testBoxPolygon(testCase)
    % Test if the function handles closed polygons properly.
    RAD = pi/180;
    PolyLon = [-7; -7; 7; 7].*RAD;  % Longitude values in radians (closed polygon)
    PolyLat = [-7; 7; 7; -7].*RAD;  % Latitude values in radians (closed polygon)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result matches the expected area of the closed polygon
    expectedArea = 0.5 * (pi / 6) * pi;  % Example expected area for the shape
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for closed polygon.');
end

function testPolygonCrossingNorthPole(testCase)
    % Test if the function correctly calculates the area of a polygon crossing the North Pole.
    PolyLon = [0; pi/2; pi; 3*pi/2; 0];  % Longitude values in radians
    PolyLat = [pi/2; pi/3; 0; -pi/3; pi/2];  % Latitude values in radians (crossing the North Pole)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is approximately half the sphere
    expectedArea = 2 * pi;  % Half the sphere's area
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for a polygon crossing the North Pole.');
end

function testPolygonCrossingSouthPole(testCase)
    % Test if the function correctly calculates the area of a polygon crossing the South Pole.
    PolyLon = [0; pi/2; pi; 3*pi/2; 0];  % Longitude values in radians
    PolyLat = [-pi/2; -pi/3; 0; pi/3; -pi/2];  % Latitude values in radians (crossing the South Pole)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is approximately half the sphere
    expectedArea = 2 * pi;  % Half the sphere's area
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for a polygon crossing the South Pole.');
end

function testConcavePolygon(testCase)
    % Test if the function correctly calculates the area of a known concave polygon.
    PolyLon = [0; pi/3; pi/2; pi/3; 0];  % Longitude values in radians
    PolyLat = [0; pi/6; 0; -pi/6; 0];  % Latitude values in radians (concave shape)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result matches the expected area for the concave polygon
    expectedArea = 0.1 * 4 * pi;  % Example expected area for concave shape
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for a concave polygon.');
end

function testConvexPolygon(testCase)
    % Test if the function correctly calculates the area of a convex polygon.
    PolyLon = [0; pi/4; pi/2; 3*pi/4; pi];  % Longitude values in radians
    PolyLat = [0; pi/6; pi/6; 0; -pi/6];  % Latitude values in radians (convex shape)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result matches the expected area for the convex polygon
    expectedArea = 0.25 * 4 * pi;  % Example expected area for convex shape
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for a convex polygon.');
end

function testLargePolygonCoveringThreeQuartersSphere(testCase)
    % Test if the function correctly calculates the area of a polygon covering 0.75 of the sphere.
    PolyLon = [0; 2*pi/3; 4*pi/3; 2*pi];  % Longitude values in radians
    PolyLat = [pi/2; 0; 0; pi/2];  % Latitude values in radians
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is approximately 0.75 * 4 * pi
    expectedArea = 0.75 * 4 * pi;
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for a large polygon covering 0.75 of the sphere.');
end

function testVerySmallPolygon(testCase)
    % Test if the function correctly calculates the area for a very small polygon.
    PolyLon = [0; 1e-6; 2e-6; 0];  % Longitude values in radians (very small polygon)
    PolyLat = [0; 1e-6; 0; -1e-6];  % Latitude values in radians (very small polygon)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is approximately zero
    expectedArea = 0;  % Area should be close to zero for very small polygons
    verifyEqual(testCase, Area, expectedArea, 'AbsTol', 1e-9, 'Failed to correctly calculate area for a very small polygon.');
end

function testCollinearPointsPolygon(testCase)
    % Test if the function handles a degenerate polygon with collinear points (area should be zero).
    PolyLon = [0; pi/4; pi/2;pi/4];  % Longitude values in radians (collinear points)
    PolyLat = [0; 0; 0;0];  % Latitude values in radians (collinear points)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is zero
    expectedArea = 0;  % Area should be zero for collinear points
    verifyEqual(testCase, Area, expectedArea, 'AbsTol', 1e-9, 'Failed to return zero area for collinear points polygon.');
end

function testCircularRegionArea(testCase)
    % Test if the function correctly calculates the area of circular regions on the sphere.
    radiusDeg = 10;  % Radius in degrees
    radiusRad = deg2rad(radiusDeg);  % Convert to radians
    numTests = 10;
    for i = 1:numTests
        centerLon = rand * 2 * pi;
        centerLat = rand * pi - pi/2;
        
        % Calculate the expected area of the circular region
        expectedArea = 2 * pi * (1 - cos(radiusRad));
        
        % Generate polygon points to approximate the circular region
        numPoints = 8;
        angles = linspace(0, 2 * pi, numPoints)';
        PolyLon = centerLon + radiusRad * cos(angles);
        PolyLat = centerLat + radiusRad * sin(angles);
        
        % Call area_sphere_polygon function
        Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
        
        % Verify the calculated area is close to the expected area
        verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for circular region on the sphere.');
    end
end

function testLunePolygon(testCase)
    % Test if the function handles an open polygon by automatically closing it.
    PolyLon = [ pi/8; pi/4;pi/8;0];  % Longitude values in radians (not closed)
    PolyLat = [ pi/2; 0;-pi/2;0];  % Latitude values in radians (not closed)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    expectedArea  = pi/2;
    
    % Verify the result is within the expected range
    verifyEqual(testCase, Area, expectedArea, 'Failed to calculate non-negative area for open polygon.');
end


function testHemispherePolygon(testCase)
    % Test if the function handles an open polygon by automatically closing it.
    PolyLon = [ 0; pi/4;pi/2;0.75*pi;pi;1.25*pi;1.5*pi;1.75*pi;2*pi];  % Longitude values in radians (not closed)
    PolyLat = [ 80; 80;80;80;80;80;80;80;80].*pi/180;  % Latitude values in radians (not closed)
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    expectedArea  = 2*pi;
    
    % Verify the result is within the expected range
    verifyEqual(testCase, Area, expectedArea, 'Failed to calculate non-negative area for open polygon.');
end

