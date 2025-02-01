function tests = test_area_sphere_polygon
    % Unit tests for the area_sphere_polygon function, which calculates the area of a polygon on a sphere.
    %
    % This Test is incomplete due to an unclear result of the function.
    % in testTriangle Area : permutation to the tirangle vertices is not
    % consistent.
    % Author: Yarin Shani
    % Note : Lots of tests that should pass are commented out. CHECK THE
    % FUNCTION if in use.

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
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 1e-6, 'Failed to calculate correct area for spherical triangle.');
end

function testTriangleAreaPermutation(testCase)
    % Test if the function correctly calculates the area of a simple
    % spherical triangle. With a permutation in the order of vertices.
    PolyLon = [ pi/4;0; pi/2];  % Longitude values in radians
    PolyLat = [pi/2; 0; 0];     % Latitude values in radians
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is within a reasonable expected range
    expectedArea = pi / 2;  % Expected approximate area for the triangle
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 1e-6, 'Failed to calculate correct area for spherical triangle.');
end

function testTriangleAreaPermutation2(testCase)
    % Test if the function correctly calculates the area of a simple
    % spherical triangle. With a permutation in the order of vertices.
    PolyLon = [ pi/2; pi/4;0];  % Longitude values in radians
    PolyLat = [0; pi/2; 0];     % Latitude values in radians
    
    % Call area_sphere_polygon function
    Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
    
    % Verify the result is within a reasonable expected range
    expectedArea = pi / 2;  % Expected approximate area for the triangle
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 1e-6, 'Failed to calculate correct area for spherical triangle.');
end

function testTriangleAreaPermutationComparison(testCase)
    % Test if the function correctly calculates the area of a simple
    % spherical triangle compared to the same triangle with different verticies ordering. 

    
    PolyLon = [ 0; pi/4;pi/2];  % Longitude values in radians
    PolyLat = [0; pi/2; 0];     % Latitude values in radians

    % Permutation between the 2nd and 3rd vertices.
    PolyLon0 = [ pi/2; 0;pi/4];  % Longitude values in radians
    PolyLat0 = [ 0;0; pi/2];     % Latitude values in radians
    
    % Call area_sphere_polygon function
    Area  = celestial.coo.area_sphere_polygon(PolyLon, PolyLat); % Negative SumAngle results in wrong answer

    Area0 = celestial.coo.area_sphere_polygon(PolyLon0, PolyLat0);

    
    % Verify the result is within a reasonable expected range

    verifyEqual(testCase, Area, Area0, 'RelTol', 1e-6, 'Failed to calculate correct area for the same spherical triangles.');
end

function testTriangleBaseEqualLatitudeComparison(testCase)
    % Test if the function correctly calculates the area of a simple
    % spherical triangle compared to the same triangle with different verticies ordering. 

    
    PolyLon = [ 0; pi/6;pi/3];  % Longitude values in radians
    PolyLat = [pi/3; pi/2; pi/3];     % Latitude values in radians

    % Permutation between the 2nd and 3rd vertices.
    PolyLon0 = [pi/6; pi/3; 0];  % Longitude values in radians
    PolyLat0 = [pi/2; pi/3; pi/3];     % Latitude values in radians
    
    % Call area_sphere_polygon function
    Area  = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);

    Area0 = celestial.coo.area_sphere_polygon(PolyLon0, PolyLat0);

    
    % Verify the result is within a reasonable expected range
    % ###### DOESNT MAKE SENSE - CHECK FUNCTION #######
    verifyEqual(testCase, Area, Area0, 'RelTol', 1e-6, 'Failed to calculate correct area for the same spherical triangles.');
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



% 
% function testConcavePolygon(testCase)
%     % Test if the function correctly calculates the area of a known concave polygon.
%     PolyLon = [0; pi/3; pi/2; -pi/3; 0];  % Longitude values in radians
%     PolyLat = [0; pi/6; 0; -pi/6; 0];  % Latitude values in radians (concave shape)
% 
%     % Call area_sphere_polygon function
%     Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
% 
%     % Verify the result matches the expected area for the concave polygon
%     expectedArea = 0.1 * 4 * pi;  % Example expected area for concave shape
%     verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for a concave polygon.');
% end
% 
% function testConvexPolygon(testCase)
%     % Test if the function correctly calculates the area of a convex polygon.
%     PolyLon = [0; pi/4; pi/2; 3*pi/4; pi];  % Longitude values in radians
%     PolyLat = [0; pi/6; pi/6; 0; -pi/6];  % Latitude values in radians (convex shape)
% 
%     % Call area_sphere_polygon function
%     Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
% 
%     % Verify the result matches the expected area for the convex polygon
%     expectedArea = 0.25 * 4 * pi;  % Example expected area for convex shape
%     verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for a convex polygon.');
% end


% function testVerySmallPolygon(testCase)
%     % Test if the function correctly calculates the area for a very small polygon.
%     PolyLon = [0; 1e-6; 2e-6; 0];  % Longitude values in radians (very small polygon)
%     PolyLat = [0; 1e-6; 0; -1e-6];  % Latitude values in radians (very small polygon)
% 
%     % Call area_sphere_polygon function
%     Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
% 
%     % Verify the result is approximately zero
%     expectedArea = 0;  % Area should be close to zero for very small polygons
%     verifyEqual(testCase, Area, expectedArea, 'AbsTol', 1e-9, 'Failed to correctly calculate area for a very small polygon.');
% end


% function testCircularRegionArea(testCase)
%     % Test if the function correctly calculates the area of circular regions on the sphere.
%     radiusDeg = 1;  % Radius in degrees
%     radiusRad = deg2rad(radiusDeg);  % Convert to radians
%     numTests = 10;
%     for i = 1:numTests
%         centerLon = rand * 2 * pi;
%         centerLat = rand * pi - pi/2;
% 
%         % Calculate the expected area of the circular region
%         expectedArea = 2 * pi * (1 - cos(radiusRad));
% 
%         % Generate polygon points to approximate the circular region
%         numPoints = 800;
%         angles = linspace(0, 2 * pi, numPoints)';
%         PolyLon = centerLon + radiusRad * cos(angles);
%         PolyLat = centerLat + radiusRad * sin(angles);
% 
%         % Call area_sphere_polygon function
%         Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
% 
%         % Verify the calculated area is close to the expected area
%         verifyEqual(testCase, Area, expectedArea, 'RelTol', 0.1, 'Failed to correctly calculate area for circular region on the sphere.');
%     end
% end
% 
% function testLunePolygon(testCase)
%     % Test if the function handles an open polygon by automatically closing it.
%     PolyLon = [ pi/8; pi/4;pi/8;0];  % Longitude values in radians (not closed)
%     PolyLat = [ pi/2; 0;-pi/2;0];  % Latitude values in radians (not closed)
% 
%     % Call area_sphere_polygon function
%     Area = celestial.coo.area_sphere_polygon(PolyLon, PolyLat);
%     expectedArea  = pi/2;
% 
%     % Verify the result is within the expected range
%     verifyEqual(testCase, Area, expectedArea, 'Failed to calculate non-negative area for open polygon.');
% end




