function tests = test_sphere_dist
    % Unit tests for the sphere_dist function in celestial.coo
    % 
    
    % 
    % The function calculates angular distances and position angles
    % between two points on the celestial sphere, handling different
    % input formats (radians, degrees, sexagesimal).
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testZeroDistance(testCase)
    % Test when the two points are the same (distance = 0)
    
    RA_1 = 1; Dec_1 = 1;  % Any arbitrary RA and Dec (radians)
    RA_2 = 1; Dec_2 = 1;  % Same as the first point
    
    % Call sphere_dist
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected distance is 0
    verifyEqual(testCase, Dist, 0, 'relTol', 1e-10);
end

function testKnownDistance(testCase)
    % Test for a known distance between two points
    
    % Define two points with known angular separation
    RA_1 = 0; Dec_1 = 0;  % First point at (0, 0)
    RA_2 = pi/2; Dec_2 = 0;  % Second point at (90 degrees, 0)
    
    % Call sphere_dist
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected distance is pi/2 radians (90 degrees)
    verifyEqual(testCase, Dist, pi/2, 'relTol', 1e-10);
end

function testPositionAngle(testCase)
    % Test the position angle between two points
    
    % Define two points with known position angle
    RA_1 = 0; Dec_1 = 0;  % First point at (0, 0)
    RA_2 = pi/2; Dec_2 = 0;  % Second point at (90 degrees, 0)
    
    % Call sphere_dist
    [~, PA] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected position angle is pi/2 radians (90 degrees east)
    verifyEqual(testCase, PA, pi/2, 'AbsTol', 1e-10);
end

function testDegreeInput(testCase)
    % Test input in degrees
    
    % Define two points in degrees
    RA_1 = 0; Dec_1 = 0;  % First point at (0 deg, 0 deg)
    RA_2 = 90; Dec_2 = 0; % Second point at (90 deg, 0 deg)
    
    % Call sphere_dist with degree input
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'deg');
    
    % Expected distance is pi/2 radians (90 degrees)
    verifyEqual(testCase, Dist, pi/2, 'relTol', 1e-10);
end

function testSexagesimalInput(testCase)
    % Test input in sexagesimal format
    
    % Define two points in sexagesimal format
    RA_1 = '00:00:00'; Dec_1 = '+00:00:00';  % First point (0 hours, 0 deg)
    RA_2 = '06:00:00'; Dec_2 = '+00:00:00';  % Second point (6 hours, 0 deg)
    
    % Call sphere_dist with sexagesimal input
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'g');
    
    % Expected distance is pi/2 radians (90 degrees)
    verifyEqual(testCase, Dist, pi/2, 'AbsTol', 1e-10);
end


function testPolePosition(testCase)
    % Test distance from the North pole
    
    RA_1 = 0; Dec_1 = pi/2;  % North pole
    RA_2 = 0; Dec_2 = 0;     % Equator
    
    % Call sphere_dist
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected distance is pi/2 radians (90 degrees)
    verifyEqual(testCase, Dist, pi/2, 'relTol', 1e-10);
end

function testAntipodalPoints(testCase)
    % Test for antipodal points (points on opposite sides of the sphere)
    
    RA_1 = 0; Dec_1 = 0;  % First point
    RA_2 = pi; Dec_2 = 0;  % Antipodal point (opposite side of the sphere)
    
    % Call sphere_dist
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected distance is pi radians (180 degrees)
    verifyEqual(testCase, Dist, pi, 'relTol', 1e-10);
end

function testVeryClosePoints(testCase)
    % Test for very close points (small angular separation)
    
    % Define two points with a small angular separation
    RA_1 = 0; Dec_1 = 0;  % First point
    RA_2 = 1e-6; Dec_2 = 1e-6;  % Second point very close to the first
    
    % Call sphere_dist
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected distance is approximately sqrt(2) * 1e-6 radians
    expectedDist = sqrt(2) * 1e-6;
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-4); % Fails for 1e-5.
end

function testMultiplePointsComparison(testCase)
    % Test multiple points comparison with vector inputs
    
    % Define reference point
    RA_1 = 0; Dec_1 = 0;
    
    % Define multiple comparison points
    RA_2 = [pi/6; pi/3; pi/2];  % Multiple RA values
    Dec_2 = [0; 0; 0];          % All at 0 Dec (equator)
    
    % Call sphere_dist with vector inputs
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected distances (for points at 30°, 60°, and 90°)
    expectedDist = [pi/6; pi/3; pi/2];
    
    % Verify distances
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
end

function testSameLongitudeDifferentLatitude(testCase)
    % Test two points with the same longitude but different latitudes
    
    RA_1 = 0; Dec_1 = 0;     % First point at the equator
    RA_2 = 0; Dec_2 = pi/3;  % Second point 60° north of the equator
    
    % Call sphere_dist
    [Dist, ~] = celestial.coo.sphere_dist(RA_1, Dec_1, RA_2, Dec_2, 'rad');
    
    % Expected distance is the angular difference between the latitudes (pi/3)
    expectedDist = pi/3;
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
end

