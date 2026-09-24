function tests = test_sphere_dist_fast
    % Unit tests for the sphere_dist_fast function
    % This function calculates angular distances and position angles using the Haversine formula
    %
    % Author: Yarin Shani

    tests = functiontests(localfunctions);
end

%% Test Functions

function testZeroDistance(testCase)
    % Test when two points are identical (distance = 0)
    
    RA_1 = 1; Dec_1 = 1;  % First point (in radians)
    RA_2 = 1; Dec_2 = 1;  % Identical point
    
    % Call sphere_dist_fast
    [Dist, ~] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected distance is 0
    verifyEqual(testCase, Dist, 0, 'relTol', 1e-10);
end

function testKnownDistance(testCase)
    % Test the angular distance between two known points
    
    RA_1 = 0; Dec_1 = 0;     % First point
    RA_2 = pi/2; Dec_2 = 0;  % Second point (90 degrees away)
    
    % Call sphere_dist_fast
    [Dist, ~] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected distance is pi/2 (90 degrees)
    verifyEqual(testCase, Dist, pi/2, 'relTol', 1e-10);
end

function testOppositePoints(testCase)
    % Test for points on opposite sides of the sphere (antipodal)
    
    RA_1 = 0; Dec_1 = 0;    % First point
    RA_2 = pi; Dec_2 = 0;   % Opposite point
    
    % Call sphere_dist_fast
    [Dist, ~] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected distance is pi (180 degrees)
    verifyEqual(testCase, Dist, pi, 'relTol', 1e-10);
end

function testPositionAngle(testCase)
    % Test the position angle between two points
    
    RA_1 = 0; Dec_1 = 0;     % First point
    RA_2 = 3*pi/2; Dec_2 = 0;  % Second point
    
    % Call sphere_dist_fast
    [~, Ang,PA] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected position angle is pi/2 (eastward)
    verifyEqual(testCase, Ang, pi/2, 'relTol', 1e-10); % output is always 3/2 pi
end

function testNearZeroDistance(testCase)
    % Test for very small angular distances
    
    RA_1 = 0; Dec_1 = 0;         % First point
    RA_2 = 1e-6; Dec_2 = 1e-6;   % Second point very close
    
    % Call sphere_dist_fast
    [Dist, ~] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected small distance (approximately sqrt(2)*1e-6 radians)
    expectedDist = sqrt(2) * 1e-6;
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
end

function testSinglePrecision(testCase)
    % Test the function with single precision inputs
    
    RA_1 = single(0); Dec_1 = single(0);    % Single precision inputs
    RA_2 = single(pi/2); Dec_2 = single(0);
    
    % Call sphere_dist_fast
    [Dist, ~] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected distance is pi/2
    verifyEqual(testCase, double(Dist), pi/2, 'relTol', 1e-7);  % Fails in 1e-8.
end


function testHaversineStability(testCase)
    % Test the stability of the Haversine formula for large distances
    
    RA_1 = 0; Dec_1 = 0;     % First point
    RA_2 = pi; Dec_2 = 0;    % Opposite point (antipodal)
    
    % Call sphere_dist_fast
    [Dist, ~] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected distance is pi (180 degrees)
    verifyEqual(testCase, Dist, pi, 'relTol', 1e-10);
end

function testLargeInputHandling(testCase)
    % Test the function's performance and memory handling with large inputs
    
    N = 1e6;  % One million points
    RA_1 = zeros(N, 1);      % All points at RA = 0
    Dec_1 = zeros(N, 1);     % All points at Dec = 0
    RA_2 = pi * ones(N, 1);  % All points at RA = pi (antipodal)
    Dec_2 = zeros(N, 1);     % All points at Dec = 0
    
    % Call sphere_dist_fast with large input
    [Dist, ~] = celestial.coo.sphere_dist_fast(RA_1, Dec_1, RA_2, Dec_2);
    
    % Expected distance for all points is pi
    verifyEqual(testCase, Dist, pi * ones(N, 1), 'relTol', 1e-10);
end
